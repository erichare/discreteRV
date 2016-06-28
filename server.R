library(shiny)
library(discreteRV)
library(MASS)
library(moments)

shinyServer(function(input, output, session) {
    
    observe({
        if (input$duparam2 <= input$duparam1) updateSliderInput(session, "duparam1", value = input$duparam2 - 1)
        
        if (input$hgparam2 > input$hgparam1) updateSliderInput(session, "hgparam2", value = input$hgparam1)
        if (input$hgparam3 > input$hgparam1) updateSliderInput(session, "hgparam4", value = input$hgparam1)
    })
    
    X <- reactive({
        my.rv <- switch(input$rv,
                        "be" = RV(c(0, 1), function(x, p = input$beparam1) p^x * (1 - p)^(1 - x), range = TRUE),     
                        "bn" = RV(c(0, input$bnparam1), function(x, n=input$bnparam1, p = input$bnparam2) choose(n, x) * p^x * (1 - p)^(n - x), range = TRUE),
                        "du" = RV(c(input$duparam1, input$duparam2), function(x, a=input$duparam1, b=input$duparam2){1 / (b - a + 1)}, range = TRUE),
                        "gm" = RV(c(1, Inf), function(x, p=input$gmparam1) (1 - p)^(x - 1) * p, range = TRUE),
                        "hg" = RV(c(max(0, input$hgparam3 + input$hgparam2 - input$hgparam1), min(input$hgparam2, input$hgparam3)), function(x, N = input$hgparam1, k = input$hgparam2, n = input$hgparam3) choose(k, x) * choose(N - k, n - x) / choose(N, n), range = TRUE),
                        "nb" = RV(c(0, Inf), function(x, r = input$nbparam1, p = input$nbparam2) choose(x + r - 1, x) * (1 - p)^r * p^x, range = TRUE),
                        "pois" = RV(c(0, Inf), function(x, lambda = input$poisparam) { lambda^x * exp(-lambda) / factorial(x) }, range = TRUE)
        )
        
        if (input$sofi > 1) my.rv <- SofIID(my.rv, input$sofi)
        
        return(my.rv)
    })

    output$dist_table <- renderTable({
        X.RV <- X()
        frac.prob <- as.character(fractions(probs(X.RV)))
        fractions <- all(nchar(frac.prob) < 6)
        
        if (fractions) pr <- as.character(fractions(probs(X.RV))) else pr <- round(probs(X.RV), digits = 4)
        
        return(t(data.frame(Outcome = as.character(X.RV), Probability = pr)))
    }, include.colnames = FALSE)
   
    output$dist_moments <- renderTable({
        X.RV <- X()
        
        frac.prob <- as.character(fractions(c(E(X.RV), V(X.RV), SKEW(X.RV), KURT(X.RV))))
        fractions <- all(nchar(frac.prob) < 6)
        
        if (fractions) moments <- as.character(fractions(c(E(X.RV), V(X.RV), SKEW(X.RV), KURT(X.RV)))) else moments <- round(c(E(X.RV), V(X.RV), SKEW(X.RV), KURT(X.RV)), digits = 4)
        
        return(t(data.frame(Moment = c("Mean", "Variance", "Skewness", "Kurtosis"), Value = moments)))
    }, include.colnames = FALSE)
    
    output$dist_plot <- renderPlot({
        return(plot(X(), ylim = c(0, 1)))
    })
    
    output$dist_norm <- renderPlot({
        return(qqnorm(X()))
    })
    
    X.sim <- reactive({
        input$sim
        
        return(rsim(X(), input$nsim))
    })
    
    output$sim_text <- renderPrint({
        return(print(X.sim()))
    })
    
    output$sim_moments <- renderTable({
        Xsim <- X.sim()
        
        frac.prob <- as.character(fractions(c(mean(Xsim), var(Xsim), skewness(Xsim), kurtosis(Xsim))))
        fractions <- all(nchar(frac.prob) < 6)
        
        if (fractions) moments <- as.character(fractions(c(mean(Xsim), var(Xsim), skewness(Xsim), kurtosis(Xsim)))) else moments <- round(c(mean(Xsim), var(Xsim), skewness(Xsim), kurtosis(Xsim)), digits = 4)
        
        return(t(data.frame("Empirical Moments" = c("Mean", "Variance", "Skewness", "Kurtosis"), Value = moments)))
    }, include.colnames = FALSE)
    
    output$sim_plot <- renderPlot({
        return(plot(X.sim()))
    })
    
    output$sim_norm <- renderPlot({
        return(qqnorm(X.sim()))
    })
  
})
