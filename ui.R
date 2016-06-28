library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("united"),
    
    # Application title
    titlePanel("discreteRV Visualizer"),
    
    sidebarLayout(
        
        # Sidebar with a slider input
        sidebarPanel(
            h4("Random Variable"),
            
            selectizeInput("rv", "Distribution", c("Bernoulli" = "be", "Binomial" = "bn", "Discrete Uniform" = "du", "Geometric"= "gm", "Hypergeometric" = "hg", "Negative Binomial" = "nb", "Poisson"  = "pois")),
            conditionalPanel(
                condition = "input.rv == 'be'",
                sliderInput("beparam1", "Success Probability (p)", min = 0, max = 1, value = 0.5, step = 0.01)
            ),
            conditionalPanel(
                condition = "input.rv == 'bn'",
                sliderInput("bnparam1", "Size (n)", min = 1, max = 100, value = 10, step = 1),
                sliderInput("bnparam2", "Success Probability (p)", min = 0, max = 1, value = 0.5, step = 0.01)
            ),
            conditionalPanel(
                condition = "input.rv == 'du'",
                sliderInput("duparam1", "Lower (a)", min = -10, max = 10, value = 0, step = 1),
                sliderInput("duparam2", "Upper (b)", min = -10, max = 10, value = 1, step = 1)
            ),
            conditionalPanel(
                condition = "input.rv == 'gm'",
                sliderInput("gmparam1", "Success Probability (p)", min = 0.01, max = 1, value = 0.5, step = 0.01)
            ),
            conditionalPanel(
                condition = "input.rv == 'hg'",
                sliderInput("hgparam1", "Size (N)", min = 0, max = 100, value = 50, step = 1),
                sliderInput("hgparam2", "Successes (K)", min = 0, max = 100, value = 20, step = 1),
                sliderInput("hgparam3", "Draws (n)", min = 0, max = 100, value = 5, step = 1)
            ),
            conditionalPanel(
                condition = "input.rv == 'nb'",
                sliderInput("nbparam1", "Failures (r)", min = 1, max = 20, value = 5, step = 1),
                sliderInput("nbparam2", "Success Probability (p)", min = 0, max = 0.99, value = 0.5, step = 0.01)
            ),
            conditionalPanel(
                condition = "input.rv == 'pois'",
                sliderInput("poisparam", "Mean Parameter", min = 0.1, max = 10, value = 1, step = 0.1)
            ),
            
            hr(),
            
            h4("Transformations"),
            sliderInput("sofi", "Sum of IID Variables", min = 1, max = 30, step = 1, value = 1),
            
            conditionalPanel(
                condition = "input.tabs1 == 'Simulation'",
                
                hr(),
                
                h4("Simulation"),
                sliderInput("nsim", "Number of Trials", min = 1, max = 100, step = 1, value = 50),
                actionButton("sim", "Simulate")
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tabs1",
                tabPanel("Distribution",
                    tableOutput("dist_table"),
                    tableOutput("dist_moments"),
                    plotOutput("dist_plot"),
                    plotOutput("dist_norm")
                ),
                tabPanel("Simulation",
                    verbatimTextOutput("sim_text"),
                    tableOutput("sim_moments"),
                    plotOutput("sim_plot"),
                    plotOutput("sim_norm")
                )
            )
        )
    )
))
