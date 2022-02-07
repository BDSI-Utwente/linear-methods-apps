require(tidyverse)
require(shiny)
require(shinyjs)
require(plotly)

INITIAL_R = .5
NUM_OBSERVATIONS = 300
LIMITS = c(-4, 4)

# define function for sampling data with a given r ------------------------
# based on https://stats.stackexchange.com/questions/522454/how-to-construct-simulate-data-that-will-have-a-given-coefficient-of-determina/522470#522470
sample_linear_with_cor <- function(r, x, b0 = 0, b1 = 1) {
    r_square <- r * r
    n <- length(x)
    y_hat <- b0 + b1 * x

    ssr <- sum((y_hat - mean(y_hat)) ^ 2)
    e <- rnorm(n)
    e <- resid(lm(e ~ x))
    e <- e * sqrt((1 - r_square) / r_square * ssr / (sum(e ^ 2)))

    y <- y_hat + e
    y
}


# Define UI for application that draws a histogram
ui <- fillPage(useShinyjs(),
               fillCol(
                   plotlyOutput("corPlot", height = "100%"),
                   fillRow(
                       sliderInput(
                           "r",
                           "Correlation",
                           0,
                           1,
                           value = INITIAL_R,
                           0.01,
                           width = "100%"
                       ),

                       tags$div(
                           tags$label("Linear fit", class = "control-label"),
                           checkboxInput("linear", "Show linear fit?", FALSE, width = "100%"),
                           checkboxInput("se", "Show uncertainty?", FALSE, width = "100%"),
                       ),
                       style = "gap: 1em;"
                   )
               ), padding = "1em")

# Define server logic required to draw a histogram
server <- function(input, output) {
    x <- rnorm(NUM_OBSERVATIONS)
    y <- reactiveVal(sample_linear_with_cor(INITIAL_R, x))

    observe({
        y(sample_linear_with_cor(input$r, x))
    })

    observe({
        if(input$linear){
            enable("se")
        } else {
            disable("se")
            updateCheckboxInput(inputId = "se", value = FALSE)
        }
    })

    output$corPlot <- renderPlotly({
        plot <- ggplot(tibble(x, y = y()), aes(x, y)) +
            geom_point() +
            coord_cartesian(xlim = LIMITS, ylim = LIMITS)
        if (input$linear) {
            plot <- plot + geom_smooth(method = "lm", se = input$se)
        }
        plot %>% plotly::ggplotly()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
