


library(shiny)
library(tidyverse)

source("../functions/helper_elements.R")

ui <- shiny::fillPage(shiny::fillCol(
    shiny::plotOutput("distPlot", width = "100%", height = "100%"),
    shiny::fillRow(
        tags$div(
            shiny::sliderInput("mean",
                               "Mean", -10,
                               10,
                               0,
                               1,
                               width = "100%"),
            style = "margin: 1em; height: min-content;"
        ),
        tags$div(
            shiny::sliderInput("sd",
                               "Standard deviation",
                               0.1,
                               10,
                               1,
                               .1,
                               width = "100%"),
            style = "margin: 1em; height: min-content;"
        )
    )
))

server <- function(input, output) {
    output$distPlot <- renderPlot({
        # sample data
        data <- tibble(x = seq(-10, 10, .1),
                       dx = dnorm(x, input$mean, input$sd))

        # make a plot
        ggplot(data, aes(x, dx)) + geom_line() + coord_cartesian(ylim = c(0, max(data$dx, .4)))
    })
}

shinyApp(ui = ui, server = server)
