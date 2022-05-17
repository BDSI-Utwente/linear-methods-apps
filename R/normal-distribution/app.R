library(shiny)
library(miniUI)
library(tidyverse)

options(shiny.autoreload = TRUE)

PRECISION = .01
RANGE = c(-10, 10)

ui <- miniPage(
    miniContentPanel(plotOutput(
        "distPlot", width = "100%", height = "100%"
    )),
    miniButtonBlock(
        style = "justify-content: space-between; gap: 1em;",
        sliderInput("mean",
                    "Mean", -10,
                    10,
                    0,
                    1,
                    width = "100%"),
        sliderInput("sd",
                    "Standard deviation",
                    0.1,
                    10,
                    1,
                    .1,
                    width = "100%"),
        actionButton("reset", "Reset to standard normal", icon = icon("undo-alt"), class = "btn btn-primary", style = "height: min-content; margin: auto 0;")
    )
)


server <- function(input, output, session) {
    onResetSN <- observe({
        updateSliderInput(session, "mean", value = 0)
        updateSliderInput(session, "sd", value = 1)
    }) %>% bindEvent(input$reset)

    data <- reactive({
        tibble(
            x = seq(RANGE[1], RANGE[2], PRECISION),
            dx = dnorm(x, input$mean, input$sd)
        )
    }) %>% bindCache(input$mean, input$sd)

    output$distPlot <- renderPlot({
        ggplot(data(), aes(x, dx)) +
            geom_line() +
            coord_cartesian(ylim = c(0, max(data()$dx, .4))) +
            labs(y = "density")
    }) %>% bindCache(input$mean, input$sd)
}

shinyApp(ui = ui, server = server)
