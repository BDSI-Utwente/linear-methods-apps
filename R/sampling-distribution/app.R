#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(miniUI)
library(tidyverse)

options(shiny.autoreload = TRUE)

# Define UI for application that draws a histogram
ui <- miniPage(
    tags$style("
        .gadget-tabs-container {
            border-top: 1px solid #ccc;
        }
        .gadget-tabs-container ul.gadget-tabs {
            background-color: #e2e2e2;
        }"
    ),
    # tags$script(src="https://kit.fontawesome.com/3910aff050.js"),
    miniTabstripPanel(
    miniTabPanel(
        "Population",
        icon = icon("globe-europe"),
        imageOutput("population", height = "100%", width = "100%"),
        miniButtonBlock(
            style = "justify-content: space-around; align-items: center; gap: 1em;",
            selectInput("dist", "Distribution", c("normal", "skewed", "uniform")),
            conditionalPanel(
                "input.dist === 'normal' || input.dist === 'skewed'",
                sliderInput("mean", "Mean", 0, 100, 50)
            ),
            conditionalPanel(
                "input.dist === 'normal' || input.dist === 'skewed'",
                sliderInput("sd", "Standard deviation", 0, 10, 1, .1)
            ),

            conditionalPanel("input.dist === 'skewed'",
                             selectInput("skew", "Skew", c("left", "right"))),

            conditionalPanel("input.dist === 'uniform'",
                             sliderInput("min", "Min", 0, 49, 0)),
            conditionalPanel(
                "input.dist === 'uniform'",
                sliderInput("max", "Max", 50, 100, 100)
            )
        )
    ),
    # miniTabPanel("Samples"),
    miniTabPanel(
        "Sampling distribution",
        icon = icon("users"),
        imageOutput("sampling_distribution", height = "100%", width = "100%"),
        miniButtonBlock(
            style = "justify-content: space-around; align-items: center; gap: 1em;",
            sliderInput("sample_size", "Sample size", 2, 500, 10),
            sliderInput("sample_count", "Number of samples", 1, 1000, 50),
            radioButtons("param", "Parameter", c(Mean = "mean", SD = "sd"), inline = TRUE)
        )
    ),
    id = "page"
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    population <- reactive(if (input$dist == "normal") {
        return(tibble(x = rnorm(5000, input$mean, input$sd)))
    } else if (input$dist == "skewed") {
        if (input$skew == "left") {
            skew <- rbeta(5000, 2, 5)
        } else {
            skew <- rbeta(5000, 5, 2)
        }
        return(tibble(x = (skew - 0.5) * input$sd * 2 + input$mean))
    } else if (input$dist == "uniform") {
        return(tibble(x = runif(5000, input$min, input$max)))
    })

    samples <- reactive({
        pop <- population()
        n <- input$sample_count
        k <- input$sample_size
        map(1:n, ~ sample(pop$x, k, TRUE))
    })

    output$population <- renderPlot(ggplot(population(), aes(x)) +
                                        geom_histogram(bins = 20))

    output$sampling_distribution <- renderPlot({
        data <- tibble(mean = samples() %>% map_dbl(mean),
                       sd = samples() %>% map_dbl(sd))
        param <- input$param
        if (input$param == "mean") {
            plot <- ggplot(data, aes(mean)) +
                geom_histogram(bins = 20)
        }

        if (input$param == "sd") {
            plot <- ggplot(data, aes(sd)) +
                geom_histogram(bins = 20)
        }

        plot
    })
}

# Run the application
shinyApp(ui = ui, server = server)
