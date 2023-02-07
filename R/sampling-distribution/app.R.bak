# Application: Sampling distribution

# Notes:
# - how relevant is it to let students set the specifics of the distribution? 
#   - we could remove the extra fields, making space in the two-plot solution
# - can we plot the results of drawing samples in the same plot? 
#   - update for intermediate results (e.g. almost animated)
#   - use different colors for different sampling distributions (different sizes, number of samples)
#   - makes it clear that the sampling distribution both normalizes and narrows with more/larger samples (averages out)

library(shiny)
library(miniUI)
library(tidyverse)

# source("../style.R") # update style from local script
# Update: load style from public gist
if (!("devtools" %in% installed.packages()[, 1])) {
    install.packages("devtools")
}
devtools::source_gist("https://gist.github.com/anhtth16/68f2b0d746590273ce2ec5c773dad2a5")

options(shiny.autoreload = TRUE)

ui <- miniPage(
    tags$style("
        .gadget-tabs-container {
            border-top: 1px solid #ccc;
        }
        .gadget-tabs-container ul.gadget-tabs {
            background-color: #e2e2e2;
        }"
    ),
    # TODO: figure out why I need to reference font awesome manually
    tags$script(src="https://kit.fontawesome.com/3910aff050.js"),
    shiny::fillRow(
        shiny::fillCol(
        imageOutput("population", height = "100%", width = "100%"),
        miniButtonBlock(
            style = "justify-content: space-around; align-items: center; gap: 1em; padding-right: 0.5em;",
            div(
            selectInput("dist", "Distribution", c("normal", "skewed", "uniform"), selectize=FALSE, width="100%"),
            style="width: min-content; flex: auto 1 1;",
            ),
            conditionalPanel(
                "input.dist === 'normal' || input.dist === 'skewed'",
                sliderInput("mean", "Mean", 0, 100, 50, width="100%"),
                style="width: min-content; flex: auto 1 1;",
            ),
            conditionalPanel(
                "input.dist === 'normal' || input.dist === 'skewed'",
                sliderInput("sd", "Standard deviation", 0, 10, 1, .1, width="100%"),
                style="width: min-content; flex: auto 1 1;",
            ),

            conditionalPanel("input.dist === 'skewed'",
                             selectInput("skew", "Skew", c("left", "right"), selectize=FALSE, width="100%"),
                style="width: min-content; flex: auto 1 1;",),

            conditionalPanel("input.dist === 'uniform'",
                             sliderInput("min", "Min", 0, 49, 0, width="100%"),
                style="width: min-content; flex: auto 1 1;",),
            conditionalPanel(
                "input.dist === 'uniform'",
                sliderInput("max", "Max", 50, 100, 100, width="100%"),
                style="width: min-content; flex: auto 1 1;",
            )
        ),
        # style = "border-right: 2px dashed grey;",
         flex = c(1, NA)
    ),
        shiny::fillCol(
        imageOutput("sampling_distribution", height = "100%", width = "100%"),
        miniButtonBlock(
            style = "justify-content: space-around; align-items: center; gap: 1em; padding-left: 0.5em;",
            sliderInput("sample_size", "Sample size", 2, 500, 10),
            sliderInput("sample_count", "Number of samples", 1, 1000, 50),
            radioButtons("param", "Parameter", c(Mean = "mean", SD = "sd"), inline = TRUE)
        ), flex = c(1, NA)
    ), flex = 1
))

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
runApp(shinyApp)
