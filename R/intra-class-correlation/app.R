library(shiny)
library(miniUI)
library(tidyverse)
library(lme4)
library(modelr) # get residuals
# library(performance) # compute ICC
library(insight) # getting variance components

INTERCEPT <- 100
SEED <- 1234

options(shiny.autoreload = TRUE)

ui <- miniPage(
    miniContentPanel(fillRow(
        plotOutput("naive", height = "100%"),
        plotOutput("corrected", height = "100%")
    )),
    miniButtonBlock(
        style = "display: flex; justify-content: space-around; gap: 1em;",
        sliderInput("n", "Respondents", 10, 20, 10, 1),
        sliderInput("p", "Measurements", 2, 20, 10, 1),
        sliderInput("icc", "Intra class correlation", 0.05, 0.95, 0.1, 0.05),
        sliderInput("var", "Random variance", 0.01, 20, 10, 0.01)
    )
)

server <- function(input, output) {
    data <- reactive({
        set.seed(SEED)
        patient <- rep(1:input$n, input$p) %>% factor()
        patient_effect <- rnorm(input$n, 0, sqrt(input$var))

        measure <- rep(1:input$p, each = input$n)

        var_resid <- input$var / input$icc - input$var
        y <- rnorm(input$n * input$p,
                   INTERCEPT + rep(patient_effect, input$p),
                   sqrt(var_resid))

        tibble(patient, measure, y)
    })

    model <- reactive(lmer(
        y ~ measure + (1 | patient),
        REML = TRUE,
        data = data()
    ))

    observe({
        cat(
            "\nRespondents:",
            input$n,
            "\nMeasurements:",
            input$p,
            "\nICC in -> out:",
            input$icc,
            "->",
            get_variance_random(model()) / (
                get_variance_random(model()) + get_variance_residual(model())
            )
        )
    })

    output$naive <- renderPlot({
        data() %>% ggplot(aes(patient, y)) + geom_boxplot()
    }) %>% bindCache(input$n, input$p, input$icc, input$var)

    output$corrected <- renderPlot({
        data() %>% add_residuals(model()) %>% ggplot(aes(patient, resid)) + geom_boxplot() + labs(y = "Residual")
    }) %>% bindCache(input$n, input$p, input$icc, input$var)
}

shinyApp(ui = ui, server = server)
