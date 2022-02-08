library(shiny)
library(miniUI)
library(tidyverse)
library(lme4)
library(modelr) # get residuals
# library(performance) # compute ICC
library(insight) # getting variance components
library(Cairo)

INTERCEPT <- 100
SEED <- 1234

options(shiny.autoreload = TRUE)

ui <- miniPage(
    theme = bslib::bs_theme(version = 4),
    title = "intra class correlation",
    miniContentPanel(
        conditionalPanel(
            "output.help",
            class = "alert alert-info p-1 rounded d-flex align-items-center",
            icon("warning", class = "h3 mx-3"),
            p(
                class = "mb-0",
                "This app has a light and a full mode, which can be selected by appending",
                code("?mode=light"),
                "or",
                code("?mode=full"),
                "to the url. You are currently viewing the full mode. Choosing a mode will suppress this message."
            )
        ),
        fillRow(
            plotOutput("naive", height = "100%"),
            plotOutput("corrected", height = "100%")
        )
    ),
    miniButtonBlock(
        style = "display: flex; justify-content: space-around; gap: 1em; padding: .5em;",
        conditionalPanel(
            "output.mode=='full'",
            sliderInput("n", "Respondents", 10, 20, 10, 1)
        ),
        conditionalPanel(
            "output.mode=='full'",
            sliderInput("p", "Measurements", 2, 20, 15, 1)
        ),
        conditionalPanel(
            "output.mode=='full'",
            sliderInput("var", "Random variance", 0.01, 20, 10, 0.01)
        ),
        sliderInput("icc", "Intra class correlation", 0.05, 0.99, 0.1, 0.05)
    )
)

server <- function(input, output) {
    model <- reactiveVal()
    data <- reactive({
        set.seed(SEED)
        patient <- rep(1:input$n, input$p) %>% factor()
        patient_effect <- rnorm(input$n, 0, sqrt(input$var))

        measure <- rep(1:input$p, each = input$n)

        var_resid <- input$var / input$icc - input$var
        y <- rnorm(input$n * input$p,
                   INTERCEPT + rep(patient_effect, input$p),
                   sqrt(var_resid))

        df <- tibble(patient, measure, y, resid_naive = y - mean(y))
        model(lmer(y ~ (1 | patient), REML = FALSE, data = df))
        df %>% add_residuals(model(), "resid_random")
    })

    observe({
        if (is.null(model()))
            return()
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
    }) %>% bindEvent(model())

    output$naive <- renderPlot({
        data() %>% ggplot(aes(patient, resid_naive)) +
            geom_boxplot() +
            labs(y = "Residual", title = "Residuals without random intercept") +
            coord_cartesian(ylim = data() %>% select(starts_with("resid")) %>% range())
    }) %>% bindCache(model()) %>% bindEvent(model(), data())

    output$corrected <- renderPlot({
        data() %>% ggplot(aes(patient, resid_random)) +
            geom_boxplot() +
            labs(y = "Residual", title = "Residuals with random intercept") +
            coord_cartesian(ylim = data() %>% select(starts_with("resid")) %>% range())
    }) %>% bindCache(model())

    output$mode <- reactive({
        qs <- getQueryString()
        ifelse(is.null(qs$mode), "full", qs$mode)
    })

    output$help <- reactive({
        qs <- getQueryString()
        is.null(qs$mode)
    })

    outputOptions(output, "mode", suspendWhenHidden = FALSE)
    outputOptions(output, "help", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
