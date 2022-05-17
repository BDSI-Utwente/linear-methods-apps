library(shiny)
library(miniUI)
library(tidyverse)
library(glue)
library(Cairo)

options(shiny.usecairo = TRUE)
options(shiny.autoreload = TRUE)

PRECISION <- 0.05
EXTRA_POINTS_AROUND_CRITICAL_VALUES = c(-0.001, 0, 0.001)
STARTING_T_VALUE <- 2.25
STARTING_DF <- 10
STARTING_P_VALUE <- pt(STARTING_T_VALUE, STARTING_DF, lower.tail = STARTING_T_VALUE <= 0) %>% round(3)
RANGE <- c(-4, 4)

pad <- function(text, char = " ", count = 1) {
    paste0(paste0(rep(char, count), collapse = ""), text, paste0(rep(char, count), collapse = ""))
}
label_t_value_html <- function(t, df) {
    glue(
        "\\(P(T {ifelse(t <= 0, '\\\\leq', '\\\\geq')} {round(t, 2)}) = {ifelse(z <= 0, pt(z), 1 - pt(z)) %>% abs() %>% round(3)}, df = {df}\\)"
    )
}
label_t_value <- function(t, df) {
    glue(
        "P(T {ifelse(t <= 0, '<=', '>=')} {round(t, 2)}) == {ifelse(t <= 0, pt(t), 1 - pt(t)) %>% abs() %>% round(3)}, df == {df}"
    )
}

ui <- miniPage(
    title = "t-distribution",
    withMathJax(),
    miniContentPanel(plotOutput("distribution", height = "100%")),
    miniButtonBlock(
        style = "justify-content: space-around; gap: 1em; padding: .5em;",
        sliderInput("alpha", "Type-I error, \\(\\alpha\\)", 0.01, 0.2, 0.05, 0.01),
        # htmlOutput("critical_values"),
        div(
            numericInput("t_value", "T value", STARTING_T_VALUE, -4, 4, 0.01),
            actionButton("t_to_p", "Calculate \\(P(Z \\geq z)\\)", class = "btn-primary")
        ),
        div(
            numericInput("p_value", "P value", STARTING_P_VALUE, 0, 1, 0.01),
            actionButton("p_to_t", "Calculate T value", class = "btn-primary")
        )
    )
)

server <- function(input, output, session) {
    crit_lower <- reactive(qt(input$alpha / 2), input$df)
    crit_upper <- reactive(qt(1 - input$alpha / 2), input$df)

    zData <- reactive(tibble(
        z = c(seq(RANGE[1], RANGE[2], PRECISION),),
        d = dnorm(z),
        p = pnorm(z)
    ))

    tData <- reactive(tibble(
        t = c(
            seq(RANGE[1], RANGE[2], PRECISION),
            crit_lower() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
            crit_upper() + EXTRA_POINTS_AROUND_CRITICAL_VALUES
        ),
        d = dt(t, input$df),
        p = pt(t, input$df)
    ))

    labels <- reactive(tibble(
        t = c(crit_lower(), crit_upper()),
        label_quantile = glue("t == {round(t, 2)}"),
        label_percent = label_t_value(t, input$df)
    ))

    drawValue <- reactiveVal(FALSE)
    zValue <- reactiveVal(STARTING_Z_VALUE)
    pValue <- reactiveVal(STARTING_P_VALUE)

    onCalculatePValue <- observe({
        zValue(input$z_value)
        pValue(pnorm(input$z_value, lower.tail = input$z_value <= 0) %>% round(3))
        drawValue(TRUE)
        updateNumericInput(session, "p_value", value = pValue())
    }) %>% bindEvent(input$z_to_p)

    onCalculateZValue <- observe({
        pValue(input$p_value)
        zValue(qnorm(input$p_value, lower.tail = input$z_value <= 0) %>% round(3))
        drawValue(TRUE)
        updateNumericInput(session, "z_value", value = zValue())
    }) %>% bindEvent(input$p_to_z)

    output$critical_values <- renderPrint({
        tagList(
            tags$label("Critical values"),
            withMathJax(),
            labels() %>% pull(z) %>% label_p_value_html() %>% map(p)
        )
    })

    output$distribution <- renderPlot({
        plot <- data() %>% ggplot(aes(z, d)) +
            geom_area(aes(fill = z <= crit_lower())) +
            geom_area(aes(fill = z >= crit_upper())) +
            geom_vline(
                xintercept = c(crit_lower(), crit_upper()),
                linetype = 5,
                size = .6,
                colour = "#226BAA"
            ) +
            geom_text(
                aes(z, label = label_quantile),
                data = labels(),
                y = 0,
                hjust = "outward",
                vjust = "outward",
                nudge_x = c(-.05, 0.05),
                # colour = "#428BCA",
                size = 5,
                parse = TRUE
            ) +
            geom_text(
                aes(z, label = label_percent),
                data = labels(),
                y = max(data()$d),
                hjust = "outward",
                vjust = "inward",
                nudge_x = c(-.05, 0.05),
                # colour = "#428BCA",
                size = 5,
                parse = TRUE
            ) +
            geom_line(size = .5) +
            # geom_text(aes())
            scale_fill_discrete(guide = "none",
                                type = c("#428BCA33", "#428BCA99"))

        if (drawValue()) {
            plot <- plot +
                geom_vline(
                    xintercept = zValue(),
                    linetype = 2,
                    size = 1,
                    colour = "maroon"
                ) +
                geom_text(
                    aes(z, label = label),
                    y = max(data()$d) * .95,
                    data = tibble(z = zValue(), label = label_p_value(z)),
                    hjust = "outward",
                    vjust = "inward",
                    nudge_x = .03,
                    size = 6,
                    colour = "black",
                    parse = TRUE
                )
        }
        plot
    })
}

shiny::shinyApp(ui, server, enableBookmarking = "url")