library(shiny)
library(miniUI)
library(tidyverse)
library(glue)
library(Cairo)

options(shiny.usecairo = TRUE)
options(shiny.autoreload = TRUE)

PRECISION <- 0.05
EXTRA_POINTS_AROUND_CRITICAL_VALUES = c(-0.001, 0, 0.001)
STARTING_Z_VALUE <- 2.25
STARTING_P_VALUE <-
    pnorm(STARTING_Z_VALUE, lower.tail = STARTING_Z_VALUE <= 0) %>% round(3)
RANGE <- c(-4, 4)

pad <- function(text, char = " ", count = 1) {
    paste0(paste0(rep(char, count), collapse = ""), text, paste0(rep(char, count), collapse = ""))
}
label_p_value_html <- function(z) {
    glue("\\(P(Z {ifelse(z <= 0, '\\\\leq', '\\\\geq')} {round(z, 2)}) = {ifelse(z <= 0, pnorm(z), 1 - pnorm(z)) %>% abs() %>% round(3)}\\)")
}
label_p_value <- function(z) {
    glue("P(Z {ifelse(z <= 0, '<=', '>=')} {round(z, 2)}) == {ifelse(z <= 0, pnorm(z), 1 - pnorm(z)) %>% abs() %>% round(3)}")
}

ui <- miniPage(
        withMathJax(),
        miniContentPanel(plotOutput("distribution", height = "100%")),
        miniButtonBlock(
            style = "justify-content: space-around; gap: 1em; padding: .5em;",
            sliderInput("alpha", "Type I error, \\(\\alpha\\)", 0.01, 0.2, 0.05, 0.01),
            # htmlOutput("critical_values"),
            div(
                numericInput("z_value", "\\(z\\)-value", STARTING_Z_VALUE, -4, 4, 0.01),
                actionButton("z_to_p", "Calculate \\(P(Z \\geq |z|)\\)", class = "btn-primary")
            ),
            div(
                numericInput("p_value", "\\(p\\)-value", STARTING_P_VALUE, 0, 1, 0.01),
                actionButton("p_to_z", "Calculate Z value", class = "btn-primary")
            )
        )
    )

server <- function(input, output, session) {
    crit_lower <- reactive(qnorm(input$alpha / 2))
    crit_upper <- reactive(qnorm(1 - input$alpha / 2))
    data <- reactive(tibble(
        z = c(
            seq(RANGE[1], RANGE[2], PRECISION),
            crit_lower() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
            crit_upper() + EXTRA_POINTS_AROUND_CRITICAL_VALUES
        ),
        d = dnorm(z),
        p = pnorm(z)
    ))

    labels <- reactive(tibble(
        z = c(crit_lower(), crit_upper()),
        label_quantile = glue("z == {round(z, 2)}"),
        label_percent = label_p_value(z)
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