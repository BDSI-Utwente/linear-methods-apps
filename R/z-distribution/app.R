# app: z-distribution
# last update: 20 June 2022

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
    pnorm(STARTING_Z_VALUE / 2, lower.tail = STARTING_Z_VALUE <= 0) %>% round(3)
RANGE <- c(-4, 4)

label_p_value_html <- function(z) { # Update label: < instead of <=
    glue(
        "\\(P(Z {ifelse(z <= 0, '<', '>')} {round(z, 2)}) = {ifelse(z <= 0, pnorm(z), 1 - pnorm(z)) %>% abs() %>% round(3)}\\)"
    )
}

label_p_value <- function(z) {# Update label: < instead of <=
    glue(
        "P(Z {ifelse(z <= 0, '<', '>')} {round(z, 2)}) = {ifelse(z <= 0, pnorm(z), 1 - pnorm(z)) %>% abs() %>% round(3)}"
    )
}

ui <- miniPage(
    title = "z distribution",
    theme = bslib::bs_theme(version = 4),
    uiOutput("css"),
    withMathJax(),
    miniContentPanel(
        conditionalPanel(
            "output.help",
            div(
                class = "alert alert-info p-1 rounded d-flex align-items-center",
                icon("warning", class = "h3 mx-3", verify_fa = FALSE),
                p(
                    class = "mb-0",
                    "This app has three modes: critical values, p-values, and combined. You can select the mode by adding",
                    code("?mode=critical_values"),
                    ",",
                    code("?mode=p_values"),
                    ", or",
                    code("?mode=combined"),
                    "to the url. You are currently viewing the combined mode. Choosing a mode will suppress this message."
                )
            )
        ),
        plotOutput("distribution", height = "100%")
    ),
    miniButtonBlock(
        style = "justify-content: space-around; gap: 1em; padding: .5em;",
        # critical values
        conditionalPanel(
            "output.mode == 'combined' || output.mode == 'critical_values'",
            div(
                class = "d-flex flex-column",
                sliderInput("alpha", "Type I error, \\(\\alpha\\)", 0.01, 0.2, 0.05, 0.01)
            ),
            div(# critical values readout
                htmlOutput("critical_values"))
        ),
        conditionalPanel(
            "output.mode == 'combined' || output.mode == 'p_values'",
            div(
                id = "p-value-panel",
                class = "d-flex",
                div(
                    class = "d-flex align-items-center",
                    numericInput("z_value", "\\(z\\)-value", STARTING_Z_VALUE,-4, 4, 0.01),
                    actionButton(
                        "z_to_p",
                        "Calculate \\(p\\)-value",
                        class = "btn-primary",
                        style = "position: relative; top: 8px;"
                    )
                ),
                div(
                    class = "d-flex align-items-center",
                    numericInput("p_value", "\\(p\\)-value", STARTING_P_VALUE, 0, 1, 0.01),
                    actionButton(
                        "p_to_z",
                        "Calculate \\(z\\)-value",
                        class = "btn-primary",
                        style = "position: relative; top: 8px;"
                    )
                )
            )
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
            crit_upper() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
            z_lower() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
            z_upper() + EXTRA_POINTS_AROUND_CRITICAL_VALUES
        ),
        fill_critical = ifelse(abs(z) >= crit_upper(), "#428BCA99", "transparent"),
        fill_p = ifelse(abs(z) >= z_upper(), "#B0306033", "transparent"),
        d = dnorm(z),
        p = pnorm(z)
    ))

    mode <- reactive({
        mode <- getQueryString()$mode
        if (is.null(mode))
            mode = "combined"
        mode
    })
    output$mode <- reactive(mode())

    help <- reactive(is.null(getQueryString()$mode))
    output$help <- reactive(help())
    output$css <- renderUI({
        tags$style(HTML("#p-value-panel {", htmltools::css( flex.direction = ifelse( mode() == "combined", "column", "row" ))), "}")
    })

    labels <- reactive(tibble(
        z = c(crit_lower(), crit_upper()),
        d = dnorm(z),
        label_quantile = glue("z == {round(z, 2)}"),
        label_percent = label_p_value(z)
    ))

    drawValue <- reactiveVal(FALSE)
    zValue <- reactiveVal(STARTING_Z_VALUE)
    z_lower <- reactive(-abs(zValue()))
    z_upper <- reactive(abs(zValue()))
    pValue <- reactiveVal(STARTING_P_VALUE)

    onCalculatePValue <- observe({
        zValue(input$z_value)
        pValue((pnorm(
            input$z_value, lower.tail = input$z_value <= 0
        ) * 2) %>% round(3))
        drawValue(TRUE)
        updateNumericInput(session, "p_value", value = pValue())
    }) %>% bindEvent(input$z_to_p)

    onCalculateZValue <- observe({
        pValue(input$p_value)
        zValue(qnorm(input$p_value / 2, lower.tail = input$z_value <= 0) %>% round(3))
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
        plot <- data() %>% ggplot(aes(z, d))+
        theme(axis.text.y=element_blank(), # Update: Remove y-axis
              axis.ticks.y=element_blank()) +
            theme(axis.text.x = element_text(face="italic", color="black", margin = margin(t = 0.5,  # Top margin
                                                                                           r = 2,  # Right margin
                                                                                           b = 0.5,  # Bottom margin
                                                                                           l = 2,  # Left margin
                                                                                           unit = "cm"), size=14))

        if (mode() == "critical_values" ||
            (mode() == "combined" && !drawValue())) {
            plot <- plot +
                geom_area(aes(fill = z <= crit_lower())) +
                geom_area(aes(fill = z >= crit_upper())) +
                scale_fill_discrete(guide = "none",
                                    type = c("#428BCA33", "#428BCA99"))
        }
        if (mode() != "p_values" && !drawValue()) { # Update: Do not show label critical values when calculating z-values/p-values
            plot <- plot +
                # geom_vline(
                #     xintercept = c(crit_lower(), crit_upper()),
                #     linetype = 5,
                #     size = .6,
                #     colour = "#226BAA"
                # ) +

            geom_text(
                aes(z, d, label = label_percent),
                fontface = "italic",
                data = labels(),
                # y = max(data()$d),
                hjust = "outward",
                vjust = "inward",
                nudge_x = c(-.05, 0.05),
                # colour = "#428BCA",
                size = 5,
                #parse = TRUE
            )+
            scale_x_continuous(breaks = c(crit_lower()%>% round(2), crit_upper()%>% round(2))) # Update: Show critical values in break
        }

        plot <- plot +
            geom_line(size = .5)
        # geom_text(aes())

        if (drawValue()) {
            plot <- plot +
                geom_area(aes(fill = z <= z_lower())) +
                geom_area(aes(fill = z >= z_upper())) +
                scale_fill_discrete(guide = "none",
                                    type = c("transparent", "#B0306033")) +
                # Update: not display vertical line
                # geom_vline(
                #     xintercept = c(z_lower(), z_upper()),
                #     linetype = 5,
                #     size = .6,
                #     colour = "maroon"
                # ) +
                geom_text(
                    aes(z, d, label = label),
                    fontface = "italic",
                    # y = max(data()$d) * .95,
                    data = tibble(
                        z = c(z_lower(), z_upper()),
                        d = dnorm(z),
                        label = label_p_value(z)
                    ),
                    hjust = "outward",
                    vjust = "inward",
                    nudge_x = c(-0.05, 0.05),
                    size = 5,
                    #parse = TRUE
                ) +
                scale_x_continuous(breaks = c(z_lower()%>% round(2), z_upper()%>% round(2))) # Update: Show critical values in break
        }
        plot
    })

    outputOptions(output, "mode", suspendWhenHidden = FALSE)
    outputOptions(output, "help", suspendWhenHidden = FALSE)
}

shiny::shinyApp(ui, server)