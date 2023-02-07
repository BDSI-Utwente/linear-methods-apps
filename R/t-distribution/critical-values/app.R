library(shiny)
library(miniUI)
library(tidyverse)
library(glue)
library(Cairo)
library(devtools)
library(plotly)

devtools::source_gist("https://gist.github.com/anhtth16/68f2b0d746590273ce2ec5c773dad2a5")

# Cairo device gives nicer anti-aliased graphics
options(shiny.usecairo = TRUE)
# Autoreload for dev
options(shiny.autoreload = TRUE)

# Main plot colour
COLOUR <- "#007bff"

# plot precision (number of steps for x axis)
PRECISION <- 500

# x-coordinate offsets for extra evaluation points
EXTRA_POINTS_AROUND_CRITICAL_VALUES <- c(-0.001, 0, 0.001)

# initial values for inputs
DF <- list(MIN = 1,
           MAX = 50,
           DEFAULT = 20)

A_LEVEL <- list(MIN = 0.001,
                MAX = 0.2,
                DEFAULT = 0.05)

H0 <- list(
    "Single-sided (Lower tail)" = 1,
    "Single-sided (Upper tail)" = 2,
    "Two-sided" = 3
)

# minimum plot range (extended when necessary below)
RANGE <- list(MIN = -4,
              MAX = 4)

# Define UI for application that draws a histogram
ui <- miniPage(
    title = "Student's t-distribution | Critital values",
    theme = bslib::bs_theme(version = 4),
    uiOutput("css"),
    withMathJax(),
    miniContentPanel(plotOutput("distPlot", height = "100%")),
    miniButtonBlock(
        div(
            style = "display: flex; flex-flow: column nowrap; flex: auto 1 1;",
            selectInput(
                "h",
                "Hypothesis",
                choices = H0,
                selectize = FALSE,
                selected = 3,
                width = "100%"
            ),
            sliderInput(
                "a",
                "\\(a\\)-level",
                0.001,
                0.2,
                0.05,
                0.001,
                ticks = FALSE,
                width = "100%"
            ),
            sliderInput(
                "df",
                "Degrees of freedom",
                DF$MIN,
                DF$MAX,
                DF$DEFAULT,
                ticks = FALSE,
                width = "100%"
            ),
        ),
        div(
            style = "display: flex; flex-flow: column nowrap; flex: auto 1 1;",
            tags$label("Critical value(s)"),
            conditionalPanel(
                "input.h != 2",
                tags$span("Lower tail", class = "small"),
                htmlOutput("critical_value_lower"),
                style = "margin-bottom: 1em;"
            ),
            conditionalPanel(
                "input.h != 1",
                tags$span("Upper tail", class = "small"),
                htmlOutput("critical_value_upper")
            )
        ),
        style = "gap: 1em; align-items: space-around; padding: 0.5em;"
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    data <- reactive({
        crit_values <- c(
            RANGE$MIN,
            RANGE$MAX,
            qt(input$a, input$df) + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
            qt(input$a, input$df, lower.tail = FALSE) + EXTRA_POINTS_AROUND_CRITICAL_VALUES
        )

        t <- c(crit_values,
               seq(min(crit_values), max(crit_values), length.out = PRECISION)) %>%
            sort() %>%
            round(3) %>%
            unique()

        d <- dt(t, input$df)
        tibble(t, d)
    }) %>% debounce(200)

    critical_values <- reactive({
        if (input$h == 1 || input$h == 2) {
            # single sided
            t <- qt(input$a, input$df) * c(1,-1)
        } else if (input$h == 3) {
            # two-sided
            t <- qt(input$a / 2, input$df) * c(1,-1)
        }

        d <- dt(t, input$df)
        p <-
            c(pt(t[1], input$df), pt(t[2], input$df, lower.tail = FALSE))
        tibble(t, d, p)
    })

    output$critical_value_upper <- renderPrint({
        withMathJax(span(
            glue(
                "\\(P(T_[{df}] > t) = {p}, \\quad t = {t}\\)",
                df = input$df,
                t = critical_values()$t[2] %>% round(3),
                p = critical_values()$p[2] %>% round(3)
            )
        ))
    })

    output$critical_value_lower <- renderPrint({
        withMathJax(span(
            glue(
                "\\(P(T_[{df}] ≤ t) = {p}, \\quad t = {t}\\)",
                df = input$df,
                t = critical_values()$t[1] %>% round(3),
                p = critical_values()$p[1] %>% round(3)
            )
        ))
    })

    output$distPlot <- renderPlot({
        plot <- ggplot(data(), aes(t, d)) +
            # main distribution lines
            geom_line(colour = COLOUR) +

            # vertical line at t
            # geom_segment(x = input$t, xend = input$t, y = 0, yend = dt(input$t, input$df), colour = "#007bff", linewidth = 0.2) +

            # fill area for P(t) >= T
            # geom_area(data = data_under_h0 %>% filter(dist == "t"), fill = "#007bff", alpha = 0.2) +

            # add annotations
            # annotate(
            #   "text",
            #   x = input$t,
        #   y = dt(input$t, input$df),
        #   label = glue("P(T >= {t}) = {p}", t = input$t, p = input$p),
        #   hjust = ifelse(input$t >= 0, -0.1, 1.1),
        #   vjust = -0.1
        # ) +

        # set x axis to be at zero
        scale_y_continuous(expand = c(0, 0))

        if (input$h != 1) {
            # upper tail
            val <- critical_values()[2, ]

            plot <- plot +
                geom_segment(
                    x = val$t,
                    xend = val$t,
                    y = 0,
                    yend = val$d,
                    colour = COLOUR
                ) +
                geom_area(
                    data = data() %>% filter(t > val$t),
                    fill = COLOUR,
                    alpha = 0.2
                ) +
                annotate(
                    "text",
                    val$t,
                    val$d,
                    parse = TRUE,
                    label = glue(
                        "P(T[{df}]>{t})=={p}",
                        df = input$df,
                        t = val$t %>% round(3),
                        p = val$p %>% round(3)
                    ),
                    vjust = 0,
                    hjust = -0.1
                )
        }

        if (input$h != 2) {
            val <- critical_values()[1, ]

            plot <- plot +
                geom_segment(
                    x = val$t,
                    xend = val$t,
                    y = 0,
                    yend = val$d,
                    colour = COLOUR
                ) +
                geom_area(
                    data = data() %>% filter(t <= val$t),
                    fill = COLOUR,
                    alpha = 0.2
                ) +
                annotate(
                    "text",
                    val$t,
                    val$d,
                    parse = TRUE,
                    label = glue(
                        "P(T[{df}]<={t})=={p}",
                        df = input$df,
                        t = val$t %>% round(3),
                        p = val$p %>% round(3)
                    ),
                    vjust = 0,
                    hjust = 1.1
                )
        }

        plot

    }) %>% bindEvent(data(), input$h)
}
# Run the application
shinyApp(ui = ui, server = server)
