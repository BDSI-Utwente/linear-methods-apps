# App: t_distribution
# Last update: 15 May 22

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
STARTING_DF <- 2
STARTING_P_VALUE <-
  pt(STARTING_T_VALUE / 2, STARTING_DF, lower.tail = STARTING_T_VALUE <= 0) %>% round(3)

RANGE <- c(-4, 4) # range for T value

label_t_value_html <- function(t, df) {
  glue(
    "\\(P(T {ifelse(t <= 0, '\\\\leq', '\\\\geq')} {round(t, 2)}) = {ifelse(t <= 0, pt(t, df), 1 - pt(t, df)) %>% abs() %>% round(3)}\\)"
  )
}

label_t_value <- function(t, df) {
  glue(
    "P(T {ifelse(t <= 0, '<=', '>=')} {round(t, 2)}) == {ifelse(t <= 0, pt(t, df), 1 - pt(t, df)) %>% abs() %>% round(3)}"
  )
}

## UI ---------------------------------------------------------
ui <- miniPage(
  title = "t distribution",
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
    conditionalPanel( # set significance level
      "output.mode == 'combined' || output.mode == 'critical_values'",
       div(
         id = "critical-value-panel",
         class = "d-flex flex-column",

         div(# set alpha
          class = "d-flex",
          sliderInput("alpha", "Type I error, \\(\\alpha\\)", 0.01, 0.2, 0.05, 0.01)
          ),
         div(# degree of freedom
          class = "align-items-center",
          sliderInput(inputId = "df",
                      label = "Degrees of Freedom", min = 1, max = 200, value = 1, step = 1)

          # numericInput(inputId = "df", label = "Degrees of Freedom", STARTING_DF,
          #              min = 1, max = 200, value = 1, step = 1
          #              )
          ),
         div(# critical values readout
           htmlOutput("critical_values")
         )
      ),
    ),

    conditionalPanel(
      "output.mode == 'combined' || output.mode == 'p_values'",
      div( # set t value
        id = "p-value-panel",
        class = "d-flex",
        div(
          class = "d-flex align-items-center",
          numericInput("t_value", "\\(t\\)-value", STARTING_T_VALUE, -4, 4, 0.01),
          actionButton(
            "t_to_p",
            "Calculate \\(p\\)-value",
            class = "btn-primary",
            style = "position: relative; top: 8px;"
          )
        ),
        div( # calculate p_value
          class = "d-flex align-items-center",
          numericInput("p_value", "\\(p\\)-value", STARTING_P_VALUE, 0, 1, 0.01),
          actionButton(
            "p_to_t",
            "Calculate \\(t\\)-value",
            class = "btn-primary",
            style = "position: relative; top: 8px;"
          )
        )
      )
    )
  )
)

# SERVER ---------------------------------------------------------
server <- function(input, output, session) {
  crit_lower <- reactive(qt(input$alpha / 2, input$df)) # find lower critical value
  crit_upper <- reactive(qt(1 - input$alpha / 2, input$df)) # find upper critical value
  data <- reactive(tibble(
    t = c(
      seq(RANGE[1], RANGE[2], PRECISION),
      crit_lower() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
      crit_upper() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
      t_lower() + EXTRA_POINTS_AROUND_CRITICAL_VALUES,
      t_upper() + EXTRA_POINTS_AROUND_CRITICAL_VALUES
    ),
    fill_critical = ifelse(abs(t) >= crit_upper(), "#428BCA99", "transparent"),
    fill_p = ifelse(abs(t) >= t_upper(), "#B0306033", "transparent"),
    d = dt(t, input$df),
    p = pt(t, input$df),
  ))

  mode <- reactive({ # default mode is combined
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
    t = c(crit_lower(), crit_upper()),
    d = dt(t, input$df),
    label_quantile = glue("t == {round(t, 2)}"),
    df = input$df,
    label_percent = label_t_value(t, input$df)
  ))


  drawValue <- reactiveVal(FALSE)
  tValue <- reactiveVal(STARTING_T_VALUE)
  t_lower <- reactive(-abs(tValue()))
  t_upper <- reactive(abs(tValue()))
  pValue <- reactiveVal(STARTING_T_VALUE)

  ## Calculate p-value based on t: take t_value from input, calculate p-value using pt()
  onCalculatePValue <- observe({
    tValue(input$t_value) # taketz from input
    pValue((pt(
      input$t_value, input$df, lower.tail = input$t_value <= 0
    ) * 2) %>% round(3))
    drawValue(TRUE)
    updateNumericInput(session, "p_value", value = pValue())
  }) %>% bindEvent(input$t_to_p)

  ## Calculate t-value based on p: take p_value from input, calculate t-value using qt()
  onCalculatetValue <- observe({
    pValue(input$p_value)
    tValue(qt(input$p_value / 2, input$df, lower.tail = input$t_value <= 0) %>% round(3))
    drawValue(TRUE)
    updateNumericInput(session, "t_value", value = tValue())
  }) %>% bindEvent(input$p_to_t)

  output$critical_values <- renderPrint({
    tagList(
      tags$label("Critical values"),
      withMathJax(),
      labels() %>% select(t, df) %>% purrr::pmap(label_p_value_html) %>% map(p)
    )
  })

  ## PLOT ---------------------------------------------------------
  output$distribution <- renderPlot({
    plot <- data() %>% ggplot(aes(t, d))

    if (mode() == "critical_values" ||
        (mode() == "combined" && !drawValue())) {
      plot <- plot +
        geom_area(aes(fill = t <= crit_lower())) +
        geom_area(aes(fill = t >= crit_upper())) +
        scale_fill_discrete(guide = "none",
                            type = c("#428BCA33", "#428BCA99"))
    }
    if (mode() != "p_values") {
      plot <- plot +
        geom_vline(
          xintercept = c(crit_lower(), crit_upper()),
          linetype = 5,
          size = .6,
          colour = "#226BAA"
        ) +
        # geom_text(
        #     aes(z, label = label_quantile),
        #     data = labels(),
        #     y = 0,
        #     hjust = "outward",
        #     vjust = "outward",
        #     nudge_x = c(-.05, 0.05),
        #     # colour = "#428BCA",
        #     size = 5,
        #     parse = TRUE
        # ) +
      geom_text(
        aes(t, d, label = label_percent),
        data = labels(),
        # y = max(data()$d),
        hjust = "outward",
        vjust = "inward",
        nudge_x = c(-.05, 0.05),
        # colour = "#428BCA",
        size = 5,
        parse = TRUE
      )
    }

    plot <- plot +
      geom_line(size = .5)
    # geom_text(aes())

    if (drawValue()) {
      plot <- plot +
        geom_area(aes(fill = t <= t_lower())) +
        geom_area(aes(fill = t >= t_upper())) +
        scale_fill_discrete(guide = "none",
                            type = c("transparent", "#B0306033")) +
        geom_vline(
          xintercept = c(t_lower(), t_upper()),
          linetype = 5,
          size = .6,
          colour = "maroon"
        ) +
        geom_text(
          aes(t, d, label = label),
          # y = max(data()$d) * .95,
          data = tibble(
            t = c(t_lower(), t_upper()),
            d = dt(t, input$df),
            label = label_t_value(t, input$df)
          ),
          hjust = "outward",
          vjust = "inward",
          nudge_x = c(-0.05, 0.05),
          size = 5,
          parse = TRUE
        )
    }
    plot
  })

  outputOptions(output, "mode", suspendWhenHidden = FALSE)
  outputOptions(output, "help", suspendWhenHidden = FALSE)
}

shiny::shinyApp(ui, server) %>% shiny::runApp()
