library(shiny)
library(miniUI)
library(tidyverse)
library(glue)
library(Cairo)
library(devtools)
library(plotly)
library(magrittr)

devtools::source_gist("https://gist.github.com/anhtth16/68f2b0d746590273ce2ec5c773dad2a5")

# Cairo device gives nicer anti-aliased graphics
options(shiny.usecairo = TRUE)
# Autoreload for dev
options(shiny.autoreload = TRUE)

# plot precision (number of steps for x axis)
PRECISION <- 500

# x-coordinate offsets for extra evaluation points
EXTRA_POINTS_AROUND_CRITICAL_VALUES <- c(-0.001, 0, 0.001)

# initial values for inputs
DF <- list(
  MIN = 1,
  MAX = 50,
  DEFAULT = 20
)
T_VALUE <- list(DEFAULT = 2.248)
P_VALUE <- list(
  DEFAULT = pt(T_VALUE$DEFAULT, DF$DEFAULT, lower.tail = T_VALUE$DEFAULT <= 0) %>% round(3)
)
A_LEVEL <- list(
  MIN = 0.01,
  MAX = 0.2,
  DEFAULT = 0.05
)

# plot range (assuming sane inputs)
RANGE <- list(
  MIN = -4,
  MAX = 4
)

# Define UI for application that draws a histogram
ui <- miniPage(
  title = "t distribution",
  theme = bslib::bs_theme(version = 4),
  uiOutput("css"),
  withMathJax(),
  # div(
  #   textOutput("normal"),
  #   textOutput("pvalues"),
  #   textOutput("twosided")
  # ),
  miniContentPanel(plotOutput("distPlot", height = "100%"),),
  miniButtonBlock(
    div(
      style = "display: flex; flex-flow: column nowrap; flex: auto 1 1;",
      # numericInput("a", "\\(a\\)-level", A_LEVEL$DEFAULT, min = A_LEVEL$MIN, max = A_LEVEL$MAX, width = "100%", step = 0.01),
      sliderInput("df", "Degrees of freedom", DF$MIN, DF$MAX, DF$DEFAULT, width = "100%"),
    ),

    conditionalPanel("output.pvalues == 'TRUE'",
      style = "display: flex; flex-flow: row nowrap; flex: auto 1 1; justify-content: space-between; align-items: flex-start;",
      numericInput("t", "\\(t\\)-value", T_VALUE$DEFAULT, step = 0.01, width = "100%"),
      # actionButton("calculate-t", label = NULL, icon = icon("repeat"), title="Calculate t-value from p-value and df", style="margin-top: 2em;")
    ),

    conditionalPanel("output.pvalues == 'TRUE'",
      style = "display: flex; flex-flow: row nowrap; flex: auto 1 1; justify-content: space-between; align-items: flex-start;",
      numericInput("p", "\\(p\\)-value", P_VALUE$DEFAULT, min = 0.0001, max = 0.9999, step = 0.001, width = "100%"),
      # actionButton("calculate-p", label = NULL, icon = icon("repeat"), title="Calculate p-value from t-value and df", style="margin-top: 2em;")

      # textOutput("critital_value_lower"),
      # textOutput("critical_value_upper")
    ),
    style = "gap: 1em; align-items: space-around; padding: 0.5em;"
  ),
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #' Reactive for query paramaters
  #' @param normal turn normal overlay on/off
  #' @param twosided toggle twosided mode (as opposed to one-sided)
  #' @param pvalues toggle p/t-values on/off
  params <- reactive({
    search_params <- parseQueryString(session$clientData$url_search)

    list(
      normal = !is.null(search_params$normal),
      twosided = !is.null(search_params$twosided),
      pvalues = !is.null(search_params$pvalues)
    )
  })

  # Add params back to output object, and make sure they are always updated
  output$normal <- renderText(params()$normal)
  outputOptions(output, "normal", suspendWhenHidden = FALSE)

  output$twosided <- renderText(params()$twosided)
  outputOptions(output, "twosided", suspendWhenHidden = FALSE)

  output$pvalues <- renderText(params()$pvalues)
  outputOptions(output, "pvalues", suspendWhenHidden = FALSE)

  data <- reactive({
    # build a list of t/z values that we want the graph to cover
    crit_values <- c(
      # default range (usually -3,3)
      RANGE$MIN,
      RANGE$MAX,

      # cover upper and lower tails
      qt(A_LEVEL$MIN, input$df),
      qt(A_LEVEL$MIN, input$df, lower.tail = FALSE)
    )

    # make sure t-value is covered, if given
    if(params()$pvalues){
      crit_values <- c(crit_values, input$t)
    }

    # interpolate extra points between the extremes of our critical values,
    # round them to avoid floating point errors, then filter out duplicates
    x <- c(
      crit_values,
      seq(min(crit_values), max(crit_values), length.out = PRECISION)
    ) %>%
      sort() %>%
      round(3) %>%
      unique()

    t <- dt(x, input$df)
    z <- dnorm(x)

    bind_rows(
      tibble(x, y = t, dist = "t"),
      tibble(x, y = z, dist = "z")
    )
  }) %>% debounce(200)

  #' Flag signal to stop observers triggering on each other
  #'
  #' Whenever an 'automatic' update is triggered, we set a flag. On the
  #' next update, we first check the flag. If it was set, we assume the
  #' current update was triggered as a reaction to the previous automated
  #' update, and do not react. We then clear the update flag, so that we
  #' are ready to react to new (presumably user-generated) update events.
  #'
  #' This is all rather fragile, and requires we have a clear idea of the
  #' possible reactions. In our case, we want to update the p-value when
  #' df or t-value changes, and change the t-value when p-value changes.
  #'
  #' Note that it would be entirely feasible to update t-value when df
  #' changes, we just chose not to go down that route.
  #'
  #' If we were to map reactions as a directed graph, we'd need to set the
  #' flag on all non-leaf updates, and check if the flag was set on all
  #' non-root updates. In our case, we have one root (df), and no leaves
  #' (t and p-values both react to (at least) each other).
  updating <- reactiveVal(FALSE)

  on_df_changed <- observe({
    # print("df changed, updating p...")

    updating(TRUE)
    p_value <- pt(input$t, input$df, lower.tail = input$t <= 0)
    if (params()$twosided) {
      p_value <- p_value * 2
    }
    updateNumericInput(session, "p", value = p_value  %>% round(3))
  }, priority = 9) %>% bindEvent(input$df, ignoreInit = TRUE)

  on_t_changed <- observe({
    # print("t changed, checking if we should update p")

    if(updating()) {
      updating(FALSE)
      # print("ignoring reaction to automated update")
    } else {
      # print("manual update, updating p...")

      updating(TRUE)
      p_value <- pt(input$t, input$df, lower.tail = input$t <= 0)
      if (params()$twosided) {
        p_value <- p_value * 2
      }
      updateNumericInput(session, "p", value = p_value  %>% round(3))
    }
  }, priority = 7) %>% bindEvent(input$t, ignoreInit = FALSE)

  on_p_changed <- observe({
    # print("p changed, checking if we should update t")

    if(updating()){
      updating(FALSE)
      # print("ignoring reaction to automated update")
    } else {
      # print("manual update, updating t...")

      updating(TRUE)
      updateNumericInput(session, "t", value = qt(1 - input$p * if_else(params()$twosided, 0.5, 1), input$df) %>% round(3))
    }
  }, priority = 5) %>% bindEvent(input$p, ignoreInit = TRUE)

  output$distPlot <- renderPlot({
    if (params()$twosided) {
      critical_t <- abs(input$t)
      data_under_h0 <- data() %>% filter(x >= critical_t | x <= -critical_t)
    } else {
      if (input$t >= 0){
        data_under_h0 <- data() %>% filter(x >= input$t)
      } else {
        data_under_h0 <- data() %>% filter(x <= input$t)
      }
    }

    plot <- ggplot(data(), aes(x = x, y = y)) +
      # t distribution
      geom_line(data = data() %>% filter(dist == "t"), colour = "#007bff")

    # add normal (z) overlay if desired
    if (params()$normal) {
      plot <- plot +
        geom_line(data = data() %>% filter(dist == "z"), colour = "grey", linetype = 2)
    }

    # add p/t value(s) if desired
    if (params()$pvalues) {
      plot <- plot +
        # vertical line at t
        geom_segment(x = input$t, xend = input$t, y = 0, yend = dt(input$t, input$df), colour = "#007bff", linewidth = 0.2) +

        # fill area for P(t) >= T, or P(|t|) >= in the twosided case
        geom_area(aes(group = x <= 0), data = data_under_h0 %>% filter(dist == "t"), fill = "#007bff", alpha = 0.2)

      # add another line for the twosided case
      if (params()$twosided) {
        plot <- plot + geom_segment(x = -input$t, xend = -input$t, y = 0, yend = dt(-input$t, input$df), colour = "#007bff", linewidth = 0.2)
      }

      # add annotations
      tail_p_value <- pt(input$t, input$df, lower.tail = input$t <= 0)
      plot <- plot + annotate(
        "text",
        x = input$t,
        y = dt(input$t, input$df),
        label = glue("P(T >= {t}) = {p}", t = input$t, p = tail_p_value %>% round(3)),
        hjust = ifelse(input$t >= 0, -0.1, 1.1),
        vjust = -0.1
      )
      if (params()$twosided) {
        plot <- plot + annotate(
          "text",
          x = -input$t,
          y = dt(-input$t, input$df),
          label = glue("P(T < {t}) = {p}", t = input$t, p = tail_p_value %>% round(3)),
          hjust = ifelse(-input$t >= 0, -0.1, 1.1),
          vjust = -0.1
        )
      }
    }

    # set x axis to be at zero
    plot <- plot +
      scale_y_continuous(expand = c(0, 0))
#
#       # add manual legend
#       scale_colour_manual(
#         guide = NULL,
#         name = "Distribution",
#         values = c("t" = "#007bff", "z" = "grey"),
#         labels = labels
#       ) +
#       scale_linetype_manual(
#         guide = NULL,
#         name = "Distribution",
#         values = c("t" = 1, "z" = 2),
#         labels = labels
#       )

    plot
  }) %>% bindEvent(data())


}
# Run the application
shinyApp(ui = ui, server = server)

