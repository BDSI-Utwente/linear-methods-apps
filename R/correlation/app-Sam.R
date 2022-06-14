# App: correlation
# Last update: 14 June 22

require(tidyverse)
require(miniUI)
require(shiny)
require(shinyjs)
require(plotly)

# options(shiny.autoreload = TRUE)

INITIAL_R = .5
NUM_OBSERVATIONS = 300
SEED = 1234444
LIMITS = c(-4, 4)

# based on https://stats.stackexchange.com/questions/522454/how-to-construct-simulate-data-that-will-have-a-given-coefficient-of-determina/522470#522470
# Define UI for application that draws a histogram

ui <- miniPage(
    title = "correlation",
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
                    "This app has two modes: show linear fit, or not. You can select the mode by adding",
                    code("?mode=linear"),
                    ", or",
                    code("?mode=none"),
                    "to the url. You are currently viewing the mode not showing linear fit. Choosing a mode will suppress this message."
                )
            )
        ),
        plotOutput("corPlot", height = "100%")
    ),

    miniButtonBlock(
        style = "justify-content: space-around; align-items: center; gap: 1em;",

        sliderInput("r",
                    "Correlation",
                    0,
                    1,
                    value = INITIAL_R,
                    0.01),

        conditionalPanel( # Checj box to show linear fit, this only display in mode "linear"
            "output.mode == 'linear'",
            checkboxInput("linear", "Show linear fit?", FALSE)
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    set.seed(SEED)
    x <- rnorm(NUM_OBSERVATIONS)
    ssr <- sum((x - mean(x)) ^ 2)
    e <- lm(e ~ x, data = tibble( e = rnorm(NUM_OBSERVATIONS), x )) %>% resid()
    y <- reactive({
        r2 <- input$r ^ 2
        error <- e * sqrt((1 - r2) / r2 * ssr / sum(e ^ 2))
        x + error
    }) %>% bindEvent(input$r)

    mode <- reactive({ # default mode is "none"
        mode <- getQueryString()$mode
        if (is.null(mode))
            mode = "none"
        mode
    })
    output$mode <- reactive(mode())

    help <- reactive(is.null(getQueryString()$mode))
    output$help <- reactive(help())
    output$css <- renderUI({
        tags$style(HTML("#p-value-panel {", htmltools::css( flex.direction = ifelse( mode() == "none", "column", "row" ))), "}")
    })

    # PLOT
    output$corPlot <- renderPlot({
        plot <- ggplot(tibble(x, y = y()), aes(x, y)) +
            geom_point() +
            coord_cartesian(xlim = LIMITS, ylim = LIMITS)

        ## Show linear fit only in mode "linear"
        # if (mode() != "none") {
        #      plot <- plot + geom_smooth(method = "lm", se = FALSE) # show linear fit, not confidence interval
        #  }

        ## UPDATE: show linear fit if tick chexbox in mode "linear"
        if (input$linear == TRUE) {
            plot <- plot + geom_smooth(method = "lm", se = FALSE) # show linear fit, not confidence interval
        }
        plot
    })
    #%>% bindCache(input$r)

    outputOptions(output, "mode", suspendWhenHidden = FALSE)
    outputOptions(output, "help", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
