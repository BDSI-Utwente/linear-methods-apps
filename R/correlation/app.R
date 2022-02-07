require(tidyverse)
require(miniUI)
require(shiny)
require(shinyjs)
require(plotly)

# options(shiny.autoreload = TRUE)

INITIAL_R = .5
NUM_OBSERVATIONS = 300
SEED = 1234
LIMITS = c(-4, 4)

# based on https://stats.stackexchange.com/questions/522454/how-to-construct-simulate-data-that-will-have-a-given-coefficient-of-determina/522470#522470


# Define UI for application that draws a histogram
ui <- miniPage(
    useShinyjs(),
    miniContentPanel(plotOutput("corPlot", height = "100%")),
    miniButtonBlock(
        style = "justify-content: space-around; align-items: center; gap: 1em;",

        sliderInput("r",
                    "Correlation",
                    0,
                    1,
                    value = INITIAL_R,
                    0.01),

        checkboxInput("linear", "Show linear fit?", FALSE),
        checkboxInput("se", "Show uncertainty?", FALSE)

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

    observe({
        if (input$linear) {
            enable("se")
        } else {
            disable("se")
            updateCheckboxInput(inputId = "se", value = FALSE)
        }
    })

    output$corPlot <- renderPlot({
        plot <- ggplot(tibble(x, y = y()), aes(x, y)) +
            geom_point() +
            coord_cartesian(xlim = LIMITS, ylim = LIMITS)
        if (input$linear) {
            plot <- plot + geom_smooth(method = "lm", se = input$se)
        }
        plot # %>% plotly::ggplotly()
    }) %>% bindCache(input$r, input$linear, input$se)
}

# Run the application

shinyApp(ui = ui, server = server)
