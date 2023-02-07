# Application: Sampling distribution

# Notes:
# - how relevant is it to let students set the specifics of the distribution?
#   - we could remove the extra fields, making space in the two-plot solution
# - can we plot the results of drawing samples in the same plot?
#   - update for intermediate results (e.g. almost animated)
#   - use different colors for different sampling distributions (different sizes, number of samples)
#   - makes it clear that the sampling distribution both normalizes and narrows with more/larger samples (averages out)

library(shiny)
library(miniUI)
library(tidyverse)
library(glue)
library(devtools)
library(sn)

# devtools::source_gist("https://gist.github.com/anhtth16/68f2b0d746590273ce2ec5c773dad2a5")

options(shiny.usecairo = TRUE)
options(shiny.autoreload = TRUE)

DIST_OPTIONS = list("Normal" = 1,
                    "Uniform" = 2,
                    "Skewed" = 3)

# number of points for population distribution function
PRECISION = 1000

# magnitude for the alpha parameter of skewed normal distribution
# larger values give more extreme skew
SKEW_ALPHA_FACTOR = 5

ui <- miniPage(
    title = "Sampling Distributions",
    theme = bslib::bs_theme(version = 4),
    uiOutput("css"),
    withMathJax(),
    miniContentPanel(plotOutput("distPlot", height = "100%")),
    miniButtonBlock(
        div(
            tags$small("Population"),
            selectInput(
                "dist",
                "Distribution",
                DIST_OPTIONS,
                selectize = FALSE,
                width = "100%"
            ),
            # TODO: remove distribution parameters, set to sensible defaults
            div(
                conditionalPanel(
                    "input.dist != 2",
                    numericInput("mean", "Mean", 0, width = "100%"),
                    numericInput("sd", "Standard Deviation", 1, width = "100%"),
                    style = "display: flex; flex-flow: row nowrap; flex: 2 2 calc(100% * 2/3); gap: 0.5em;"
                ),
                conditionalPanel(
                    "input.dist == 3",
                    selectInput(
                        "skew",
                        "Skew",
                        c("Left" = 1, "Right" = 2),
                        selectize = FALSE,
                        width = "100%"
                    ),
                    style = "display: flex; flex-flow: row nowrap; flex: 1 1 calc(100% / 3); gap: 0.5em;"
                ),
                conditionalPanel(
                    "input.dist == 2",
                    numericInput("min", "Minimum", 0, width = "100%"),
                    numericInput("max", "Maximum", 1, width = "100%"),
                    style = "display: flex; flex-flow: row nowrap; flex: 1 1 auto; gap: 0.5em;"
                ),
                style = "display: flex; flex-flow: row nowrap; gap: 1em;"
            ),
            style = "flex: 1 1 50%;"
        ),
        div(
            tags$small("Draw Samples"),
            div(
                numericInput(
                    "sample_count",
                    "Samples",
                    value = 10,
                    min = 1,
                    max = 1000,
                    step = 1,
                    width = "100%"
                ),
                numericInput(
                    "sample_size",
                    "Sample size",
                    value = 10,
                    min = 1,
                    max = 1000,
                    step = 1,
                    width = "100%"
                ),
                actionButton("draw", "Draw!", width = "100%", style = "margin-top: 2em; margin-left: 0;"),
                style = "display: flex; flex-flow: row nowrap; gap: 0.5em; align-items: flex-start;"
            ),
            style = "flex: 1 1 50%;"
        ),
        style = "gap: 1em;"
    ),
)

server <- function(input, output, session) {
    # extra wrapper around min/max for uniform dist to make sure
    # min/max aren't swapped
    limits <- reactive({
        list(
            min = range(input$min, input$max)[1],
            max = range(input$min, input$max)[2]
        )
    })

    # wrapper around alpha parameter for skewed normal dist
    skew <- reactive({
        ifelse(input$skew == 1,
               SKEW_ALPHA_FACTOR * -1,
               SKEW_ALPHA_FACTOR)
    })

    population <- reactive({
        q <- seq(0, 1, length.out = PRECISION)

        if (input$dist == 1) {
            # normal
            x <- qnorm(q, input$mean, input$sd)
            d <- dnorm(x, input$mean, input$sd)
        } else if (input$dist == 2) {
            # uniform
            # extend range slightly to cover the limits
            range <- limits()$max - limits()$min

            x <- seq(# extend ~5% below and above
                limits()$min - range * 0.05,
                limits()$max + range * 0.05,

                # add more points to compensate
                length.out = PRECISION * 1.1)

            d <- dunif(x, limits()$min, limits()$max)
        }
        else if (input$dist == 3) {
            # skewed
            x <- qsn(q, input$mean, input$sd, skew())
            d <- dsn(x, input$mean, input$sd, skew())
        }

        tibble(x, d)
    })

    # wrapper to create an appropriate random draw function
    random <- reactive({
        if (input$dist == 1) {
            # normal
            f <- function(n)
                rnorm(n, input$mean, input$sd)
        } else if (input$dist == 2) {
            # uniform
            f <- function(n)
                runif(n, limits()$min, limits()$max)
        }
        else if (input$dist == 3) {
            # skewed
            f <- function(n)
                rsn(n, input$mean, input$sd, skew())
        }

        f
    })

    # reactive value to keep track of drawn samples so far
    samples <- reactiveVal({
        tibble(size = numeric(0),
               mean = numeric(0),
               .rows = 0)
    })

    # clear samples when population changes
    onPopulationChanged <- observe({
        samples(tibble(
            size = numeric(0),
            mean = numeric(0),
            .rows = 0
        ))
    }) %>% bindEvent(population())

    # draw samples
    onDraw <- observe({
        # TODO: see if there is a more elegant approach to drawing from a sampling distribution directly.
        means = replicate(input$sample_count,
                          random()(input$sample_size) %>% mean())

        samples(samples() %>% bind_rows(tibble(
            mean = means, size = input$sample_size
        )))

    }) %>% bindEvent(input$draw)

    output$distPlot <- renderPlot({

        plot <- ggplot(population()) +

            # population density plot
            geom_line(aes(x, d)) +

            # labels
            scale_y_continuous(name = "Population density") +

            # remove x axis label as it has no defined units
            scale_x_continuous(name = "") +

            # move the legend within the plot
            theme(
                legend.position = c(0.9, 0.9),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14)
            )

        if (samples() %>% nrow()) {
            sampling_counts <- samples() %>% dplyr::count(size)
            # TODO: normalize heights to sum to 1, instead of setting the max to 1

            # calculate the max height of sampling distribution graphs so that
            # we can normalize it to the same height as the population density,
            # and draw a secondary axis.
            # We first need to get the width of bars in the histogram to calculate
            # the number of samples in each bin.
            pop_limits <- population() %>% filter(is.finite(x)) %>% pull(x) %>% range()
            pop_range = pop_limits[2] - pop_limits[1]
            sampling_mean_counts <- samples() %>%
                mutate(mean = cut_width(mean, pop_range / 40, center = 0)) %>%
                dplyr::count(size, mean)

            # We can use the ratio of maximum heights of both layers to normalize
            # the heights, and as the transformation for the secondary axis.
            sec_axis_factor <- max(sampling_mean_counts$n) / max(population()$d)

            plot <- plot +
                # add secondary y-axis for sampling distribution counts
                scale_y_continuous(
                    name = "Population density",
                    sec.axis = sec_axis(~ . * sec_axis_factor, name = "Number of samples")) +

                # histograms for sampling distribution(s)
                geom_bar(
                    aes(
                        x = mean,
                        y = after_stat(count) / sec_axis_factor,
                        colour = size %>% factor(labels = unique(size)),
                        fill = size %>% factor(labels = unique(size)),
                        group = size
                    ),
                    data = samples(),
                    alpha = 0.3,
                    position = "identity",
                    stat = "bin",
                    bins = 40
                ) +

                # describe samples in legend
                scale_colour_discrete(
                    name = "Distribution of drawn samples",
                    labels = sampling_counts %>% glue_data("{n} samples of size {size}"),
                    aesthetics = c("colour", "fill")
                )
        }


        plot
    })
}

after_stat

# Run the application
shinyApp(ui = ui, server = server)
