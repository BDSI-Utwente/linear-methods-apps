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

# number of points for population distribution function
PRECISION = 1000

# number of bins for sampling distribution histograms
BINS = 40

# magnitude for the alpha parameter of skewed normal distribution
# larger values give more extreme skew
SKEW_ALPHA_FACTOR = 5

# distributions and their parameters
DISTRIBUTIONS = list(
    Normal = list(mean = 0,
                  sd = 1),
    Uniform = list(min = 0,
                   max = 10),
    Skewed = list(mean = 0,
                  sd = 1,
                  alpha = SKEW_ALPHA_FACTOR),
    Bimodal = list(
        mean = c(-2, 2),
        sd = c(1, 1),
        p = 0.5 # prop to use dist 1
    )
)

ui <- miniPage(
    title = "Sampling Distributions",
    theme = bslib::bs_theme(version = 4),
    # uiOutput("css"),
    shiny::withMathJax(),
    shinyjs::useShinyjs(),
    miniContentPanel(plotOutput("distPlot", height = "100%")),
    miniButtonBlock(
        div(
            tags$small("Population"),
            selectInput(
                "dist",
                "Distribution",
                DISTRIBUTIONS %>% names(),
                selectize = FALSE,
                width = "100%"
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
    population <- reactive({
        q <- seq(0, 1, length.out = PRECISION)

        if (input$dist == "Normal") {
            # normal
            x <-
                qnorm(q,
                      DISTRIBUTIONS$Normal$mean,
                      DISTRIBUTIONS$Normal$sd)
            d <-
                dnorm(x,
                      DISTRIBUTIONS$Normal$mean,
                      DISTRIBUTIONS$Normal$sd)
        } else if (input$dist == "Uniform") {
            # uniform
            # extend range slightly to cover the limits
            range <-
                DISTRIBUTIONS$Uniform$max - DISTRIBUTIONS$Uniform$min

            x <- seq(
                # extend ~5% below and above
                DISTRIBUTIONS$Uniform$min - range * 0.05,
                DISTRIBUTIONS$Uniform$max + range * 0.05,

                # add more points to compensate
                length.out = PRECISION * 1.1
            )

            d <-
                dunif(x,
                      DISTRIBUTIONS$Uniform$min,
                      DISTRIBUTIONS$Uniform$max)
        }
        else if (input$dist == "Skewed") {
            # skewed
            x <-
                qsn(
                    q,
                    DISTRIBUTIONS$Skewed$mean,
                    DISTRIBUTIONS$Skewed$sd,
                    DISTRIBUTIONS$Skewed$alpha
                )
            d <-
                dsn(
                    x,
                    DISTRIBUTIONS$Skewed$mean,
                    DISTRIBUTIONS$Skewed$sd,
                    DISTRIBUTIONS$Skewed$alpha
                )
        }
        else if (input$dist == "Bimodal") {
            limits = range(
                qnorm(
                    q,
                    DISTRIBUTIONS$Bimodal$mean[1],
                    DISTRIBUTIONS$Bimodal$sd[1]
                ),
                qnorm(
                    q,
                    DISTRIBUTIONS$Bimodal$mean[2],
                    DISTRIBUTIONS$Bimodal$sd[2]
                ),
                finite = TRUE
            )
            x <- seq(limits[1], limits[2], length.out = PRECISION)
            d <-
                dnorm(x,
                      DISTRIBUTIONS$Bimodal$mean[1],
                      DISTRIBUTIONS$Bimodal$sd[1]) *
                DISTRIBUTIONS$Bimodal$p +
                dnorm(x,
                      DISTRIBUTIONS$Bimodal$mean[2],
                      DISTRIBUTIONS$Bimodal$sd[2]) *
                (1 - DISTRIBUTIONS$Bimodal$p)
        }

        tibble(x, d)
    })

    # wrapper to create an appropriate random draw function
    random <- reactive({
        if (input$dist == "Normal") {
            # normal
            f <- function(n)
                rnorm(n,
                      DISTRIBUTIONS$Normal$mean,
                      DISTRIBUTIONS$Normal$sd)
        } else if (input$dist == "Uniform") {
            # uniform
            f <- function(n)
                runif(n,
                      DISTRIBUTIONS$Uniform$min,
                      DISTRIBUTIONS$Uniform$max)
        }
        else if (input$dist == "Skewed") {
            # skewed
            f <- function(n)
                rsn(
                    n,
                    DISTRIBUTIONS$Skewed$mean,
                    DISTRIBUTIONS$Skewed$sd,
                    DISTRIBUTIONS$Skewed$alpha
                )
        }
        else if (input$dist == "Bimodal") {
            # binormal - for each point we first draw a distribution
            f <- function(n)
                if_else(
                    runif(n) <= DISTRIBUTIONS$Bimodal$p,
                    rnorm(n, DISTRIBUTIONS$Bimodal$mean[1], DISTRIBUTIONS$Bimodal$sd[1]),
                    rnorm(n, DISTRIBUTIONS$Bimodal$mean[2], DISTRIBUTIONS$Bimodal$sd[2])
                )
        }
        else {
            stop("unknown distribution: ", input$dist)
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
        # disable draw button while we are updating
        # TODO: we could render a subtle progress bar while drawing new samples
        shinyjs::disable("draw")

        # TODO: see if there is a more elegant approach to drawing from a sampling distribution directly.
        means = replicate(input$sample_count,
                          random()(input$sample_size) %>% mean())

        samples(samples() %>% bind_rows(tibble(
            mean = means, size = input$sample_size
        )))

        # re-enable after we're done
        shinyjs::enable("draw")

    }) %>% bindEvent(input$draw)

    output$distPlot <- renderPlot({
        plot <- ggplot(population()) +

            # population density plot
            geom_line(aes(x, d)) +

            # remove x axis label as it has no defined units
            scale_x_continuous(name = "") +

            # add label for y axis
            scale_y_continuous(name = "Population density") +

            # move the legend within the plot
            theme(
                legend.position = c(0.9, 0.9),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14)
            )


        if (samples() %>% nrow()) {
            # summarize samples for label
            sampling_groups <- samples() %>% count(size)

            # TODO: try to draw secondary axis for sample counts in observed
            # sampling distributions?
            # I've already wasted a day on this, syncing counts with density so
            # that both the count and relative density remains correct has proven
            # a bitch to figure out (i.e. what f to use so that f(y1) = y2).

            plot <- plot +
                # histograms for sampling distribution(s)
                geom_bar(
                    aes(
                        x = mean,
                        # y = after_stat(count) / sec_axis_factor,
                        y = after_stat(density),
                        colour = size %>% factor(levels = unique(size)),
                        fill = size %>% factor(levels = unique(size)),
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
                    name = "Observed sampling distributions",
                    labels = sampling_groups %>% glue_data("{n} samples of size {size}"),
                    aesthetics = c("colour", "fill")
                )
        }

        plot
    }) %>% bindEvent(population(), samples())
}

# Run the application
shinyApp(ui = ui, server = server)
