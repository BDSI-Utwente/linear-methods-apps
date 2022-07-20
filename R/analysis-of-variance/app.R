# Application: ANOVA
# Update at: 20/07/2022

require(shiny)
require(plotly)
require(tidyverse)
require(shinyjs)
require(glue)
# source("../style.R") # update style from local script
# Update: load style from public gist
if (!("devtools" %in% installed.packages()[, 1])) {
    install.packages("devtools")
}
devtools::source_gist("https://gist.github.com/anhtth16/68f2b0d746590273ce2ec5c773dad2a5")


defaults <- list(
    group1 = list(
        mean = 20,
        variance = 2,
        size = 20,
        rnorm = repeatable(rnorm)
    ),
    group2 = list(
        mean = 16,
        size = 25,
        variance = 2.5,
        rnorm = repeatable(rnorm)
    ),
    group3 = list(
        mean = 18,
        variance = 1.5,
        size = 12,
        rnorm = repeatable(rnorm)
    ),
    mean = list(min = 10,
                max = 30,
                step = 1)
    ,
    variance = list(min = 0.5,
                    max = 5,
                    step = 0.5),
    size = list(min = 1,
                max = 50,
                step = 1)
)

ui <- fillPage(useShinyjs(),
               fillCol(
                   plotOutput("boxPlot", height = "100%"),
                   fillRow(
                       tags$div(
                           tags$h3("Group 1"),
                           sliderInput(
                               "mean_1",
                               "Mean",
                               defaults$mean$min,
                               defaults$mean$max,
                               defaults$group1$mean,
                               defaults$mean$step,
                               width = "100%"
                           ),
                           sliderInput(
                               "var_1",
                               "Variance",
                               defaults$variance$min,
                               defaults$variance$max,
                               defaults$group1$variance,
                               defaults$variance$step,
                               width = "100%"
                           ),
                           sliderInput(
                               "size_1",
                               "Size",
                               defaults$size$min,
                               defaults$size$max,
                               defaults$group1$size,
                               defaults$size$step,
                               width = "100%"
                           ),
                       ),
                       tags$div(
                           tags$h3("Group 2"),
                           sliderInput(
                               "mean_2",
                               "Mean",
                               defaults$mean$min,
                               defaults$mean$max,
                               defaults$group2$mean,
                               defaults$mean$step,
                               width = "100%"
                           ),
                           sliderInput(
                               "var_2",
                               "Variance",
                               defaults$variance$min,
                               defaults$variance$max,
                               defaults$group2$variance,
                               defaults$variance$step,
                               width = "100%"
                           ),
                           sliderInput(
                               "size_2",
                               "Size",
                               defaults$size$min,
                               defaults$size$max,
                               defaults$group2$size,
                               defaults$size$step,
                               width = "100%"
                           ),
                       ),
                       tags$div(
                           tags$h3("Group 3"),
                           sliderInput(
                               "mean_3",
                               "Mean",
                               defaults$mean$min,
                               defaults$mean$max,
                               defaults$group3$mean,
                               defaults$mean$step,
                               width = "100%"
                           ),
                           sliderInput(
                               "var_3",
                               "Variance",
                               defaults$variance$min,
                               defaults$variance$max,
                               defaults$group3$variance,
                               defaults$variance$step,
                               width = "100%"
                           ),
                           sliderInput(
                               "size_3",
                               "Size",
                               defaults$size$min,
                               defaults$size$max,
                               defaults$group3$size,
                               defaults$size$step,
                               width = "100%"
                           ),
                       ),
                       style = "gap: 1em;"
                   ),
                   style = "gap: 1em;"
               ),
               padding = "1em")

server <- function(input, output) {
    data <- reactiveVal(bind_rows(
        tibble(
            group = "Group 1",
            value = defaults$group1$rnorm(
                defaults$group1$size,
                defaults$group1$mean,
                defaults$group1$variance
            )
        ),
        tibble(
            group = "Group 2",
            value = defaults$group2$rnorm(
                defaults$group2$size,
                defaults$group2$mean,
                defaults$group2$variance
            )
        ),
        tibble(
            group = "Group 3",
            value = defaults$group3$rnorm(
                defaults$group3$size,
                defaults$group3$mean,
                defaults$group3$variance
            )
        ),
    ))

    observe({
        data(bind_rows(
            tibble(
                group = "Group 1",
                value = defaults$group1$rnorm(input$size_1, input$mean_1, input$var_1)
            ),
            tibble(
                group = "Group 2",
                value = defaults$group2$rnorm(input$size_2, input$mean_2, input$var_2)
            ),
            tibble(
                group = "Group 3",
                value = defaults$group3$rnorm(input$size_3, input$mean_3, input$var_3)
            ),
        ))
    })

    output$boxPlot <- renderPlot({
        ggplot(data(), aes(
            x = value,
            y = group,
            colour = group
        )) +
            geom_boxplot() + geom_point()
        #ggplotly()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
