# Karel Kroeze
# >> Write a small R shiny application using the miniUI package. The application should have a distribution plot of Student's t-distribution. Underneath the plot should be a row of widgets that allow the user to change the degrees of freedom, t-value, and p-value. The app should update the p-value whenever the user changes the degrees of freedom or t-value, and should update the t-value when the user changes the p-value. Make sure that updates are not chaining of eachother.
<<<<<<< Updated upstream
# 
=======
#
>>>>>>> Stashed changes
# Here is a code for the described R Shiny application using the miniUI package:

# bash
# Copy code
# library(shiny)
# library(miniUI)

# ui <- miniPage(
#   miniMainPanel(
#     plotOutput("t_plot"),
#     miniRow(
#       miniInput("df", "Degrees of Freedom", value = 3),
#       miniInput("t", "t-value", value = 0),
#       miniInput("p", "p-value", value = 0.5)
#     )
#   )
# )

# server <- function(input, output) {
<<<<<<< Updated upstream
  
#   output$t_plot <- renderPlot({
#     plot(dt(seq(-5,5,0.1), df = input$df), 
#          type = "l", 
#          xlab = "t", 
#          ylab = "density", 
#          main = paste("Student's t-distribution (df =", input$df, ")")
#     )
#   })
  
#   observeEvent(input$df, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })
  
#   observeEvent(input$t, {
#     updateNumericInput(session, "p", value = pt(input$t, df = input$df))
#   })
  
#   observeEvent(input$p, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })
  
=======

#   output$t_plot <- renderPlot({
#     plot(dt(seq(-5,5,0.1), df = input$df),
#          type = "l",
#          xlab = "t",
#          ylab = "density",
#          main = paste("Student's t-distribution (df =", input$df, ")")
#     )
#   })

#   observeEvent(input$df, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })

#   observeEvent(input$t, {
#     updateNumericInput(session, "p", value = pt(input$t, df = input$df))
#   })

#   observeEvent(input$p, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })

>>>>>>> Stashed changes
# }

# shinyApp(ui, server)
# This app should display a plot of the Student's t-distribution based on the degrees of freedom entered by the user, as well as update the t-value and p-value when either is changed by the user.




# Karel Kroeze
<<<<<<< Updated upstream
# >> change the miniInput functions for their shiny equivalents. 
=======
# >> change the miniInput functions for their shiny equivalents.
>>>>>>> Stashed changes
# Here is the updated code for the R Shiny application with the miniInput functions changed to their shiny equivalents:

# bash
# Copy code
# library(shiny)

# ui <- fluidPage(
#   mainPanel(
#     plotOutput("t_plot"),
#     fluidRow(
#       numericInput("df", "Degrees of Freedom", value = 3),
#       numericInput("t", "t-value", value = 0),
#       numericInput("p", "p-value", value = 0.5)
#     )
#   )
# )

# server <- function(input, output) {
<<<<<<< Updated upstream
  
#   output$t_plot <- renderPlot({
#     plot(dt(seq(-5,5,0.1), df = input$df), 
#          type = "l", 
#          xlab = "t", 
#          ylab = "density", 
#          main = paste("Student's t-distribution (df =", input$df, ")")
#     )
#   })
  
#   observeEvent(input$df, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })
  
#   observeEvent(input$t, {
#     updateNumericInput(session, "p", value = pt(input$t, df = input$df))
#   })
  
#   observeEvent(input$p, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })
  
=======

#   output$t_plot <- renderPlot({
#     plot(dt(seq(-5,5,0.1), df = input$df),
#          type = "l",
#          xlab = "t",
#          ylab = "density",
#          main = paste("Student's t-distribution (df =", input$df, ")")
#     )
#   })

#   observeEvent(input$df, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })

#   observeEvent(input$t, {
#     updateNumericInput(session, "p", value = pt(input$t, df = input$df))
#   })

#   observeEvent(input$p, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })

>>>>>>> Stashed changes
# }

# shinyApp(ui, server)



# Karel Kroeze
<<<<<<< Updated upstream
# >> add an extra toggle that allows the user to overlay the normal distribution on the plot. 
=======
# >> add an extra toggle that allows the user to overlay the normal distribution on the plot.
>>>>>>> Stashed changes
# Here is the updated code for the R Shiny application with an additional toggle that allows the user to overlay the normal distribution on the plot:

# bash
# Copy code
# library(shiny)

# ui <- fluidPage(
#   mainPanel(
#     plotOutput("t_plot"),
#     fluidRow(
#       numericInput("df", "Degrees of Freedom", value = 3),
#       numericInput("t", "t-value", value = 0),
#       numericInput("p", "p-value", value = 0.5),
#       toggleInput("overlay", "Overlay Normal Distribution", value = FALSE)
#     )
#   )
# )

# server <- function(input, output) {
<<<<<<< Updated upstream
  
#   output$t_plot <- renderPlot({
#     plot(dt(seq(-5,5,0.1), df = input$df), 
#          type = "l", 
#          xlab = "t", 
#          ylab = "density", 
=======

#   output$t_plot <- renderPlot({
#     plot(dt(seq(-5,5,0.1), df = input$df),
#          type = "l",
#          xlab = "t",
#          ylab = "density",
>>>>>>> Stashed changes
#          main = paste("Student's t-distribution (df =", input$df, ")")
#     )
#     if(input$overlay){
#       curve(dnorm(x), col = "red", add = TRUE)
#     }
#   })
<<<<<<< Updated upstream
  
#   observeEvent(input$df, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })
  
#   observeEvent(input$t, {
#     updateNumericInput(session, "p", value = pt(input$t, df = input$df))
#   })
  
#   observeEvent(input$p, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })
  
=======

#   observeEvent(input$df, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })

#   observeEvent(input$t, {
#     updateNumericInput(session, "p", value = pt(input$t, df = input$df))
#   })

#   observeEvent(input$p, {
#     updateNumericInput(session, "t", value = qt(input$p, df = input$df))
#   })

>>>>>>> Stashed changes
# }

# shinyApp(ui, server)



# Karel Kroeze
# >>use ggplot plotting functions instead of base R plotting functions.
# [NOTE: This is where ChatGPT gave up.]
# Here is the updated code for the R Shiny application using ggplot plotting functions instead of base R plotting functions:

# scss
# Copy code
# library(shiny)
# library(ggplot2)

# ui <- fluidPage(
#   mainPanel(
#     plotOutput("t_plot"),
#     fluidRow(
#       numericInput("df", "Degrees of Freedom", value = 3),
#       numericInput("t", "t-value", value = 0),
#       numericInput("p", "p-value", value = 0.5),
#       toggleInput("overlay", "Overlay Normal Distribution", value = FALSE)
#     )
#   )
# )

# server <- function(input, output) {
<<<<<<< Updated upstream
  
=======

>>>>>>> Stashed changes
#   output$t_plot <- renderPlot({
#     x <- seq(-5,5,0.1)
#     y <- dt(x, df = input$df)
#     ggplot(

############# result after some tuning
#' Basically workable, but missing some essential controls.
<<<<<<< Updated upstream
#' 
=======
#'
>>>>>>> Stashed changes
#' - no limits or sanity checking on inputs
#' - uses index as x-axis for t-distribution density
#' - does not adapt to lower or upper tailed t-test
#' - `toggleInput` is not actually a function (checkboxInput)
#' - session argument hasn't been declared in server function, but is used in update calls
#' - no way to stop cyclic updates of p <-> t-values
#' - use of basic plotting functions

library(shiny)

ui <- fluidPage(
  mainPanel(
    plotOutput("t_plot"),
    fluidRow(
      numericInput("df", "Degrees of Freedom", value = 3),
      numericInput("t", "t-value", value = 0),
      numericInput("p", "p-value", value = 0.5),
      checkboxInput("overlay", "Overlay Normal Distribution", value = FALSE)
    )
  )
)

server <- function(input, output, session) {
<<<<<<< Updated upstream
  
  output$t_plot <- renderPlot({
    plot(seq(-5,5,0.1), dt(seq(-5,5,0.1), df = input$df), 
         type = "l", 
         xlab = "t", 
         ylab = "density", 
=======

  output$t_plot <- renderPlot({
    plot(seq(-5,5,0.1), dt(seq(-5,5,0.1), df = input$df),
         type = "l",
         xlab = "t",
         ylab = "density",
>>>>>>> Stashed changes
         main = paste("Student's t-distribution (df =", input$df, ")")
    )
    if(input$overlay){
      curve(dnorm(x), col = "red", add = TRUE)
    }
  })
<<<<<<< Updated upstream
  
  observeEvent(input$df, {
    updateNumericInput(session, "t", value = qt(input$p, df = input$df))
  })
  
  observeEvent(input$t, {
    updateNumericInput(session, "p", value = pt(input$t, df = input$df))
  })
  
  observeEvent(input$p, {
    updateNumericInput(session, "t", value = qt(input$p, df = input$df))
  })
  
}

shinyApp(ui, server)
=======

  observeEvent(input$df, {
    updateNumericInput(session, "t", value = qt(input$p, df = input$df))
  })

  observeEvent(input$t, {
    updateNumericInput(session, "p", value = pt(input$t, df = input$df))
  })

  observeEvent(input$p, {
    updateNumericInput(session, "t", value = qt(input$p, df = input$df))
  })

}

shinyApp(ui, server)

>>>>>>> Stashed changes
