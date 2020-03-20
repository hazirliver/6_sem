library(shiny)
library(datasets)

# We tweak the "am" field to have nicer factor labels. Since this doesn't
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) 
{
  library(ggplot2)
  
  Tt <<- reactive(input$Tt)
  Hh <<- reactive(input$Hh)
  N <<- reactive(input$N)
  
  Mm <<- as.function(alist(t = , reactive(input$Mm)))
  covar <<- as.function(alist(t_1 = , t_2 =,  reactive(input$covar)))
  
  t1 <<- reactive(input$t_1)
  t2 <<- reactive(input$t_2)
  
  reactive(set.seed(input$seed))
  
  
  output$output_tmp <- observe(main_func(Tt, Hh, N, t1, t2)[1])
  #output$t1 <- t1
  #output$t2 <- t2
  #output$cor <- reactive(main_func(Tt, Hh, N, t1, t2)[1])
  #output$cor_LB <- reactive(main_func(Tt, Hh, N, t1, t2)[2])
  #output$cor_UB <- reactive(main_func(Tt, Hh, N, t1, t2)[3])
  

  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  
  
})