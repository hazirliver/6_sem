

library(shiny)

# Define UI for application that draws a histogram
shinyUI((fluidPage(
  
  # Application title
  headerPanel("Input initial data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    numericInput("Tt", "End of interval (T):", 4),
    numericInput("h", "Step (h):", 0.02),
    numericInput("s", "Sigma:", 0.75),
    numericInput("z", "Radius:", 2.5)
  ),
  # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput(outputId = "p", height = "400px"),
      tabsetPanel(
        id = 'dataset',
        tabPanel("traj", DT::dataTableOutput("table1")),
        tabPanel("Vars", DT::dataTableOutput("table2")),
        tabPanel("sq_Vars", DT::dataTableOutput("table3"))
      )
    )
  )
))
