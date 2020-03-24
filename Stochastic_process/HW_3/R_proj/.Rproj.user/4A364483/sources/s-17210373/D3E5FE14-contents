# Define UI for dataset viewer application
ui <- (pageWithSidebar(
  
  # Application title
  headerPanel("Input initial data"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    numericInput("Tt", "End of interval (T):", 10),
    numericInput("Hh", "Step (h):", 0.05),
    numericInput("N", "Number of trajectories:", 160),
    textInput("Mm", "Expected value:", "1 + exp(-t)"),
    textInput("covar", "Autocovariance function:", "2*exp(-abs(t2 - t1)/2)"),
    numericInput("t_1", "First section:", 1),
    numericInput("t_2", "Second section:", 2)
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    #h3(textOutput("caption")),
    verbatimTextOutput("a")
    #dataTableOutput("b"),
    #textOutput("plot11"))
  )
))