library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Введите исходные данные"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view
  sidebarPanel(
    numericInput("Tt", "Конец интервала (T):", 10),
    numericInput("Hh", "Шаг (h):", 0.05),
    numericInput("N", "Количество траекторий:", 160),
    textInput("Mm", "Математическое ожидание:", "1 + exp(-t)"),
    textInput("covar", "Автоковариационная функция:", "2*exp(-abs(t_2 - t_1)/2)"),
    textInput("t_1", "Первое сечение:", "1"),
    textInput("t_2", "Второе сечение:", "2"),
    numericInput("seed", "Set seed:", 1337)
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations
  mainPanel(
    textOutput("t1"),
    verbatimTextOutput("output_tmp")
  )
))