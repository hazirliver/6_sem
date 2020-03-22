library(shiny)
library(ggplot2)
library(data.table)
library(V8)
source("./shinyapp/ui.R")
source("./shinyapp/server.R")

#install.packages("V8")

shinyApp(ui = ui, server = server)
#runApp("./shinyapp")

library(rsconnect)
Sys.setlocale(locale="en_US.UTF-8")
rsconnect::setAccountInfo(name='hazirliver',token='2E064CFD3CB4A9E25774670094962F12',secret='5XXgH3XjPW8Iq1r5SADIHd0hd1G/4HptlaZQX/fk')
rsconnect::deployApp('./shinyapp')
