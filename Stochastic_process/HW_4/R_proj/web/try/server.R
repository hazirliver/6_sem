#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(rsconnect)
deployApp()
library(shiny)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session, ...) {
   
  output$p <- renderPlotly({
    Tt <- input$Tt
    h <- input$h
    sigma <- input$s
    z <- input$z

    ##
    N  <-  Tt/h
    pair  <-  t(replicate(N,
                          sapply(c(1,2), function(y) rnorm(1, 0, sqrt(h)*sigma)),
                          simplify = T))
    traj <- apply(rbind(c(0,0),pair), 2, cumsum)

    accumulate_by <- function(dat, var) {
      var <- lazyeval::f_eval(var, dat)
      lvls <- plotly:::getLevels(var)
      dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
      })
      dplyr::bind_rows(dats)
    }

    traj.1 <<- data.frame(number = 0:N+1,
                         Xx = traj[,1],
                         Yy = traj[,2])
    fig <- traj.1 %>% accumulate_by(~number)


    fig <- fig %>% plot_ly(
      x = ~Xx,
      y = ~Yy,
      frame = ~frame,
      type = "scatter",
      mode = "lines+markers",
      line = list(simplify = F)
    )

    fig <- fig %>%
      layout(
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = ""
        )
      )

    fig <- fig %>% animation_opts(
      frame = N+1,
      transition = 0,
      redraw = FALSE
    )

    fig <- fig %>% animation_slider(
      hide = F
    )

    fig
  })

  output$table1 <- DT::renderDataTable({
    DT::datatable(traj.1[,-1] )
  })
  
  
  output$table2 <- DT::renderDataTable({
    var_foo <- function(y)
    {
      sum(abs(diff(y)))
    }
    
    vars <- t(apply(traj.1[,-1], 2, var_foo))
    
    DT::datatable(vars)
  })
  
  output$table3 <- DT::renderDataTable({
    sq.var_foo <- function(y)
    {
      sum(abs(diff(y))^2)
    }
    sq.vars <- t(apply(traj.1[,-1], 2, sq.var_foo))
    
    DT::datatable(sq.vars)
  })
})

