server <- function(input, output, session) 
{
  main_func <- function(Tt, Hh, N, Mm_text, covar_text, t1, t2)
  {
    Mm <<- eval(parse(text = paste('Mm <- function(t) { return(' , Mm_text , ')}', sep='')))
    covar <<- eval(parse(text = paste('covar <- function(t1,t2) { return(' , covar_text , ')}', sep='')))
    ### Single path simulation function
    trajectories <- function(Tt, Hh)
    {
      Nn <- Tt/Hh + 1
      vect_rep <- repeatable(rnorm)
      vect <- vect_rep(Nn)
      vect_expected <- Mm(vect)
      Sigma_mat <- outer(vect,vect,FUN = covar)
      eps_rep <- repeatable(rnorm)
      eps <- eps_rep(Nn)
      L <- t(chol(Sigma_mat))
      eta <- as.numeric(L %*% eps)
      ksi <- vect + eta
      return(ksi)
    }
    
    ### replicate
    traj_list <- replicate(N, trajectories(Tt, Hh), simplify = F)
    
    ### Construction of sections with calculations corr, UB, LB
    selected_cut <- as.data.frame(matrix(c(sapply(traj_list, `[[`, t1), sapply(traj_list, `[[`, t2)),
                                         ncol = 2, byrow = F))
    scatter_plot <- ggplot(selected_cut, aes(x = V1, y = V2)) + geom_point()
    cor_test <- cor.test(selected_cut$V1, selected_cut$V2)
    
    output_tmp_f <- list(matr = selected_cut, scatter = scatter_plot,
                         corr = cor_test)
    
    ### Output Formation
    corr <- as.numeric(output_tmp_f$corr$estimate)
    corr_LB <- output_tmp_f$corr$conf.int[1]
    corr_UB <- output_tmp_f$corr$conf.int[2]
    
    output_main <- c(corr, corr_LB, corr_UB,selected_cut)
    
    return(output_main)
  }
  
  
  Tt <<- reactive(input$Tt)
  Hh <<- reactive(input$Hh)
  N <<- reactive(input$N)

  Mm <<- reactive(input$Mm)
  covar <<- reactive(input$covar)
  
  t1 <<- reactive(input$t_1)
  t2 <<- reactive(input$t_2)
  
  #npt_seed <<- reactive((as.numeric(input$seed)))
  
  #a <- main_func(Tt = 10,Hh = 0.05,N = 160, Mm_text = Mm, covar_text = covar, t1 = 1,t2 = 2)
  
  #out_data_table <- data.table(rbind(c("r", "r_l", "r_u"),))
  
  cutt <- reactive(main_func(Tt = input$Tt,
                             Hh = input$Hh,
                             N = input$N,
                             Mm_text = input$Mm,
                             covar_text = input$covar,
                             t1 = input$t_1,
                             t2 = input$t_2)[4])

  #output$plot11 <- reactive(asd)
  
  output$a <- reactive(main_func(Tt = input$Tt,
                 Hh = input$Hh,
                 N = input$N,
                 Mm_text = input$Mm,
                 covar_text = input$covar,
                 t1 = input$t_1,
                 t2 = input$t_2)[-4])
  
  
}
