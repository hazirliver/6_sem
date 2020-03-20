library(ggplot2)
main_func <- function(Tt, Hh, N, t1, t2)
{
  ### Функция моделирования одной траектории
  trajectories <- function(Tt, Hh)
  {
    Nn <- reactive(Tt/Hh + 1)
    vect <- reactive(runif(Nn))
    vect_expected <- reactive(Mm(vect))
    Sigma_mat <- reactive(outer(vect,vect,FUN = covar))
    eps <- reactive(runif(Nn))
    L <- reactive(t(chol(Sigma_mat)))
    eta <- reactive(as.numeric(L %*% eps))
    ksi <- reactive(vect + eta)
    return(ksi)
  }
  
  
  ### Реплецирование
  traj_list <- reactive(replicate(N, trajectories(Tt, Hh), simplify = F))
  
  ### Построение сечений с рассчетами corr, UB, LB
  selected_cut <- reactive(as.data.frame(matrix(c(sapply(traj_list, `[[`, t1), sapply(traj_list, `[[`, t2)),
                                                ncol = 2, byrow = F)))
  scatter_plot <- reactive(ggplot(selected_cut, aes(x = V1, y = V2)) + geom_point())
  cor_test <- reactive(cor.test(selected_cut$V1, selected_cut$V2))
  
  output <- reactive(list(matr = selected_cut, scatter = scatter_plot,
                          corr = cor_test))
  
  ### Формирование аутпута
  corr <- reactive(as.numeric(output$corr$estimate))
  corr_LB <- reactive(output$corr$conf.int[1])
  corr_UB <- reactive(output$corr$conf.int[2])
  
  output_main <- reactive(c(corr, corr_LB, corr_UB))
  
  return(output_main)
}