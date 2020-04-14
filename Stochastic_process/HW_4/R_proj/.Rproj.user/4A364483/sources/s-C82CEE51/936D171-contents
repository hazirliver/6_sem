library(htmlwidgets)
library(plotly)
library(data.table)
library(dplyr)

rm(tmp)
tmp <- pairs.list[[70]] %>% 
  data.table %>% 
  plot_ly(x = ~V1, y = ~V2, type = "scatter", mode = "lines+markers",
          name = "h = 0.02")
tmp <- tmp %>% add_trace(x = ~pairs.extnd.list[[70]][seq(2,N,2),1], 
                         y = ~pairs.extnd.list[[70]][seq(2,N,2),2], 
                         mode = "markers", color = I("lightgreen"),
                         name = "interm. points")
tmp <- tmp %>% add_trace(x = ~pairs.extnd.list[[70]][1:N,1], 
                         y = ~pairs.extnd.list[[70]][1:N,2], 
                         mode = "lines", name = "h = 0.01")
tmp

#####################################################
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


traj.1 <- data.frame(number = 0:(N/2)+1,
                     Xx = pairs.list[[70]][,1],
                     Yy = pairs.list[[70]][,2])


traj.1


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
  frame = (N/2)+1, 
  transition = 0, 
  redraw = FALSE
)

fig <- fig %>% animation_slider(
  hide = T
)

###############################################
###############################################
###############################################
###############################################
traj.1 <- data.frame(Xx = pairs.list[[70]][,1],
                               Yy = pairs.list[[70]][,2])
traj.2 <- data.frame(Xx = pairs.extnd.list[[70]][seq(2,N,2),1],
                               Yy = pairs.extnd.list[[70]][seq(2,N,2),2])

traj.1.acc <- traj.1 %>% cbind(number = 0:(N/2), h = rep("0.02"))
traj.2.acc <- traj.2 %>% cbind(number = 1:(N/2), h = rep("0.01"))

traj <- rbind(traj.1.acc, traj.2.acc)

traj.acc <- traj %>% accumulate_by(~number)

fig1 <- traj.acc %>% plot_ly(
  x = ~Xx,
  y = ~Yy,
  frame = ~frame,
  split = ~h,
  type = "scatter",
  mode = "lines+markers",
  line = list(simplify = F)
)

traj.1.acc <- traj.1.acc %>% accumulate_by(~number)
traj.2.acc <- traj.2.acc %>% accumulate_by(~number)


fig1 <- traj.1.acc %>% plot_ly(
  x = ~Xx,
  y = ~Yy,
  frame = ~frame,
  type = "scatter",
  mode = "lines+markers",
  line = list(simplify = F)
)

fig2 <- traj.2.acc %>% plot_ly(
  x = ~Xx,
  y = ~Yy,
  frame = ~frame,
  type = "scatter",
  mode = "lines+markers",
  line = list(simplify = F)
)

fig <- fig1 %>% add_trace(fig2)

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
  frame = N, 
  transition = 0, 
  redraw = FALSE
)

fig <- fig %>% animation_slider(
  hide = F
)



traj.1_2_tmp <- rbind(traj.1, traj.2)
traj.1_2 <- data.frame(number = c(1:(N/2), 1:(N/2)),
                       traj.1_2_tmp,
                       traj = c(rep("0.02",(N/2)),rep("0.01", (N/2))))

  

fig.1_2 <- traj.1_2 %>% accumulate_by(~number)






fig <- fig.1_2 %>% plot_ly(
  x = ~Xx,
  y = ~Yy,
  frame = ~frame,
  split = ~traj,
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
  frame = (3/2)*N+2, 
  transition = 0, 
  redraw = FALSE
)

fig <- fig %>% animation_slider(
  hide = T
)


fig




###############################################
###############################################
###############################################
###############################################



fig.2 <- sec_tr %>% accumulate_by(~number)



fig <- fig.2 %>% add_trace(x = ~Xx,
                         y = ~Yy , 
                         mode = "lines+markers", name = "h = 0.01")
fig
#######################################################
base <- traj.1 %>% plot_ly(x = ~Xx, y = ~Yy, 
                           type = "scatter", mode = "lines+markers")
base


base %>% 
  add_
###






















df <- txhousing 

fig <- df %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area"))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

str(fig)
fig <- fig %>% accumulate_by(~date)
str(fig)


fig <- fig %>%
  plot_ly(
    x = ~date, 
    y = ~median,
    split = ~city,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )
fig


fig %>%  ~date

head(fig[,!(colnames(fig) %in% "date")])

var <- fig[,(colnames(fig) %in% "date")]
dat <- fig[,!(colnames(fig) %in% "date")]

lazyeval::f_eval(~ var, dat)

  
fig <- fig %>% accumulate_by(~date)

head(fig)

tmp <- fig ~ data

fig1 <- accumulate_by(fig~date)

head(fig)
plotly:::getLevels(var)
