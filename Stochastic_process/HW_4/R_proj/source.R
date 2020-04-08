library("rgl") # plot3d
library("circlize") # random colors
library("psych")
library("data.table")
library("zoo")
library("scales")
addTaskCallback(function(...) {set.seed(1337);TRUE})
### Начальные данны
Tt  <-  4
n  <-  160
sigma  <-  0.75
h  <-  0.02
z  <-  2.5
#####
N  <-  Tt/h
N

# Формирование одной траектории
pair.func <- function() 
{
  pair  <-  t(replicate(N,
              sapply(c(1,2), function(y) rnorm(1, 0, h * sigma^2)),
              simplify = T))
  return(apply(rbind(c(0,0),pair), 2, cumsum))
}

tmp <- pair.func()
head(tmp,4)
tail(tmp,4)
###1

pairs.list <- replicate(n, pair.func(), F)
head(pairs.list[[6]],4)
tail(pairs.list[[6]],4)



###2

open3d()
plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[1]][,2],
       type = "l", xlab = "Time", ylab = "", zlab = "", 
       main = "Trajectory 1")
rgl.snapshot("../img/3d_1.png","png")

open3d()
plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[6]][,2],
       type = "l", xlab = "Time", ylab = "", zlab = "", 
       main = "Trajectory 6")
rgl.snapshot("../img/3d_6.png","png")

open3d()
plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[70]][,2],
       type = "l", xlab = "Time", ylab = "", zlab = "", 
       main = "Trajectory 70")
rgl.snapshot("../img/3d_70.png","png")

open3d()
plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[140]][,2],
       type = "l", xlab = "Time", ylab = "", zlab = "", 
       main = "Trajectory 140")
rgl.snapshot("../img/3d_140.png","png")



png(filename = "../img/2d_1.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[1]], type = "l", main = "Trajectory 1, h = 0.02", 
     xaxt='n', yaxt='n', xlab = "", ylab = "")
dev.off()


png(filename = "../img/2d_6.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[6]], type = "l", main = "Trajectory 6, h = 0.02", 
     xaxt='n', yaxt='n', xlab = "", ylab = "")
dev.off()

png(filename = "../img/2d_70.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[70]], type = "l", main = "Trajectory 70, h = 0.02", 
     xaxt='n', yaxt='n', xlab = "", ylab = "")
dev.off()

png(filename = "../img/2d_140.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[140]], type = "l", main = "Trajectory 140, h = 0.02", 
     xaxt='n', yaxt='n', xlab = "", ylab = "")
dev.off()


###3




##

sc_x <- rescale(rollapply(pairs.list[[1]][,1], 2, by = 2, sum),
                to = c(min(pairs.list[[1]][,1]), max(pairs.list[[1]][,2])))
sc_y <- rescale(rollapply(pairs.list[[1]][,2], 2, by = 2, sum),
                to = c(min(pairs.list[[1]][,2]), max(pairs.list[[1]][,2])))

plot(sc_x,sc_y, type = "l",
     main = "h = 0.02", xaxt='n', yaxt='n', xlab = "", ylab = "")


plot(rollapply(pairs.list[[1]], 2, by = 2, sum), type = "l",
     main = "h = 0.02", xaxt='n', yaxt='n', xlab = "", ylab = "")
axis(1, at = seq(-0.02,0.16,0.02))
axis(2, at = seq(-0.10,0.25,0.05))

plot(pairs.list[[1]], type = "l", main = "h = 0.01", 
     xaxt='n', yaxt='n', xlab = "", ylab = "")
axis(1, at = seq(-0.02,0.16,0.02))
axis(2, at = seq(-0.10,0.25,0.05))

lines(pairs.list[[1]], type = "l")
lines(rollapply(pairs.list[[1]], 2, by = 2, sum), type = "l")

lines(pairs.list[[1]][seq(1,400,2),], type = "l", col = "lightblue")
lines(pairs.list[[1]][seq(2,400,2),], type = "l", col = "red")

##

###2

plot.brownian <- function(nplots = 6)
{
  min.x <- min(unlist(lapply(pairs.list, function(x) min(x[,1]))[1:nplots]))
  max.x <- max(unlist(lapply(pairs.list, function(x) max(x[,1]))[1:nplots]))
  
  min.y <- min(unlist(lapply(pairs.list, function(x) min(x[,2]))[1:nplots]))
  max.y <- max(unlist(lapply(pairs.list, function(x) max(x[,2]))[1:nplots]))
  plot(pairs.list[[1]], type = "l", 
               xlim = c(min.x, max.x),
               ylim = c(min.y,max.y),
               xlab = "",
               ylab = "",
               main = c("Brownian motion for", as.character(nplots),
                        "trajectories"))
  for (i in 2:nplots)
  {
    lines(pairs.list[[i]], col = rand_color(nplots,luminosity =  "bright"))
  }
}

plot.brownian()


plot.trajectories <- function(nplots = 2)
{
  plot3d(x = 0:(N), y = pairs.list[[1]][,1], z = pairs.list[[1]][,2],
         type = "l", col = rand_color(nplots,luminosity =  "bright"))
}

plot.trajectories()

###3

tmp1 <- sum(abs(diff(pairs.list[[160]][,2])))
tmp2 <- sum(abs(diff(pairs.list[[1]][,2])))

var_foo <- function(y)
{
  sum(abs(diff(y)))
}

vars <- t(sapply(pairs.list, function(x) apply(x, 2, var_foo)))
head(vars,5)
tail(vars,5)

mean_vars <- colMeans(vars)
mean_vars


sq.var_foo <- function(y)
{
  sum(abs(diff(y))^2)
}

sq.vars <- t(sapply(pairs.list, function(x) apply(x, 2, sq.var_foo)))
head(sq.vars,5)
tail(sq.vars,5)

mean_sq.vars <- colMeans(vars)
mean_sq.vars
#############




