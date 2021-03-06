library("rgl") # plot3d
library("circlize") # random colors
library("psych")
library("data.table")
library("zoo")
library("plotrix")
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
                        sapply(c(1,2), function(y) rnorm(1, 0, sqrt(h)*sigma)),
                        simplify = T))
  return(apply(rbind(c(0,0),pair), 2, cumsum))
}


###1
pairs.extnd.list <- replicate(n, pair.func(), F)
# tmpr <- replicate(n, rbind(c(0,0),
#                         rollapply(pair.func(), 2, by = 2, sum)), F)
# pairs.list <- Map(function(x, y) scales::rescale(x, to = range(y)),
#                   tmpr, pairs.extnd.list)

pairs.list <- Map(function(x) x[c(T,F),], pairs.extnd.list)


head(pairs.list[[1]], 4)

head(pairs.list[[6]],4)
tail(pairs.list[[6]],4)



###2

# open3d()
# plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[1]][,2],
#        type = "l", xlab = "Time", ylab = "", zlab = "", 
#        main = "Trajectory 1")
# rgl.snapshot("../img/3d_1.png","png")
# 
# open3d()
# plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[6]][,2],
#        type = "l", xlab = "Time", ylab = "", zlab = "", 
#        main = "Trajectory 6")
# rgl.snapshot("../img/3d_6.png","png")
# 
# open3d()
# plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[70]][,2],
#        type = "l", xlab = "Time", ylab = "", zlab = "", 
#        main = "Trajectory 70")
# rgl.snapshot("../img/3d_70.png","png")
# 
# open3d()
# plot3d(x = seq(0,Tt,h), y = pairs.list[[1]][,1], z = pairs.list[[140]][,2],
#        type = "l", xlab = "Time", ylab = "", zlab = "", 
#        main = "Trajectory 140")
# rgl.snapshot("../img/3d_140.png","png")



png(filename = "../img/2d_1.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[1]], type = "l", main = "Trajectory 1, h = 0.02", 
     xlab = "", ylab = "")
dev.off()


png(filename = "../img/2d_6.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[6]], type = "l", main = "Trajectory 6, h = 0.02", 
    xlab = "", ylab = "")
dev.off()

png(filename = "../img/2d_70.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[70]], type = "l", main = "Trajectory 70, h = 0.02", 
     xlab = "", ylab = "")
dev.off()

png(filename = "../img/2d_140.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[140]], type = "l", main = "Trajectory 140, h = 0.02", 
     xlab = "", ylab = "")
dev.off()


###3

var_foo <- function(y)
{
  sum(abs(diff(y)))
}

vars <- t(sapply(pairs.list, function(x) apply(x, 2, var_foo)))
head(vars,5)
tail(vars,5)

mean_vars <- colMeans(vars)
mean_vars

#

sq.var_foo <- function(y)
{
  sum(abs(diff(y))^2)
}

sq.vars <- t(sapply(pairs.list, function(x) apply(x, 2, sq.var_foo)))
head(sq.vars,5)
tail(sq.vars,5)

mean_sq.vars <- colMeans(sq.vars)
mean_sq.vars


###4


source("./h_dvd_2.R", echo = T)


###5

euc_metric <- function(y)
{
  sqrt(sum(y[dim(y)[1],]^2))
}

z.sc <- as.numeric(Map(euc_metric, pairs.list))
head(z.sc,5)
tail(z.sc,5)
pos.sc <- length(z_sc[z_sc > z])
pos.sc
z.prob <- pos.sc/((N/2)+1)
z.prob


z.theor <- 1 - pchisq(z^2/(sigma^2 * Tt),2)
z.theor

png(filename = "../img/2d_1_circle.png",
    width = 1920, height = 1080,
    res = 96 * 2)
MASS::eqscplot(pairs.list[[1]], type = "l", main = "Trajectory 1, h = 0.02, z = 2.5", 
     xlab = "", ylab = "", xlim = c(-z-0.5, z+0.5), ylim = c(-z-0.5,z+0.5), asp = 1,
     lwd = 2)
points(pairs.list[[1]][(N/2)+1,1], pairs.list[[1]][(N/2)+1,2],
       lwd = 2, col = "green", pch = 16)
draw.circle(0,0,2.5, nv = 1000, lwd = 2, border = "red")
legend("topright", inset = 0.02, c("traj", "z = 2.5", "at time T"), 
       lty = c(1,1,NA), lwd = c(2,2,2), pch = c(NA,NA,16), col= c("black", "red", "green"))
dev.off()




png(filename = "../img/2d_6_circle.png",
    width = 1920, height = 1080,
    res = 96 * 2)
MASS::eqscplot(pairs.list[[6]], type = "l", main = "Trajectory 6, h = 0.02, z = 2.5", 
               xlab = "", ylab = "", xlim = c(-z-0.5, z+0.5), ylim = c(-z-0.5,z+0.5), asp = 1,
               lwd = 2)
points(pairs.list[[6]][(N/2)+1,1], pairs.list[[6]][(N/2)+1,2],
       lwd = 2, col = "green", pch = 16)
draw.circle(0,0,2.5, nv = 1000, lwd = 2, border = "red")
legend("topright", inset = 0.02, c("traj", "z = 2.5", "at time T"), 
       lty = c(1,1,NA), lwd = c(2,2,2), pch = c(NA,NA,16), col= c("black", "red", "green"))
dev.off()




png(filename = "../img/2d_70_circle.png",
    width = 1920, height = 1080,
    res = 96 * 2)
MASS::eqscplot(pairs.list[[70]], type = "l", main = "Trajectory 70, h = 0.02, z = 2.5", 
               xlab = "", ylab = "", xlim = c(-z-0.5, z+0.5), ylim = c(-z-0.5,z+0.5), asp = 1,
               lwd = 2)
points(pairs.list[[70]][(N/2)+1,1], pairs.list[[70]][(N/2)+1,2],
       lwd = 2, col = "green", pch = 16)
draw.circle(0,0,2.5, nv = 1000, lwd = 2, border = "red")
legend("topright", inset = 0.02, c("traj", "z = 2.5", "at time T"), 
       lty = c(1,1,NA), lwd = c(2,2,2), pch = c(NA,NA,16), col= c("black", "red", "green"))
dev.off()




png(filename = "../img/2d_140_circle.png",
    width = 1920, height = 1080,
    res = 96 * 2)
MASS::eqscplot(pairs.list[[140]], type = "l", main = "Trajectory 140, h = 0.02, z = 2.5", 
               xlab = "", ylab = "", xlim = c(-z-0.5, z+0.5), ylim = c(-z-0.5,z+0.5), asp = 1,
               lwd = 2)
points(pairs.list[[140]][(N/2)+1,1], pairs.list[[140]][(N/2)+1,2],
       lwd = 2, col = "green", pch = 16)
draw.circle(0,0,2.5, nv = 1000, lwd = 2, border = "red")
legend("topright", inset = 0.02, c("traj", "z = 2.5", "at time T"), 
       lty = c(1,1,NA), lwd = c(2,2,2), pch = c(NA,NA,16), col= c("black", "red", "green"))
dev.off()


#END
##############################
#














###############################################################################

euc_metric(pairs.list[[160]][201,])

pairs.list[[160]][201,]

t(sapply(pairs.list, function(x) apply(x, 1, euc_metric)))

sapply(pairs.list, euc_metric)
pairs.list[[160]]



plot(pairs.list[[6]], type = "l", main = "Trajectory 6, h = 0.02", 
     xlab = "", ylab = "", xlim = c(-.06, .06), ylim = c(-.06,.06))
draw.circle(0,0,0.025)

##################################

























#################################################

plot(pairs.list[[1]], type = "l")
points(pairs.list[[70]], cex = 0.5)
lines(pairs.extnd.list[[1]], type = "l", col = "blue")


t(sapply(pairs.extnd.list, function(x) apply(x, 2, min))) 
t(sapply(pairs.list, function(x) apply(x, 2, min)))

sapply(pairs.extnd.list, range)


###

stck.list <- list(pairs.list[[1]][1:5,], pairs.list[[2]][1:5,])
stck.list.2 <- list(pairs.extnd.list[[1]][1:5,], pairs.extnd.list[[2]][1:5,])

sc_x.1 <- scales::rescale(stck.list[[1]][,1], to = range(stck.list.2[[1]][,1]))
sc_y.1 <- scales::rescale(stck.list[[1]][,2], to = range(stck.list.2[[1]][,2]))


tmp <- Map(function(x, y) scales::rescale(x, to = range(y)) , pairs.extnd.list, pairs.list)

plot(tmp[[1]],type = "l")
plot(pairs.list[[1]], type = "l")

plot(sc_x.1, sc_y.1, type = "l", xaxt='n', yaxt='n', xlab = "", ylab = "")
axis(1, at = seq(-0.01,0.1,0.004))
axis(2, at = seq(-0.01,0.1,0.004))

plot(stck.list.2[[1]], type = "l", xaxt='n', yaxt='n', xlab = "", ylab = "")
axis(1, at = seq(-0.01,0.1,0.004))
axis(2, at = seq(-0.01,0.1,0.004))

scales::rescale(stck.list[[1]], to = range(stck.list.2[[1]]))
stck.list[[1]]

plot(stck.list.2[[1]], type = "l")

plot(scales::rescale(stck.list[[1]], to = range(stck.list.2[[1]])), type = "l")
plot(stck.list[[1]], type = "l")

simplify2array(pairs.extnd.list)

scales::rescale(simplify2array(pairs.extnd.list),
                to = sapply(pairs.extnd.list, range))


##

sc_x <- scales::rescale(rollapply(pairs.list[[1]][,1], 2, by = 2, sum), to = c(min(pairs.list[[1]][,1]), max(pairs.list[[1]][,1])))
sc_y <- scales::rescale(rollapply(pairs.list[[1]][,2], 2, by = 2, sum),
                to = c(min(pairs.list[[1]][,2]), max(pairs.list[[1]][,2])))

plot(sc_x,sc_y, type = "l",
     main = "h = 0.02", xaxt='n', yaxt='n', xlab = "", ylab = "")
axis(1, at = seq(-0.02,0.16,0.02))
axis(2, at = seq(-0.10,0.25,0.05))

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


#############




