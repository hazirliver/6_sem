png(filename = "../img/2d_1_ext.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.extnd.list[[1]], type = "l", main = "Trajectory 1, h = 0.01", 
     xlab = "", ylab = "")
dev.off()


png(filename = "../img/2d_6_ext.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.extnd.list[[6]], type = "l", main = "Trajectory 6, h = 0.01", 
     xlab = "", ylab = "")
dev.off()

png(filename = "../img/2d_70_ext.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.extnd.list[[70]], type = "l", main = "Trajectory 70, h = 0.01", 
     xlab = "", ylab = "")
dev.off()

png(filename = "../img/2d_140_ext.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.extnd.list[[140]], type = "l", main = "Trajectory 140, h = 0.01", 
     xlab = "", ylab = "")
dev.off()


png(filename = "../img/2d_1_1.png",
    width = 1920, height = 1080,
    res = 96 * 2)
plot(pairs.list[[1]], type = "l", 
     main = "Trajectory 1, h = 0.02, h = 0.01", xlab = "", ylab = "",
     col = "#82c7ff")
lines(pairs.extnd.list[[1]], type = "l", col = "#ff82a1")
legend("topright", inset = 0.02, legend = c("h = 0.02", "h = 0.01"), 
       col = c("#82c7ff", "#ff82a1"), lwd = c(1,1))
dev.off()

png(filename = "../img/2d_1_1_reduced.png",
    width = 1920, height = 1080,
    res = 96 * 2.5)
plot(pairs.extnd.list[[1]][1:20,], type = "n", main = "Trajectory 1 reduced, h = 0.02, 0.01 with interm. points", 
     xlab = "", ylab = "")
lines(pairs.list[[1]][2:10,], type = "l", col = "#82c7ff",
      lwd = 2)
points(pairs.extnd.list[[1]][seq(4,19,2),], lwd = 1)
lines(pairs.extnd.list[[1]][3:19,], type = "l", col = "#ff82a1",
      lwd = 2)
legend("topleft", inset = 0.02, legend = c("h = 0.02", "h = 0.01", "interm. points"), 
       col = c("#82c7ff", "#ff82a1", "black"), lwd = c(2,2,1), lty = c(1,1,NA), pch = c(NA,NA,1))
dev.off()

##################

vars.extnd <- t(sapply(pairs.extnd.list, function(x) apply(x, 2, var_foo)))
head(vars.extnd,5)
tail(vars.extnd,5)

mean_vars.extnd <- colMeans(vars.extnd)
mean_vars.extnd



sq.vars.extnd  <- t(sapply(pairs.extnd.list, function(x) apply(x, 2, sq.var_foo)))
head(sq.vars.extnd ,5)
tail(sq.vars.extnd ,5)

mean_sq.vars.extnd  <- colMeans(sq.vars.extnd )
mean_sq.vars.extnd 

compare_table <- data.table(mean_vars = mean_vars,
                            mean_vars.extnd = mean_vars.extnd,
                            mean_sq.vars = mean_sq.vars,
                            mean_sq.vars.extnd = mean_sq.vars.extnd)
compare_table
compare_table.rel <- data.table(compare_table[,1] / compare_table[,2],
                                compare_table[,3] / compare_table[,4])
compare_table.rel
