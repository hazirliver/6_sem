#1##############
#library(MASS)
library(xtable)
alpha <- 0.05
n <- 140
mu <- c(1,-3)
Sigma <- matrix(c(1,-1,-1,2), byrow = T, ncol = 2)
set.seed(5009)
x <- rnorm(n,mu[1], sqrt(Sigma[1,1]))
r <- Sigma[1,2] / sqrt(Sigma[1,1]*Sigma[2,2])
eps_k <- rnorm(n,mean = 0, sqrt(Sigma[2,2]*(1-r^2)))
y <- mu[2] + Sigma[1,2]/Sigma[1,1] * (x-mu[1]) + eps_k
mean(y)
mean(x)
cor(x,y)
df <- matrix(c(x,y,eps_k), ncol = 3)
colnames(df) <- c("x","y","eps")
summary(df)
var(df[,1]) 
var(df[,2])
var(df[,3])

#df <- mvrnorm(n,mu,Sigma, empirical = F)

tab <- xtable(df,align=rep("|c|", ncol(df)+1), digits = 5)
print(tab,hline.after=seq(from=-1,to=nrow(tab),by=1))

num_bins <- length(hist(df[,2], breaks = "Sturges", freq = T)$breaks)
x_c <- cut(df[,1], num_bins)
y_c <- cut(df[,2], num_bins)
z <- table(x_c, y_c)
library(plot3D)
png(filename = "../img/2d_1.png",
    width = 1920, height = 1080,
    pointsize = 24, res = 96 * 1.25)
main_perspective1 <- hist3D(x = seq(from = floor(min(df[,1])), 
                                    ceiling(max(df[,1])), 
                                    length.out = nrow(z)),
                            y = seq(from = floor(min(df[,2])), 
                                    ceiling(max(df[,2])), 
                                    length.out = nrow(z)), 
                            z=z, border="black", 
                            ticktype = "detailed", lighting=T, 
                            bty = "g", phi = 30, theta = 40, 
                            lphi = 50)
dev.off()


png(filename = "../img/2d_2.png",
    width = 1920, height = 1080,
    pointsize = 24, res = 96 * 1.25)
main_perspective2 <- hist3D(x = seq(from = floor(min(df[,1])), 
                                    ceiling(max(df[,1])), 
                                    length.out = nrow(z)),
                            y = seq(from = floor(min(df[,2])), 
                                    ceiling(max(df[,2])), 
                                    length.out = nrow(z)),
                            z=z, border="black", 
                            ticktype = "detailed", lighting=T, 
                            bty = "g", phi = 30, theta = 80, 
                            lphi = 50)
dev.off()

png(filename = "../img/2d_3.png",
    width = 1920, height = 1080,
    pointsize = 24, res = 96 * 1.25)
main_perspective3 <- hist3D(x = seq(from = floor(min(df[,1])), 
                                    ceiling(max(df[,1])), 
                                    length.out = nrow(z)),
                            y = seq(from = floor(min(df[,2])), 
                                    ceiling(max(df[,2])), 
                                    length.out = nrow(z)),
                            z=z, border="black", 
                            ticktype = "detailed", lighting=T, 
                            bty = "g", phi = 30, theta = 120, 
                            lphi = 50)
dev.off()


png(filename = "../img/2d_4.png",
    width = 1920, height = 1080,
    pointsize = 24, res = 96 * 1.25)
main_perspective4 <- hist3D(x = seq(from = floor(min(df[,1])), 
                                    ceiling(max(df[,1])), 
                                    length.out = nrow(z)),
                            y = seq(from = floor(min(df[,2])), 
                                    ceiling(max(df[,2])), 
                                    length.out = nrow(z)),
                            z=z, border="black", 
                            ticktype = "detailed", lighting=T, 
                            bty = "g", phi = 30, theta = 160, 
                            lphi = 50)
dev.off()
# hist3D(x = seq(from = floor(min(df[,1])), ceiling(max(df[,1])), length.out = nrow(z)),
#                             y = seq(from = floor(min(df[,2])), ceiling(max(df[,2])), length.out = nrow(z)),
#                             z=z, border="black", ticktype = "detailed", lighting=T, 
#                             bty = "g", phi = 0, theta = 0, lphi = 50)
# 
# hist3D(x = seq(from = min(df[,1]), max(df[,1]), length.out = nrow(z)),
#        y = seq(from = min(df[,2]), max(df[,2]), length.out = nrow(z)),
#        z=z, border="black", ticktype = "detailed", lighting=T, 
#        bty = "g", phi = 0, theta = 180, lphi = 50, d = 20)
png(filename = "../img/1d_without_line.png",
    width = 1920, height = 1080,
    pointsize = 24, res = 96 * 1.25)
plot(df, lwd = 2, col = "black")
dev.off()
#2#############
meanx <- mean(df[,1])
meany <- mean(df[,2])

beta_1 <- (n*sum(df[,1]*df[,2]) - 
             sum(df[,1])*sum(df[,2]))/
          (n*sum((df[,1])^2) - (sum(df[,1]))^2)
beta_0 <- sum(df[,2])/n - beta_1*sum(df[,1])/n

beta_1 <- (sum((df[,2] - meany)*(df[,1] - meanx)))/
  (sum((df[,1] - meanx)^2))
beta_1
beta_0  <- meany - beta_1 * meanx
beta_0

p <- 1
sigma_ost <- (sum((df[,2] - beta_1 * 
                     df[,1] - beta_0)^2))/(n-2)
delta_ost <- (sum((df[,2] - beta_1 * 
                     df[,1] - beta_0)^2))/(n-2)
sigma_ost
lm1 <- lm(df[,2] ~ df[,1])
summary(lm1)
png(filename = "../img/1d_line.png",
    width = 1920, height = 1080,
    pointsize = 24, res = 96 * 1.25)
plot(df, lwd = 2, col = "black")
abline(lm1, lwd = 3, col = "purple")
dev.off()


#3#############
#Sigma[1,2] / (sqrt(Sigma[1,1] * Sigma[2,2]))
#r <- cor(df)[1,2]
#rnorm(1,mean = 0, sqrt(Sigma[2,2]*(1-r^2)))

library(psychometric)

mu20 <- 1/n * sum((df[,1] - meanx)^2)
mu20
mu02 <- 1/n * sum((df[,2] - meany)^2)
mu02
mu11 <- 1/n * sum((df[,1] - meanx)*(df[,2] - meany))
mu11

#CIr(r, n, 0.95)

rv <- mu11/sqrt(mu20*mu02)
rv

CIr_lower_bound <- tanh(atanh(rv) - rv/(2*(n-1)) 
                        - qnorm(1-alpha/2)/sqrt(n-3))
CIr_lower_bound
CIr_upper_bound <- tanh(atanh(rv) - rv/(2*(n-1)) 
                        + qnorm(1-alpha/2)/sqrt(n-3))
CIr_upper_bound

confint(lm1)
confint(lm1, 1, level = 1-alpha)
confint(lm1, 2, level = 1-alpha)

#delta - izvestno

#CIbeta0_lower_bound <- beta_0 - (qnorm(1-alpha/2) * sqrt(sigma_ost) * sqrt(sum(df[,1]^2)))/sqrt(n*sum((df[,1] - mean(df[,1]))^2))
#CIbeta0_upper_bound <- beta_0 + (qnorm(1-alpha/2) * sqrt(sigma_ost) * sqrt(sum(df[,1]^2)))/sqrt(n*sum((df[,1] - mean(df[,1]))^2))


#CIbeta1_lower_bound <-  beta_1 - (qnorm(1-alpha/2) * sqrt(sigma_ost))/sqrt(sum((df[,1] - mean(df[,1]))^2))
#CIbeta1_upper_bound <-  beta_1 + (qnorm(1-alpha/2) * sqrt(sigma_ost))/sqrt(sum((df[,1] - mean(df[,1]))^2))

#delta - neizvestno

CIbeta0_lower_bound <- beta_0 - (qt(1-alpha/2, n-2) * 
                                   sqrt(sigma_ost) * 
                                   sqrt(sum(df[,1]^2)))/
                                    sqrt(n*sum((df[,1] - 
                                                  mean(df[,1]))^2))
CIbeta0_lower_bound
CIbeta0_upper_bound <- beta_0 + (qt(1-alpha/2, n-2) * 
                                   sqrt(sigma_ost) * 
                                   sqrt(sum(df[,1]^2)))/
                                    sqrt(n*sum((df[,1] -
                                                  mean(df[,1]))^2))
CIbeta0_upper_bound

CIbeta1_lower_bound <-  beta_1 - (qt(1-alpha/2, n-2) * 
                                    sqrt(sigma_ost))/
                  sqrt(sum((df[,1] - mean(df[,1]))^2))
CIbeta1_lower_bound
CIbeta1_upper_bound <-  beta_1 + (qt(1-alpha/2, n-2) *
                                    sqrt(sigma_ost))/
                  sqrt(sum((df[,1] - mean(df[,1]))^2))
CIbeta1_upper_bound

confint(lm1)

CIdelta_ost_upper_boud<- (n-2)*delta_ost / 
  (qchisq(alpha/2, n-2))
CIdelta_ost_upper_boud
CIdelta_ost_lower_boud <- (n-2)*delta_ost / 
  (qchisq(1-alpha/2, n-2))
CIdelta_ost_lower_boud
delta_ost



M <- matrix(c(n, sum(df[,1]), 
              sum(df[,1]),sum((df[,1])^2)), 
            ncol = 2, byrow = T)
M_obr <- solve(M)



CIbeta0_lower_bound <- beta_0 - 
  qt(1-alpha/2, n-2) * sqrt(sigma_ost) * sqrt(M_obr[1,1])
CIbeta0_lower_bound
CIbeta0_upper_bound <- beta_0 + 
  qt(1-alpha/2, n-2) * sqrt(sigma_ost) * sqrt(M_obr[1,1])
CIbeta0_upper_bound


CIbeta1_lower_bound <-  beta_1 - 
  (qt(1-alpha/2, n-2) * sqrt(sigma_ost)) * sqrt(M_obr[2,2])
CIbeta1_lower_bound
CIbeta1_upper_bound <-  beta_1 + 
  (qt(1-alpha/2, n-2) * sqrt(sigma_ost)) * sqrt(M_obr[2,2])
CIbeta1_upper_bound
