#install.packages("readxl")
library(readxl)
df <- as.data.frame((read_xlsx("g_form.xlsx"))[,-1])
head(df)

# Корректируем записи, в которых:
# Кола < 40
# Шокол < 40
# Кофе < 10
# Часы < 3000
# Самок < 5000
# Крем < 50

#df[df$`Бутылка колы (0.5)` < 40,1] <- 40
df[df$`Плитка шоколада (100 гр)` < 40,2] <- 40
#df[df$`Чашка кофе (0.3)` < 20,3] <- 20
#df[df$`Часы наручные` < 3000,4] <- 3000
#df[df$Электросамокат < 5000,5] <- 5000
#df[df$`Крем для рук` < 50,6] <- 50
dim(df)

df[df$`Плитка шоколада (100 гр)` < 40,2] <- 40
my_dat <- df$`Плитка шоколада (100 гр)`
my_dat[my_dat >= 300] <- 300

hist(my_dat, xlab = "Максимальная стоимость, руб.", ylab = "Количество человек")

## Оптовые цены
opt1 <- 5
opt2 <- 10
opt3 <- 20
opt4 <- 30
opt5 <- 50
#

tmp <- aggregate(data.frame(count = my_dat), list(value = my_dat), length)

stat_table_tmp <- data.frame(
  p_i = tmp$value,
  f_i = tmp$count,
  D_i = rev(cumsum(rev(tmp$count)))
)

opt_1 <- (stat_table_tmp$p_i-opt1) * stat_table_tmp$D_i
opt_2 <- (stat_table_tmp$p_i-opt2) * stat_table_tmp$D_i
opt_3 <- (stat_table_tmp$p_i-opt3) * stat_table_tmp$D_i
opt_4 <- (stat_table_tmp$p_i-opt4) * stat_table_tmp$D_i
opt_5 <- (stat_table_tmp$p_i-opt5) * stat_table_tmp$D_i


stat_table <- data.frame(stat_table_tmp,
                         opt_1,opt_2,opt_3,opt_4,opt_5)



stat_table$opt_5[stat_table$opt_5 <= 0] <- NA

lapply(stat_table[,4:8], max,na.rm = T)

max_pl <- max(range(stat_table[,4:8], na.rm = T))
min_pl <- min(range(stat_table[,4:8], na.rm = T))


library("ggplot2")
ggplot(stat_table, aes(x = p_i))+
  geom_line(aes(y = opt_1), color = "#03A82F") +
  geom_line(aes(y = opt_2), color = "#07728C") +
  geom_line(aes(y = opt_3), color = "#E17204") +
  geom_line(aes(y = opt_4), color = "#E11E04") +
  geom_line(aes(y = opt_5), color = "#429DC7")

png(filename = "../img/1.png",
    width = 1920, height = 1080,
    res = 96 * 3)
plot(x = stat_table$p_i, y = stat_table$D_i, "o",
     xlab = "Максимальная стоимость, руб.",
     ylab = "Спрос, чел.")
dev.off()


#############################################
n <- stat_table$D_i[1]

pi <- stat_table$p_i
Ni <- stat_table$f_i
piNi <- pi * Ni
D_pi <- stat_table$D_i
D_pi_Ni <- D_pi * Ni
pi_2_Ni <- pi^2 * Ni
D_pi_pi_Ni <- D_pi_Ni * pi

prep <- data.frame(  pi = pi,Ni = Ni,piNi = piNi, 
                     D_pi = D_pi, D_pi_Ni = D_pi_Ni,
                     pi_2_Ni = pi_2_Ni,
                     D_pi_pi_Ni = D_pi_pi_Ni)

write.csv(prep, file = "tab1.xlsx")
a_zv <- ( sum(D_pi_pi_Ni) - ( (1/n) * sum(piNi) * sum(D_pi_Ni)) )/
  ( sum(pi_2_Ni) - n * (sum(piNi)/n)^2)

b_zv <- sum(D_pi_Ni)/n

d_zv <- b_zv - a_zv * sum(piNi)/n

Dzv_pi <- function(p)
{
  return(a_zv * p + d_zv)
}

D_zv_pi <- Dzv_pi(pi)
predposl <- Ni * (D_pi - D_zv_pi)
posl <- Ni * (D_pi - D_zv_pi)^2
ss <- sum(posl)
sigma_zv <- sqrt(ss/n)

tab1 <- cbind(prep, D_zv_pi, predposl, posl)
write.csv(tab1, file = "tab1.csv")
D_dov_verh <- function(p)
{
  a_zv * p + d_zv + 1.96 * sigma_zv *
    sqrt(1/n + ((p-mean(piNi))^2)/
           (sum(pi_2_Ni) - n * (mean(piNi))^2))
}
D_dov_nizh<- function(p)
{
  a_zv * p + d_zv - 1.96 * sigma_zv *
    sqrt(1/n + ((p-mean(piNi))^2)/
           (sum(pi_2_Ni) - n * (mean(piNi))^2))
}

D_dov_verh(100)
D_dov_nizh(100)

p_opt <- function(p)
{
  return(p/2 + 80.83)
}

p_opt(opt1)
p_opt(opt2)
p_opt(opt3)
p_opt(opt4)
p_opt(opt5)


prep$D_pi_2 <- prep$D_pi
plot(D_pi_2 ~ pi, prep, type = "o", xlab = "pi", ylab = "D(pi)")
abline(lm(D_pi ~ pi, prep), col = "green", lwd = 2)









pi_log <- log(stat_table$p_i)
Ni_log <- stat_table$f_i
piNi_log <- pi_log * Ni_log 
D_pi_log <- log(stat_table$D_i)
D_pi_Ni_log <- D_pi_log * Ni_log 
pi_2_Ni_log <- pi_log^2 * Ni_log 
D_pi_pi_Ni_log <- D_pi_Ni_log * pi_log

prep_log <- data.frame(  pi_log = pi_log,Ni_log = Ni_log,piNi_log = piNi_log, 
                     D_pi_log = D_pi_log, D_pi_Ni_log = D_pi_Ni_log,
                     pi_2_Ni_log = pi_2_Ni_log,
                     D_pi_pi_Ni_log = D_pi_pi_Ni_log)


a_zv_log <- ( sum(D_pi_pi_Ni_log) - ( (1/n) * sum(piNi_log) * sum(D_pi_Ni_log)) )/
  ( sum(pi_2_Ni_log) - n * (sum(piNi_log)/n)^2)


b_zv_log <- sum(D_pi_Ni_log)/n

d_zv_log <- b_zv_log - a_zv_log * sum(piNi_log)/n


d_s_sayta <- exp(1/n * sum(D_pi_Ni_log) -
                   a_zv_log/n * sum(piNi_log))


Dzv_pi_log <- function(p)
{
  return(d_zv_log * p^(a_zv_log ))
}

tmp1 <- Dzv_pi_log(pi_log)

tmp_foo <- function(p)
{
  return(d_s_sayta * p^(a_zv_log))
}

yy <- tmp_foo(pi)

plot(D_pi ~ pi, type = "o")
lines(yy ~ pi, col = "red", lwd = 2)
abline(lm(D_pi ~ pi, prep), col = "green", lwd = 2)


gav <- function(p)
{
  return(-(1.128076)/(1-1.128076)*p)
}

gav(opt1)
gav(opt2)
gav(opt3)
gav(opt4)
gav(opt5)






