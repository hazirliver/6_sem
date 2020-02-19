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

df[df$`Бутылка колы (0.5)` < 40,1] <- 40
df[df$`Плитка шоколада (100 гр)` < 40,2] <- 40
df[df$`Чашка кофе (0.3)` < 20,3] <- 20
df[df$`Часы наручные` < 3000,4] <- 3000
df[df$Электросамокат < 5000,5] <- 5000
df[df$`Крем для рук` < 50,6] <- 50
dim(df)

## Оптовые цены
opt1 <- 5
opt2 <- 10
opt3 <- 20
opt4 <- 30
opt5 <- 50
#
my_dat <- df$`Плитка шоколада (100 гр)`
my_dat[my_dat >= 300] <- 300
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



ggplot(stat_table, aes(x = stat_table$p_i))+
  geom_line(aes(y = opt_1), color = "#03A82F") +
  geom_line(aes(y = opt_2), color = "#07728C") +
  geom_line(aes(y = opt_3), color = "#E17204") +
  geom_line(aes(y = opt_4), color = "#E11E04") +
  geom_line(aes(y = opt_5), color = "#429DC7")

plot(x = stat_table$p_i, y = stat_table$D_i)

tmp$count
cumsum(tmp$count)






