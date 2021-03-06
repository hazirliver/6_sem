### Начальные данные:
m <- 6 # Число состояний марковской цепи
k <- 5 # время (шаги)
n <- 180 # траектории


#### Моделирование цепи маркова с m состояниями
# 1. Генерируем (m+1) раз вектор r=(r_1, ..., r_{m-1}) из независимых 
#             и равномерно распределенных на отрезке
#             [0;1] случайных величин

r_tmp <- replicate((m+1), runif((m-1), min = 0, max = 1), simplify = F)
r_tmp
# 2. Для каждого из полученный векторов строим вариационный ряд, то есть упорядочиваем по возрастанию

r <- lapply(r_tmp, sort)
r
# 3. Находим длины отрезков, на которые вектор r разбивает отрезок [0;1] --
#             получаем вектор вероятностей p

p_tmp <- lapply(r, diff)

heads <- lapply(r, head, 1)
tails <- lapply(r, function(x) (1-tail(x,1)))

p <- mapply(append, mapply(append, heads,p_tmp,SIMPLIFY = F),
            tails, SIMPLIFY = F)
p

mapply(sum, p)


# 4. Первый из полученных векторов p считаем вектором начальных вероятностей, 
#             из остальных составляем матрицу переходов P,
#             записывая их по строкам

p0 <- p[[1]] # вектор начальных условий
p0
P <- t(simplify2array(p))[-1,] # матрица переходов
P

##### Построение размеченного графа состояний цепи
#install.packages("diagram")
#install.packages("markovchain")
#library(markovchain)
library(diagram)

png(filename = "../img/1.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
plotmat(signif(t(P),3), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.04, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.25,
        arr.width=.1,
        self.cex = .7,
        self.shifty = -.01,
        self.shiftx = .07,
        main = "Markov Chain")
dev.off()


#### Вычисление безусловных вероятностей состояний цепи на k шаге
#install.packages("matrixcalc")
library(matrixcalc)

p_k <- p0 %*% matrix.power(P,k)
p_k

p_k2 <- p0 %*% matrix.power(P,2)
p_k2
p_k3 <- p0 %*% matrix.power(P,3)
p_k3
p_k4 <- p0 %*% matrix.power(P,4)
p_k4
p_k5 <- p0 %*% matrix.power(P,5)
p_k5
p_k10 <- p0 %*% matrix.power(P,10)
p_k10

#### Моделирование траектории длины k цепи маркова
#1. Генерируем равномерно распределенную на [0;1] случайную величину r0 и по вектору p0
#             разыгрываем начальное состояние

tracs <- list()

for (i in 1:n)
{
  r0 <- runif(1, min = 0, max = 1)
  foo <- function(r0_loc,j)
  {
    ifelse(r0_loc < r[[j+1]][1],1,
    ifelse(r0_loc < r[[j+1]][2],2,
    ifelse(r0_loc < r[[j+1]][3],3,
    ifelse(r0_loc < r[[j+1]][4],4,
    ifelse(r0_loc < r[[j+1]][5],5,6)))))
  }
  
  step_1 <- foo(r0,0)
  print(c("r0", r0,step_1))
  
  r1 <- runif(1, min = 0, max = 1)
  step_2 <- foo(r1,step_1)
  print(c("r1",r1,step_2))
  
  r2 <- runif(1, min = 0, max = 1)
  step_3 <- foo(r2,step_2)
  print(c("r2",r2,step_3))
  
  r3 <- runif(1, min = 0, max = 1)
  step_4 <- foo(r3,step_3)
  print(c("r3",r3,step_4))
  
  r4 <- runif(1, min = 0, max = 1)
  step_5 <- foo(r4,step_4)
  print(c("r4",r4,step_5))
  
  trac <- list(c(step_1,step_2,step_3,step_4,step_5))
  tracs[i] <- trac
}

tracs_array <- t(simplify2array(tracs,higher = F))
colnames(tracs_array) <- paste("Шаг",as.character(1:k))
rownames(tracs_array) <- paste("Тр.",as.character(1:n))
tracs_array
head(tracs_array,7)
tail(tracs_array,7)
### Вычисление эмпирических вероятностей (относительных частот) 
#         состояний  цепи на k шаге.



library(ggplot2)
hist(tracs_array[,k], breaks =0:m)$counts
emp <- hist(tracs_array[,k], breaks =0:m)$density
emp
theor <- as.numeric(p_k)


plot_df <- data.frame(type = rep(c("theoretical", "emperical"), each=m),
                      state = rep(paste("S",as.character(1:m),sep = "_"), 2),
                      prob = c(theor, emp))

png(filename = "../img/2.png",
    width = 1920, height = 1080,
    res = 96 * 2)
ggplot(data=plot_df, aes(x=state, y=prob, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw()
dev.off()

emp
theor
prob_diff <- emp - theor
prob_diff
max(abs(prob_diff))




###Вычисление финальных вероятностей для Марковской цепи из задачи 1 
#       и сравнение их с вероятностями состояний на k шаге
#install.packages("matlib")
library(matlib)
b <- c(rep(0,m-1),1)
b
maat <- rbind((t(P) - diag(m))[-m,],rep(1,m))
maat
res <- solve(maat,b)
res


showEqn(signif(maat,3), b, 
        vars = paste("pi", as.character(1:m),sep = ""), latex = T)

res
as.numeric(p_k)
res - as.numeric(p_k)
max(abs(res-as.numeric(p_k)))
