### Начальные данные:
m <- 5 # Число состояний марковской цепи
k <- 7 # время (шаги)
n <- 120 # траектории


#### Моделирование цепи маркова с m состояниями
# 1. Генерируем (m+1) раз вектор r=(r_1, ..., r_{m-1}) из независимых 
#             и равномерно распределенных на отрезке
#             [0;1] случайных величин

r_tmp <- replicate((m+1), runif((m-1), min = 0, max = 1), simplify = F)

# 2. Для каждого из полученный векторов строим вариационный ряд, то есть упорядочиваем по возрастанию

r <- lapply(r_tmp, sort)
lapply(r, round,3)
# 3. Находим длины отрезков, на которые вектор r разбивает отрезок [0;1] --
#             получаем вектор вероятностей p

p_tmp <- lapply(r, diff)

heads <- lapply(r, head, 1)
tails <- lapply(r, function(x) (1-tail(x,1)))

p <- mapply(append, mapply(append, heads,p_tmp,SIMPLIFY = F),
            tails, SIMPLIFY = F)
lapply(p, round, 3)

mapply(sum, p)


# 4. Первый из полученных векторов p считаем вектором начальных вероятностей, 
#             из остальных составляем матрицу переходов P,
#             записывая их по строкам

p0 <- p[[1]] # вектор начальных условий
P <- t(simplify2array(p))[-1,] # матрица переходов
round(P,3)

##### Построение размеченного графа состояний цепи
#install.packages("diagram")
#install.packages("markovchain")
#library(markovchain)
library(diagram)


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



#### Вычисление безусловных вероятностей состояний цепи на k шаге
#install.packages("matrixcalc")
library(matrixcalc)

#
p0_stroka <- matrix(p0, nrow = 1)
p_k_stroka <- p0_stroka %*% matrix.power(P,k)

#

p_k <- p0 %*% matrix.power(P,k)


p_k2 <- p0 %*% matrix.power(P,2)
p_k3 <- p0 %*% matrix.power(P,3)
p_k4 <- p0 %*% matrix.power(P,4)
p_k5 <- p0 %*% matrix.power(P,5)
p_k6 <- p0 %*% matrix.power(P,6)
p_k8 <- p0 %*% matrix.power(P,8)
round(p_k,3)
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
    ifelse(r0_loc < r[[j+1]][4],4,5))))
  }
  
  step_1 <- foo(r0,0)
  #print(c("r0", r0,step_1))
  
  r1 <- runif(1, min = 0, max = 1)
  step_2 <- foo(r1,step_1)
  #print(c("r1",r1,step_2))
  
  r2 <- runif(1, min = 0, max = 1)
  step_3 <- foo(r2,step_2)
  #print(c("r2",r2,step_3))
  
  r3 <- runif(1, min = 0, max = 1)
  step_4 <- foo(r3,step_3)
  #print(c("r3",r3,step_4))
  
  r4 <- runif(1, min = 0, max = 1)
  step_5 <- foo(r4,step_4)
  #print(c("r4",r4,step_5))
  
  r5 <- runif(1, min = 0, max = 1)
  step_6 <- foo(r5,step_5)
  #print(c("r5",r5,step_6))
  
  r6 <- runif(1, min = 0, max = 1)
  step_7 <- foo(r6,step_6)
  #print(c("r6",r6, step_7))
  
  
  trac <- list(c(step_1,step_2,step_3,step_4,step_5,step_6,step_7))
  tracs[i] <- trac
}

tracs_array <- t(simplify2array(tracs,higher = F))
colnames(tracs_array) <- paste("Шаг",as.character(1:k))
rownames(tracs_array) <- paste("Тр.",as.character(1:n))



### Вычисление эмпирических вероятностей (относительных частот) 
#         состояний  цепи на k шаге.



library(ggplot2)





emp <- hist(tracs_array[,k], breaks =0:m)$density
theor <- as.numeric(p_k)


plot_df <- data.frame(type = rep(c("theoretical", "emperical"), each=m),
                      sostoyanie = rep(paste("S",as.character(1:m),sep = "_"), 2),
                      prob = c(theor, emp))

ggplot(data=plot_df, aes(x=sostoyanie, y=prob, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw()



prob_diff <- emp - theor
prob_diff
max(abs(prob_diff))




###Вычисление финальных вероятностей для Марковской цепи из задачи 1 
#       и сравнение их с вероятностями состояний на k шаге
#install.packages("matlib")
library(matlib)
b <- c(rep(0,m-1),1)



showEqn(signif(rbind((P - diag(m))[-m,],rep(1,m)),3), b, 
        vars = paste("p", as.character(1:m),sep = ""))

maat <- rbind((P - diag(m))[-m,],rep(1,m))

ress <- matrix(solve(maat,b,nrow = 1))


P %*% ress
max(abs(ress - p_k))



#
b_stroka <- matrix(c(rep(0,m-1),1), nrow = 1)
ress %*% P

