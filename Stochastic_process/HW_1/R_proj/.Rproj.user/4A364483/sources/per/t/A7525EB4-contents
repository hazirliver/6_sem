### Начальные данные:
m <- 4 # Число состояний марковской цепи
k <- 8 # время (шаги)
n <- 100 # траектории


#### Моделирование цепи маркова с m состояниями
# 1. Генерируем (m+1) раз вектор r=(r_1, ..., r_{m-1}) из независимых 
#             и равномерно распределенных на отрезке
#             [0;1] случайных велечин

r_tmp <- replicate((m+1), runif((m-1), min = 0, max = 1), simplify = F)

# 2. Для каждого из полученный векторов строим вариационный ряд, то есть упорядочиваем по возрастанию

r <- lapply(r_tmp, sort)

# 3. Находим длины отрезков, на которые вектор r разбивает отрезок [0;1] --
#             получаем вектор вероятностей p

p_tmp <- lapply(r, diff)

heads <- lapply(r, head, 1)
tails <- lapply(r, function(x) (1-tail(x,1)))

p <- mapply(append, mapply(append, heads,p_tmp,SIMPLIFY = F),tails, SIMPLIFY = F)

# 4. Первый из полученных векторов p считаем вектором начальных вероятностей, 
#             из остальных составляем матрицу переходов P,
#             записывая их по строкам

p0 <- p[[1]] # вектор начальных условий
P <- t(simplify2array(p))[-1,] # матрица переходов


##### Построение размеченного графа состояний цепи
#install.packages("diagram")
#install.packages("markovchain")
library(markovchain)
library(diagram)

png(filename = "../img/1.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
plotmat(signif(P,3), 
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


#### Моделирование траектории длины k цепи маркова
#1. Генерируем равномерно распределенную на [0;1] случайную величину r0 и по вектору p0
#             разыгрываем начальное состояние

r0 <- runif(1, min = 0, max = 1)

foo <- function(r0_loc,j)
{
  ifelse(r0_loc < r[[j]][1],0,
  ifelse(r0_loc < r[[j]][2],1,
  ifelse(r0_loc < r[[j]][3],2,
  ifelse(r0_loc < r[[j]][4],3,
  ifelse(r0_loc < r[[j]][5],4,0)))))
}

step_1 <- foo(r0,1)
step_2 <- foo(runif(1, min = 0, max = 1),step_1)
step_3 <- foo(runif(1, min = 0, max = 1),step_2)
step_4 <- foo(runif(1, min = 0, max = 1),step_3)
step_5 <- foo(runif(1, min = 0, max = 1),step_4)

xi




aa <- matrix(c(0,5/11,6/11,
               5/12,0,7/12,
               6/13,7/13,0),nrow = 3, byrow = T)
matrix.power(aa,3)

bb <- matrix(c(1/2,0,1/2,0,
               0,0,0,1,
               1/4,1/2,1/4,0,
               0,1/2,1/2,0),
             byrow = T, ncol = 4)
bb

vect <- c(1/7, 2/7, 2/7, 2/7)

eigen(bb)

vect %*% bb
