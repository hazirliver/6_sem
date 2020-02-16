### Начальные данные:
m <- 6 # Число состояний марковской цепи
k <- 5 # время (шаги)
n <- 180 # траектории


#### Моделирование цепи маркова с m состояниями
# 1. Генерируем (m+1) раз вектор r=(r_1, ..., r_{m-1}) из независимых 
#             и равномерно распределенных на отрезке
#             [0;1] случайных величин

r_tmp <- replicate((m+1), runif((m-1), min = 0, max = 1), simplify = F)

# 2. Для каждого из полученный векторов строим вариационный ряд, то есть упорядочиваем по возрастанию

r <- lapply(r_tmp, sort)

# 3. Находим длины отрезков, на которые вектор r разбивает отрезок [0;1] --
#             получаем вектор вероятностей p

p_tmp <- lapply(r, diff)

heads <- lapply(r, head, 1)
tails <- lapply(r, function(x) (1-tail(x,1)))

p <- mapply(append, mapply(append, heads,p_tmp,SIMPLIFY = F),
            tails, SIMPLIFY = F)

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

tracs <- list()

for (k in 1:n)
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
  step_2 <- foo(runif(1, min = 0, max = 1),step_1)
  step_3 <- foo(runif(1, min = 0, max = 1),step_2)
  step_4 <- foo(runif(1, min = 0, max = 1),step_3)
  step_5 <- foo(runif(1, min = 0, max = 1),step_4)
  
  trac <- list(c(step_1,step_2,step_3,step_4,step_5))
  tracs[k] <- trac
}

tracs_array <- t(simplify2array(tracs,higher = F))
colnames(tracs_array) <- paste("Шаг",as.character(1:m))
rownames(tracs_array) <- paste("Тр.",as.character(1:n))



### Вычисление эмпирических вероятностей (относительных частот) 
#         состояний  цепи на k шаге.



library(ggplot2)

emp <- hist(tracs_array, breaks = 0:m)$density
theor <- as.numeric(p_k)

matr <- matrix(c(rep("theor", m),rep("emperical", m),
                 1:m,1:m,
                 theor,emp), ncol = 3)



plot_df <- data.frame(type = rep(c("theoretical", "emperical"), each=m),
                      step = rep(paste("Шаг",as.character(1:m)), 2),
                      prob = c(theor, emp))

ggplot(data=plot_df, aes(x=step, y=prob, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_bw()

prob_diff <- emp - theor
max(abs(prob_diff))
