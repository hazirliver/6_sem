
# Пример классификации методом к-средних

#  Читаем данные примера
beverage.01 <- read.table("Data/beverage_r.csv", header=T, sep=";")


#  Проверим размерность данных
dim(beverage.01)

# Вспомним имена переменных
names(beverage.01)


# Удачно угадаем, что кластеров три
#  Важная опция  nstart=...
summ.1 = kmeans(beverage.01[,2:9], 3, iter.max = 100)


names(summ.1)
#  [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#  [6] "betweenss"    "size"         "iter"         "ifault"  


# К каким кластерам принадлежат объекты?
summ.1$cluster
#   [1] 3 2 2 1 2 2 1 3 3 2 2 1 3 3 1 3 3 2 3 2 2 2 1 2 1 1 1 3 3 1 2 1 2 1

#  Координаты центров кластеров
#  Основной источник вдохновения для интерпретации
summ.1$centers
#    COKE    D_COKE    D_PEPSI     D_7UP PEPSI    SPRITE       TAB   SEVENUP
#  1  0.0 1.0000000 0.54545455 0.5454545   0.0 0.0000000 0.9090909 0.0000000
#  2  1.0 0.2307692 0.07692308 0.0000000   1.0 0.1538462 0.0000000 0.2307692
#  3  0.7 0.3000000 0.10000000 0.1000000   0.3 0.9000000 0.1000000 0.6000000

#  Результат неудобно читать. 

options(digits=2)



Транспонируем
t(summ.1$centers)
#1          2   3
#  COKE    0.0000000 1.00000000 0.7
#  D_COKE  1.0000000 0.23076923 0.3
#  D_PEPSI 0.5454545 0.07692308 0.1
#  D_7UP   0.5454545 0.00000000 0.1
#  PEPSI   0.0000000 1.00000000 0.3
#  SPRITE  0.0000000 0.15384615 0.9
#  TAB     0.9090909 0.00000000 0.1
#  SEVENUP 0.0000000 0.23076923 0.6

options(digits=7)

#  Сумма квадратов расстояний от объектов кластера до центра кластера
summ.1$withinss
#  [1]  6.363636  7.230769 12.300000


#  Сумма элементов предыдущего вектора
summ.1$tot.withinss


summ.1$totss
#  то же самое, что и 
#  sum(33*(apply(beverage.01[,2:9], 2, sd))^2)


#  Смотрим help
summ.1$tot.betweenss


#  Размеры кластеров
summ.1$size
#  [1] 11 13 10




#  Попробуем определить "правильное" число кластеров

wss <- (nrow(beverage.01[,2:9])-1)*sum(apply(beverage.01[,2:9],2,var))
for (i in 2:15) wss[i] <- kmeans(beverage.01[,2:9], 
                                 centers=i)$tot.withinss
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 



#  Задача

#  Попробовать решение с 4 кластерами
#  Сравнить кластеризации

#  Для сравнения использовать команду
table(summ.1$cluster, summ.2$cluster)




#  Проекция данных на плоскость
#  Multidimentional scaling

beverage.dist <- dist(beverage.01[,2:9])
beverage.mds <- cmdscale(beverage.dist)

groups <- cutree(clust.beverage, k=3) 

plot(beverage.mds, col = groups, xlab = "Index", ylab = "Y")



#  Определение числа кластеров

beverage.01 <- read.table("beverage_r.csv", header=T, sep=";")

dist.beverage <- dist(beverage.01[,2:9])


clust.beverage <- hclust(dist.beverage, "ward")


install.packages(NbClust)

library(NbClust)

Best <- NbClust(beverage.01[,2:9], distance = "euclidean", min.nc = 2, max.nc = 15, method = "ward", index = "alllong")











