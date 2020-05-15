#  Шаг 1 Создаем тот набор данных, который будем кластеризовать

#  Зададим зерно датчика случайных чисел
#  Чтобы у всех получались одинаковые картинки
set.seed(1234)

#  количество точек в каждом кластере
n.obs <- 900
#  стандартные отклонения (корень из дисперсии)
sd.1 <- 0.06


#  генерируем кластеры
#  точки из 1-го кластера
x1 <- rnorm(n.obs, mean = 0.2, sd = sd.1)
y1 <- rnorm(n.obs, mean = 0.38, sd = sd.1)

#  точки из 2-го кластера
x2 <- rnorm(n.obs, mean = 0.49, sd = sd.1)
y2 <- rnorm(n.obs, mean = 0.25, sd = sd.1)

#  точки из 3-го кластера
x3 <- rnorm(n.obs, mean = 0.62, sd = sd.1)
y3 <- rnorm(n.obs, mean = 0.42, sd = sd.1)

#  точки из 4-го кластера
x4 <- rnorm(n.obs, mean = 0.42, sd = sd.1)
y4 <- rnorm(n.obs, mean = 0.78, sd = sd.1)

#  точки из 5-го кластера
x5 <- rnorm(n.obs, mean = 0.85, sd = sd.1)
y5 <- rnorm(n.obs, mean = 0.75, sd = sd.1)


#  объединяем данные в одну матрицу
x.0 <- c(x1, x2, x3, x4, x5)
y.0 <- c(y1, y2, y3, y4, y5)
data.0 <- cbind(x.0, y.0)

#  делаем красиво: добавляем имена столбцов 
colnames(data.0) <- c("x", "y")


#  Шаг 2  Создаем начальные центры кластеров
#         На практике - случайные, сейчас берем такие,
#         чтобы процесс кластеризации выглядел выразительно

#  абсциссы точек
x.start <- c(0.50, 0.41, 0.43, 0.62, 0.38)
#  ординаты точек
y.start <- c(0.20, 0.22, 0.32, 0.36, 0.71)

#  объединяем данные в одну матрицу
centers.0 <- cbind(x.start, y.start)


#  Шаг 3  Проводим кластеризацию
#  Вместо выполнения 15 итераций разом,
#  15 раз выполняем по одной итерации.
#  Чтобы посмотреть, как работает процедура

clus.1 <- kmeans(data.0, centers=centers.0,     iter.max=1, algorithm = "Lloyd")
clus.2 <- kmeans(data.0, centers=clus.1$centers,iter.max=1, algorithm = "Lloyd")
clus.3 <- kmeans(data.0, centers=clus.2$centers,iter.max=1, algorithm = "Lloyd")

clus.4 <- kmeans(data.0, centers=clus.3$centers,iter.max=1, algorithm = "Lloyd")
clus.5 <- kmeans(data.0, centers=clus.4$centers,iter.max=1, algorithm = "Lloyd")
clus.6 <- kmeans(data.0, centers=clus.5$centers,iter.max=1, algorithm = "Lloyd")

clus.7 <- kmeans(data.0, centers=clus.6$centers,iter.max=1, algorithm = "Lloyd")
clus.8 <- kmeans(data.0, centers=clus.7$centers,iter.max=1, algorithm = "Lloyd")
clus.9 <- kmeans(data.0, centers=clus.8$centers,iter.max=1, algorithm = "Lloyd")
clus.10 <- kmeans(data.0, centers=clus.9$centers,iter.max=1, algorithm = "Lloyd")
clus.11 <- kmeans(data.0, centers=clus.10$centers,iter.max=1, algorithm = "Lloyd")
clus.12 <- kmeans(data.0, centers=clus.11$centers,iter.max=1, algorithm = "Lloyd")
clus.13 <- kmeans(data.0, centers=clus.12$centers,iter.max=1, algorithm = "Lloyd")
clus.14 <- kmeans(data.0, centers=clus.13$centers,iter.max=1, algorithm = "Lloyd")
clus.15 <- kmeans(data.0, centers=clus.14$centers,iter.max=1, algorithm = "Lloyd")
clus.16 <- kmeans(data.0, centers=clus.15$centers,iter.max=1, algorithm = "Lloyd")


#  Шаг 4  Графики процесса кластеризации


#  Задаем размер точек на графиках, которые появятся позднее
cex.1 <- 0.6

#  Задаем цвета кластеров, которые появятся позднее
col.1 <- c("green", "blue", "cyan", "darkorchid", "darkgoldenrod")



#  Итерация 0 - что имеем до начала применения процедуры



#  Итерация 1

#  Исходные данные
png(filename = "../img/k-means_Step-by-Step/initial_data.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
    plot(data.0, col="blue", pch=19,main="Iteration 0", cex = cex.1)
dev.off()

#  Начальные центры кластеров
png(filename = "../img/k-means_Step-by-Step/initial_data_with_initial_centroids.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
    plot(data.0, col="blue", pch=19,main="Iteration 0", cex = cex.1)
    points(x.start, y.start, col = 'red', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*5, pch = 19)
dev.off()


#  Распределяем объекты по кластерам
png(filename = "../img/k-means_Step-by-Step/initial_data_colored.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
    plot(data.0, col=col.1[clus.1$cluster], pch=19,main="Iteration 1", cex = cex.1)
    points(x.start, y.start, col = 'red', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*5, pch = 19)
dev.off()

png(filename = "../img/k-means_Step-by-Step/initial_data_with_new_centroids_and_arrows.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
    plot(data.0, col=col.1[clus.1$cluster], pch=19,main="Iteration 1", cex = cex.1)
    points(x.start, y.start, col = 'red', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*5, pch = 19)
    #  Новые центры кластеров
    points(clus.1$centers, col = 'black', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*5, pch = 19)
    #  Стрелки - перемещение центров кластеров
    arrows(x.start, y.start, x1 = clus.1$centers[,1], y1 = clus.1$centers[,2], 
       col = "red", lwd = 4, angle = 15, length = 0.2)
dev.off()

#-----------------------------------------

#  Итерация 2
png(filename = "../img/k-means_Step-by-Step/iter2.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
    plot(data.0, col=col.1[clus.2$cluster], pch=19,main="Iteration 2", cex = cex.1)
    
    points(clus.1$centers, col = 'red', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*5, pch = 19)
    
    points(clus.2$centers, col = 'black', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*5, pch = 19)
    
    arrows(clus.1$centers[,1], clus.1$centers[,2], x1 = clus.2$centers[,1], y1 = clus.2$centers[,2], 
           col = "red", lwd = 4, angle = 15, length = 0.2)
dev.off()

#-----------------------------------------

#  Итерация 3
png(filename = "../img/k-means_Step-by-Step/iter3.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
    #  Исходные данные
    plot(data.0, col="blue", pch=19,main="Iteration 3", cex = cex.1)
    
    #  Новые центры кластеров
    points(clus.2$centers, col = 'red', xlim = c(0,1), ylim = c(0, 1.1), 
           cex = cex.1*9, pch = 19)
dev.off()

#-----------------------------------------



#  Шаг 5  Как перераспределялись объекты между кластерами
par(mfrow=c(2,2))
plot(data.0, col=col.1[clus.1$cluster], pch=19,main="Iteration 1", cex = cex.1/10)
plot(data.0, col=col.1[clus.2$cluster], pch=19,main="Iteration 2", cex = cex.1/10)
plot(data.0, col=col.1[clus.3$cluster], pch=19,main="Iteration 3", cex = cex.1/10)
plot(data.0, col=col.1[clus.4$cluster], pch=19,main="Iteration 4", cex = cex.1/10)

plot(data.0, col=col.1[clus.5$cluster], pch=19,main="Iteration 5", cex = cex.1/10)
plot(data.0, col=col.1[clus.6$cluster], pch=19,main="Iteration 6", cex = cex.1/10)
plot(data.0, col=col.1[clus.7$cluster], pch=19,main="Iteration 7", cex = cex.1/10)
plot(data.0, col=col.1[clus.8$cluster], pch=19,main="Iteration 8", cex = cex.1/10)

plot(data.0, col=col.1[clus.9$cluster], pch=19,main="Iteration 9", cex = cex.1/10)
plot(data.0, col=col.1[clus.10$cluster], pch=19,main="Iteration 10", cex = cex.1/10)
plot(data.0, col=col.1[clus.11$cluster], pch=19,main="Iteration 11", cex = cex.1/10)
plot(data.0, col=col.1[clus.12$cluster], pch=19,main="Iteration 12", cex = cex.1/10)

plot(data.0, col=col.1[clus.13$cluster], pch=19,main="Iteration 13", cex = cex.1/10)
plot(data.0, col=col.1[clus.14$cluster], pch=19,main="Iteration 14", cex = cex.1/10)
plot(data.0, col=col.1[clus.15$cluster], pch=19,main="Iteration 15", cex = cex.1/10)
plot(data.0, col=col.1[clus.16$cluster], pch=19,main="Iteration 16", cex = cex.1/10)


