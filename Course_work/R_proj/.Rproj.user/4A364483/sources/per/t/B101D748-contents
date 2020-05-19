
# Создадим 5 точек, которые будет кластеризовать
p1 <- c(1,1)
p2 <- c(1,2)
p3 <- c(6,4)
p4 <- c(8,5)
p5 <- c(8,6)

# График начальных данных
df <- rbind(p1,p2,p3,p4,p5)
png(filename = "../img/hclust/single/initial_data.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  plot(df, xlab = "", ylab = "", lwd = 2, pch = 19)
  text(df, labels=c("p1", "p2","p3","p4","p5"), cex= 0.7, pos = c(3,3,3,1,1))

dev.off()

#-------------------------------------------------------------------------------------------------------#
# График иерархической кластеризации по методу signle с евклидовой метрикой расстояний
h1 <- hclust(dist(df), method = "single")
png(filename = "../img/hclust/single/hclust_euc_1.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  plot(h1, xlab = "", hang = -1)

dev.off()

h1$height

#-------------------------------------------------------------------------------------------------------#

# График иерархической кластеризации по методу signle с евклидовой метрикой расстояний
h2 <- hclust(dist(df), method = "complete")
png(filename = "../img/hclust/complete/hclust_euc_1.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  plot(h2, xlab = "", hang = -1)

dev.off()

h1$height




