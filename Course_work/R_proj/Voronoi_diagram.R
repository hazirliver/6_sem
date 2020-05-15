library(ggplot2)
library(ggvoronoi)

### Иллюстрация диаграммы Вороного
set.seed(1234)
x <- sample(1:200,10)
y <- sample(1:200,10)
points <- data.frame(x, y)

png(filename = "../img/Voronoi_diagram/voronoi1.png",
    width = 1920, height = 1080,
    res = 96 * 1.25)
ggplot(points,aes(x,y)) +
  stat_voronoi(geom="path", lwd = 2) +
  geom_point(col = "red", lwd = 5) +
  theme_void()
dev.off()









