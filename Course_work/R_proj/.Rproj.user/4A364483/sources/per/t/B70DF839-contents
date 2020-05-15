library(dplyr)
library(ggplot2)
library(kernlab) # kernel k-means

set.seed(1234)
n <- 250
# Пример линейно не разделимого множества
c1 <- data_frame(x = rnorm(n), y = rnorm(n), cluster = 1)
c2 <- data_frame(r = rnorm(n, 5, .25), theta = runif(n, 0, 2 * pi),
                 x = r * cos(theta), y = r * sin(theta), cluster = 2) %>%
  dplyr::select(x, y, cluster)
points1 <- rbind(c1, c2) %>% mutate(cluster = factor(cluster))

png(filename = "../img/kernel_k-means/initial_data.png",
    width = 853, height = 707,
    res = 96 * 1.25)
  ggplot(points1, aes(x, y)) + geom_point() +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
dev.off()

# k-means
fit <- kmeans(points1[,-3],2)
png(filename = "../img/kernel_k-means/basic_k-means_clusters.png",
    width = 853, height = 707,
    res = 96 * 1.25)
  ggplot(points1, aes(x,y, color = as.factor(fit$cluster))) + geom_point()  + 
  labs(color = "K-means clusters")+
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
dev.off()
# kernel k-means
fit_kernel <- kkmeans(as.matrix(points1[,-3]), centers = 2, kernel = "rbfdot", kpar = list(sigma = 0.15))
png(filename = "../img/kernel_k-means/kernel_k-means_clusters.png",
    width = 853, height = 707,
    res = 96 * 1.25)
  ggplot(points1, aes(x,y, color = as.factor(fit_kernel@.Data))) + 
    geom_point() + 
    labs(color = "Kernel K-means clusters")+
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))
dev.off()

