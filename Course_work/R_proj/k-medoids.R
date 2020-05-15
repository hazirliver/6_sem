library(ggplot2)
library(cluster) # k-medoids
library(caret) # Confusion matrix

set.seed(1234)
n <- 20
# Первый кластер
x1 <- rnorm(n, 0, 0.20)
y1 <- rnorm(n, 0, 0.20)

# Второй кластер
x2 <- rnorm(n-3, 0.6, 0.25)
y2 <- rnorm(n-3, 0.6, 0.25)

# Выборсы для второго кластера
x2_outliers <- rnorm(3, 1, 0.1)
y2_outliers <- rnorm(3, 3, 0.1)

x2_full <- c(x2, x2_outliers)
y2_full <- c(y2, y2_outliers)

# Датасет без выбросов
df_reduced <- data.frame(x = c(x1,x2), y = c(y1,y2), 
                         clus = factor(c(rep(1,n), rep(3,n-3)), labels = c("1 clus","2nd clus")))
df_w_outliers <- data.frame(x = c(x1,x2_full), y = c(y1,y2_full), 
                 clus = factor(c(rep(1,n), rep(3,n-3), rep(2,3)), labels = c("1 clus", "2d clus outliers", "2nd clus")))

# Верно раскрашенные данные
png(filename = "../img/k-medoids/initial_data.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  ggplot(df_w_outliers, aes(x,y, color = clus)) + geom_point() + labs(color = "K-means clusters") +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))

dev.off()




# Случай без выбросов
#----------------------------------------------------------------------------------------------------------------#

# Верно раскрашенные данные
png(filename = "../img/k-medoids/initial_data_reduced.png",
    width = 853, height = 707,
    res = 96 * 1.25)

ggplot(df_reduced, aes(x,y, color = clus)) + geom_point() + labs(color = "K-means clusters without outliers") +
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))

dev.off()

# k-means 
fit_k_means_outliers <- kmeans(df_reduced[,-3], 2)
png(filename = "../img/k-medoids/outliers_k-means_clusters.png",
    width = 853, height = 707,
    res = 96 * 1.25)

ggplot(df_reduced, aes(x,y, color = as.factor(fit_k_means_outliers$cluster))) + geom_point() + 
  labs(color = "K-means clusters without outliers")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA)) +
  geom_point(data = as.data.frame(fit_k_means_outliers$centers), aes(x,y), pch = 10, size = 7, col = "black")

dev.off()

# k-medoids
fit_k_medoids_outliers <- pam(df_reduced[,-3], 2)
png(filename = "../img/k-medoids/outliers_k-medoids_clusters.png",
    width = 853, height = 707,
    res = 96 * 1.25)

ggplot(df_reduced, aes(x,y, color = as.factor(fit_k_medoids_outliers$clustering))) + geom_point() + 
  labs(color = "K-medoids clusters without outliers")+
  theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA)) +
  geom_point(data = as.data.frame(fit_k_medoids_outliers$medoids), aes(x,y), pch = 19, size = 4, col = "black")

dev.off()

real_clusters <- as.numeric(df_reduced$clus)
real_clusters[real_clusters == 3] <- 2

# k-means
k_means_clusters_outliers <- fit_k_means_outliers$cluster
confusionMatrix(table(k_means_clusters_outliers, real_clusters)) 

# k-medoids 
k_medoids_clusters_outliers <- fit_k_medoids_outliers$clustering
confusionMatrix(table(k_medoids_clusters_outliers, real_clusters)) 







# Случай с выбросами
#----------------------------------------------------------------------------------------------------------------#
# Верно раскрашенные данные
png(filename = "../img/k-medoids/initial_data.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  ggplot(df_w_outliers, aes(x,y, color = clus)) + geom_point() + labs(color = "K-means clusters") +
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA))

dev.off()

# k-means 
fit_k_means <- kmeans(df_w_outliers[,-3], 2)
png(filename = "../img/k-medoids/basic_k-means_clusters.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  ggplot(df_w_outliers, aes(x,y, color = as.factor(fit_k_means$cluster))) + geom_point() + 
    labs(color = "K-means clusters")+
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA)) +
    geom_point(data = as.data.frame(fit_k_means$centers), aes(x,y), pch = 10, size = 7, col = "black")
  
dev.off()

# k-medoids
fit_k_medoids <- pam(df_w_outliers[,-3], 2)
png(filename = "../img/k-medoids/basic_k-medoids_clusters.png",
    width = 853, height = 707,
    res = 96 * 1.25)

  ggplot(df_w_outliers, aes(x,y, color = as.factor(fit_k_medoids$clustering))) + geom_point() + 
    labs(color = "K-medoids clusters")+
    theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA)) +
    geom_point(data = as.data.frame(fit_k_medoids$medoids), aes(x,y), pch = 19, size = 4, col = "black")
  
dev.off()

real_clusters <- as.numeric(df_w_outliers$clus)
real_clusters[real_clusters == 3] <- 2

# k-means
k_means_clusters <- fit_k_means$cluster
confusionMatrix(table(k_means_clusters, real_clusters)) 
precision(table(k_means_clusters, real_clusters))
recall(table(k_means_clusters, real_clusters))
# k-medoids 
k_medoids_clusters <- fit_k_medoids$clustering
confusionMatrix(table(k_medoids_clusters, real_clusters))















