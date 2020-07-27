# Topic 4  Clustering

#install.packages("mlr")
#install.packages("clue")
#install.packages("clusterSIm")
library(mlr)
library(clue)
library(clusterSim)
library(cluster)
library(factoextra) 
library(ggplot2)
library(cowplot)
library(dplyr)
theme_set(theme_bw())

# K-Means Clustering

# Load data
data(iris)
#iris <- read.csv(file.choose()) 
# iris.x = subset(iris,select=-c(Species))
iris.x <- iris %>% select(-Species) 

# KMeans Clustering
kcluster <- kmeans(iris.x, centers = 3, nstart = 25)
kcluster$cluster <- as.factor(kcluster$cluster)

p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=kcluster$cluster)) + geom_point()
plot_grid(p1,p2)

# Ex: KMeans Clustering

# Load data
#wine <-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",header=TRUE,sep=";")
#wheat <-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt",header=TRUE)
data(mtcars)
mtcars.select <- subset(mtcars,select=c('mpg','hp','cyl'))
mtcars.x <- mtcars.select %>% select(-cyl)

kcluster <- kmeans(mtcars.x, centers = 5, nstart = 25)
kcluster$cluster <- as.factor(kcluster$cluster)

p1 <- ggplot(mtcars.select, aes(x=mpg, y=hp, color=cyl)) + geom_point()
p2 <- ggplot(mtcars.select, aes(x=mpg, y=hp, color=kcluster$cluster)) + geom_point()
plot_grid(p1,p2)

# Hierachical Clustering

disM <- dist(iris.x)
hcluster <- hclust(disM)

# Visualization
plot(as.dendrogram(hcluster))
rect.hclust(hcluster, k=3, border="red")

groups <- cutree(hcluster, k=3)
groups <- as.factor(groups)
p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=groups)) + geom_point()
plot_grid(p1,p2)

# Ex: Hierachical Clustering

data(mtcars)
mtcars.select <- subset(mtcars,select=c('mpg','hp','cyl'))
mtcars.x <- mtcars.select %>% select(-cyl)

disM <- dist(mtcars.x)
hcluster <- hclust(disM)

plot(as.dendrogram(hcluster))
rect.hclust(hcluster, k=3, border="red")

groups <- cutree(hcluster, k=5)
groups <- as.factor(groups)
p1 <- ggplot(mtcars.select, aes(x=mpg, y=hp, color=cyl)) + geom_point()
p2 <- ggplot(mtcars.select, aes(x=mpg, y=hp, color=groups)) + geom_point()
plot_grid(p1,p2)

# Silhouette Analyis

kmean.pam <- pam(iris.x, k = 3)
plot(silhouette(kmean.pam))
kmean.pam $silinfo$widths
kmean.pam $silinfo$avg.width

# Ex: Silhouette Analyis
kmean.pam <- pam(mtcars.x, k = 5)
plot(silhouette(kmean.pam))
kmean.pam $silinfo$widths
kmean.pam $silinfo$avg.width

# Anomaly Detection

data(iris)
iris.x <- iris %>% select(-Species) 

kcluster <- kmeans(iris.x, centers = 3, nstart = 25)
kcluster$cluster <- as.factor(kcluster$cluster)

centers <- kcluster$centers[kcluster$cluster, ]
distances <- sqrt(rowSums((iris.x - centers)^2))
outliers <- order(distances, decreasing=T)[1:5]

plot(iris.x[,c("Petal.Length", "Petal.Width")], pch=19, col=kcluster$cluster, cex=1)
points(iris.x[outliers, c("Petal.Length", "Petal.Width")], pch="+", col=4, cex=3)

# Ex: Anomaly Detection
data(mtcars)
mtcars.select <- subset(mtcars,select=c('mpg','hp','cyl'))
mtcars.x <- mtcars.select %>% select(-cyl)

kcluster <- kmeans(mtcars.x, centers = 3, nstart = 25)
kcluster$cluster <- as.factor(kcluster$cluster)

centers <- kcluster$centers[kcluster$cluster, ]
distances <- sqrt(rowSums((mtcars.x - centers)^2))
outliers <- order(distances, decreasing=T)[1:5]

plot(mtcars.x[,c("mpg", "hp")], pch=19, col=kcluster$cluster, cex=1)
points(mtcars.x[outliers, c("mpg", "hp")], pch="+", col=4, cex=3)


