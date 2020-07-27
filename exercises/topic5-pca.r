# Topic 5  PCA

library(ggplot2)
library(cowplot) 
library(dplyr)
library(pROC)
theme_set(theme_bw())

# Load data
data(iris)
p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point()
p2 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + geom_point()
p3 <- ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + geom_point()
p4 <- ggplot(iris, aes(x=Sepal.Width, y=Petal.Width, color=Species)) + geom_point()
plot_grid(p1, p2, p3, p4, labels = "AUTO")

# PCA
iris %>% select(-Species) %>% # remove Species column
  scale() %>%                 # scale to 0 mean and unit variance
  prcomp() ->                 # do PCA
  pca                         # store result as `pca`

pca

# Visualize 
pca_data <- data.frame(pca$x, Species=iris$Species)
ggplot(pca_data, aes(x=PC1, y=PC2, color=Species)) + geom_point()

# Variation of PC
percent <- 100*pca$sdev^2/sum(pca$sdev^2)
perc_data <- data.frame(percent=percent, PC=1:length(percent))
ggplot(perc_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80)


# Ex: PCA

# Load data
data(Boston)
Boston %>% select(-medv) %>% 
  scale() %>%                 
  prcomp() ->               
  pca

# Visualize 
pca_data <- data.frame(pca$x, medv=Boston$medv)
ggplot(pca_data, aes(x=PC1, y=PC2, color=medv)) + geom_point()

# Variation of PC
percent <- 100*pca$sdev^2/sum(pca$sdev^2)
perc_data <- data.frame(percent=percent, PC=1:length(percent))
ggplot(perc_data, aes(x=PC, y=percent)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(percent, 2)), size=4, vjust=-.5) + 
  ylim(0, 80)

