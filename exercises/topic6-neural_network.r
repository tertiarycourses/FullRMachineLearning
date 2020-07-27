# Topic 5  Deep Learning

library(neuralnet)

data(iris)
iris$setosa <- iris$Species=="setosa"
iris$virginica <- iris$Species == "virginica"
iris$versicolor <- iris$Species == "versicolor"

iris.train.idx <- sample(x = nrow(iris), size = nrow(iris)*0.7)
iris.train <- iris[iris.train.idx,]
iris.valid <- iris[-iris.train.idx,]

iris.net <- neuralnet(setosa+versicolor+virginica ~ 
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                      data=iris.train, hidden=c(10,10), rep = 5, err.fct = "ce", 
                      linear.output = F, lifesign = "minimal", stepmax = 1000000,
                      threshold = 0.001)

plot(iris.net, rep="best")

iris.prediction <- compute(iris.net, iris.valid[-5:-8])
idx <- apply(iris.prediction$net.result, 1, which.max)
predicted <- c('setosa', 'versicolor', 'virginica')[idx]
table(predicted, iris.valid$Species)

# Ex: Neural Network
iris.net <- neuralnet(setosa+versicolor+virginica ~ 
                        Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                      data=iris.train, hidden=c(10,20,10), rep = 5, err.fct = "ce", 
                      linear.output = F, lifesign = "minimal", stepmax = 1000000,
                      threshold = 0.001)

plot(iris.net, rep="best")

iris.prediction <- compute(iris.net, iris.valid[-5:-8])
idx <- apply(iris.prediction$net.result, 1, which.max)
predicted <- c('setosa', 'versicolor', 'virginica')[idx]
table(predicted, iris.valid$Species)
