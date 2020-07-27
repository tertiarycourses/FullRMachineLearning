# Topic 3  Classifiactoin

library(mlr)
library(randomForest)
library(pROC)
library(ggplot2)
library(cowplot)
theme_set(theme_bw())

# Create binary targets for iris dataset
data(iris)
#iris <- read.csv(file.choose()) 
iris$Species <- ifelse(test=iris$Species == "virginica", yes=1, no=0)

# Logistic Regression using GLM
iris <-iris[order(iris$Sepal.Length),]
plot(x=iris$Sepal.Length, y=iris$Species)
glm.fit <- glm(iris$Species ~ iris$Sepal.Length, family=binomial)
summary(glm.fit)
lines(iris$Sepal.Length, glm.fit$fitted.values)

# Ex: Logistic Regression using GLM

num.samples <- 100
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/num.samples)), yes=1, no=0)
obese 
plot(x=weight, y=obese)
glm.fit <- glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)

# Logistic Regression using MLR

# Load data
iris$Species = as.factor(iris$Species)

# Create task
classifier.task = makeClassifTask(data = iris, target= "Species")

# List all classifers in MRL
listLearners("classif")$class

# Create Logistics Regression Learner (only for binary classification)
logreg = makeLearner("classif.logreg") 

# Create the model
model = train(logreg, classifier.task)

# K-fold Cross Validation
kfold = makeResampleDesc("RepCV", folds=10,reps=50)
kfoldCV = resample(learner=logreg,task = classifier.task, resampling = kfold)

# Visualize the result
plotLearnerPrediction(logreg, features=c("Petal.Length","Petal.Width"), task=classifier.task)

# Other Classification using MLR

# reset iris data
data("iris")

# Create task
classifier.task = makeClassifTask(data = iris, target= "Species")

# Create KNN Learner
knn = makeLearner("classif.knn", par.vals = list("k"=3))  

# Create SVM Learner
svm = makeLearner("classif.svm") 

# Create navie Bayes Learner
nb = makeLearner("classif.naiveBayes") 

# Create Decision Tree Learner
dt = makeLearner("classif.rpart") 

# Create Random Forest Learner
rf = makeLearner("classif.randomForest") 

# Create the model
model = train(knn, classifier.task)
#model = train(rf, classifier.task)

# K-fold Cross Validation
kfold = makeResampleDesc("RepCV", folds=10,reps=50)
kfoldCV = resample(learner=knn,task = classifier.task, resampling = kfold)
#kfoldCV = resample(learner=svm,task = classifier.task, resampling = kfold)
#kfoldCV = resample(learner=nb,task = classifier.task, resampling = kfold)
#kfoldCV = resample(learner=dt,task = classifier.task, resampling = kfold)
#kfoldCV = resample(learner=rf,task = classifier.task, resampling = kfold)

# Visualize the result
plotLearnerPrediction(knn, features=c("Petal.Length","Petal.Width"), task=classifier.task)


# Ex: Classifiacton

# Load data
titanic <- read.csv(file.choose()) 
titanic = subset(titanic, select=c(Survived, Pclass, Sex, Age))
str(titanic)
titanic$Survived <- ifelse(test=titanic$Survived == "Yes", yes=1, no=0)
titanic$Survived <- ifelse(test=titanic$Sex == "male", yes=1, no=0)
titanic$Survived = as.factor(titanic$Survived)
titanic$Sex = as.factor(titanic$Sex)
str(titanic)
titanic <- titanic[!is.na(titanic$Age),]

# Create task
classifier.task = makeClassifTask(data = titanic, target= "Survived")

# Create Logistics Regression Learner (only for binary classification)
logreg = makeLearner("classif.logreg") 

# Create KNN Learner
knn = makeLearner("classif.knn", par.vals = list("k"=3))  

# Create SVM Learner
svm = makeLearner("classif.svm") 

# Create navie Bayes Learner
nb = makeLearner("classif.naiveBayes") 

# Create Decision Tree Learner
dt = makeLearner("classif.rpart") 

# Create Random Forest Learner
rf = makeLearner("classif.randomForest") 

# Create the model
model = train(logreg, classifier.task)
#model = train(knn, classifier.task)
#model = train(rf, classifier.task)

# K-fold Cross Validation
kfold = makeResampleDesc("RepCV", folds=10,reps=50)
kfoldCV = resample(learner=logreg,task = classifier.task, resampling = kfold)
#kfoldCV = resample(learner=knn,task = classifer.task, resampling = kfold)
#kfoldCV = resample(learner=svm,task = classifer.task, resampling = kfold)
#kfoldCV = resample(learner=nb,task = classifer.task, resampling = kfold)
#kfoldCV = resample(learner=dt,task = classifer.task, resampling = kfold)
#kfoldCV = resample(learner=rf,task = classifer.task, resampling = kfold)

### Confusion Matrix
calculateConfusionMatrix(kfoldCV$pred)

# Ex: Confusion Matrix

# ROC

# ROC using pROC library
iris <-iris[order(iris$Sepal.Length),]
plot(x=iris$Sepal.Length, y=iris$Species)
glm.fit <- glm(iris$Species ~ iris$Sepal.Length, family=binomial)
summary(glm.fit)
lines(iris$Sepal.Length, glm.fit$fitted.values)

roc(iris$Sepal.Length,glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Positive Percentage")

# Ex: ROC
num.samples <- 100
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/num.samples)), yes=1, no=0)
obese 
plot(x=weight, y=obese)
glm.fit <- glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)

roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")

# ROC using MLR

# Create task
classifier.task = makeClassifTask(data = iris, target= "Species")

# Create Logistics Regression Learner (only for binary classification)
logreg = makeLearner("classif.logreg",predict.type = "prob") 

# Create the model
model = train(logreg, classifier.task)

# K-fold Cross Validation
kfold = makeResampleDesc("RepCV", folds=10,reps=50)
kfoldCV = resample(learner=logreg,task = classifier.task, resampling = kfold)

df = generateThreshVsPerfData(kfoldCV$pred, measures = list(fpr,tpr,mmce))
plotROCCurves(df)
