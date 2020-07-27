# Topic 7 Ensemble Methods

library(mlr)
library(gbm)
library(xgboost)

# Random Forest
data(iris)

nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
iris.train <- iris[inTrain,]
iris.test <- iris[-inTrain,]

rf.task = makeClassifTask(id ="rf", data = iris.train, target= "Species")
rf.lrn = makeLearner("classif.randomForest")   
mod = train(rf.lrn, rf.task)
getLearnerModel(mod)

rf.pred = predict(mod, newdata = iris.test)

performance(rf.pred, measures = list(mmce, acc))


calculateConfusionMatrix(rf.pred)
fimp=generateFeatureImportanceData(task=rf.task,learner=rf.lrn)

ll=length(iris.test)-1
barplot(as.matrix(fimp$res[1:ll]), names.arg =names(fimp$res[1:ll]),xlab=row.names(fimp$res),horiz=T)

plotLearnerPrediction(rf.lrn, features=c("Petal.Length","Petal.Width"),  task=rf.task)

# Gradient Boos

data(iris)
nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
iris.train <- iris[inTrain,]
iris.test <- iris[-inTrain,]

gbm.task = makeClassifTask(id ="gbm", data = iris.train, target= "Species")

gbm.lrn = makeLearner("classif.gbm")   
mod = train(gbm.lrn, gbm.task)
getLearnerModel(mod)

gbm.pred = predict(mod, newdata = iris.test)
performance(gbm.pred, measures = list(mmce, acc))

head(getPredictionTruth(gbm.pred))
head(getPredictionResponse(gbm.pred))

calculateConfusionMatrix(gbm.pred)

plotLearnerPrediction(gbm.lrn, features=c("Petal.Length","Petal.Width"), task=gbm.task)

fimp=generateFeatureImportanceData(task=gbm.task,learner=gbm.lrn)

ll=length(iris.test)-1
barplot(as.matrix(fimp$res[1:ll]), names.arg =names(fimp$res[1:ll]),xlab=row.names(fimp$res),horiz=T)
plotLearnerPrediction(gbm.lrn, features=c("Petal.Length","Petal.Width"), task=gbm.task)


# XGBoost

data(iris)
nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
iris.train <- iris[inTrain,]
iris.test <- iris[-inTrain,]

xg.task = makeClassifTask(id ="xgb", data = iris.train, target= "Species")


xg.lrn = makeLearner("classif.xgboost")   
mod = train(xg.lrn, xg.task)
getLearnerModel(mod)
xg.pred = predict(mod, newdata = iris.test)

performance(xg.pred, measures = list(mmce, acc))
head(getPredictionTruth(xg.pred))
head(getPredictionResponse(xg.pred))
calculateConfusionMatrix(xg.pred)


plotLearnerPrediction(xg.lrn, features=c("Petal.Length","Petal.Width"), task=xg.task)

fimp=generateFeatureImportanceData(task=xg.task,learner=xg.lrn)
ll=length(iris.test)-1
barplot(as.matrix(fimp$res[1:ll]), names.arg =names(fimp$res[1:ll]),xlab=row.names(fimp$res),horiz=T)

plotLearnerPrediction(xg.lrn, features=c("Petal.Length","Petal.Width"), task=xg.task) 