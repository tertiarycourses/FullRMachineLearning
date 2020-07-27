# Topic 8 Hyperparmaeter Tuning

#Grid Search
data(iris)
nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
iris.train <- iris[inTrain,1:4]
iris.test <- iris[-inTrain,1:4]

xgb_task = makeRegrTask(id ="xgb", data = iris.train, target= "Petal.Width")
xgb_learn = makeLearner("regr.xgboost")  
xgb_model=train(xgb_learn, xgb_task)
getParamSet("regr.xgboost")
params = makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=20),
  makeIntegerParam("max_depth",lower=1,upper=5)
)

ctrl = makeTuneControlGrid()     # this can only deal with discrete parameter sets
rdesc = makeResampleDesc("CV", iters = 5L)
msrs = list(mse, rsq)
xgb_tuned <- tuneParams(learner = xgb_learn,              # this will take 2 mins
                        task = xgb_task,
                        resampling = rdesc,
                        measures = msrs,                  ## *** if error > restart R
                        par.set = params,
                        control = ctrl,
                        show.info = TRUE)   # can set to FALSE

xgb_new <- setHyperPars(learner = xgb_learn, par.vals = xgb_tuned$x)

# Random Search
data(iris)
nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
iris.train <- iris[inTrain,1:4]
iris.test <- iris[-inTrain,1:4]


xgb_task = makeRegrTask(id ="xgb", data = iris.train, target= "Petal.Width")
xgb_learn = makeLearner("regr.xgboost")  
xgb_model=train(xgb_learn, xgb_task)

getParamSet("regr.xgboost")
params = makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=20),
  makeIntegerParam("max_depth",lower=1,upper=5)
)
ctrl = makeTuneControlRandom(maxit = 10L)
rdesc = makeResampleDesc("CV", iters = 5L)
msrs = list(mse, rsq)
xgb_tuned <- tuneParams(learner = xgb_learn,              # this will take 2 mins
                        task = xgb_task,
                        resampling = rdesc,
                        measures = msrs,                  ## *** if error > restart R
                        par.set = params,
                        control = ctrl,
                        show.info = TRUE)   # can set to FALSE

xgb_new <- setHyperPars(learner = xgb_learn, par.vals = xgb_tuned$x)
xgb_newmodel <- train(xgb_new, xgb_task)
tune_effects = generateHyperParsEffectData(xgb_tuned, partial.dep = TRUE)
plotHyperParsEffect(tune_effects, 
                    partial.dep.learn = "regr.xgboost",
                    x = "max_depth", 
                    y = "rsq.test.mean",           #rmse.test.rmse
                    z = "iteration",
                    plot.type = "line")

xgb_newmodel <- train(xgb_new, xgb_task)
tune_effects = generateHyperParsEffectData(xgb_tuned, partial.dep = TRUE)
plotHyperParsEffect(tune_effects, 
                    partial.dep.learn = "regr.xgboost",
                    x = "max_depth", 
                    y = "rsq.test.mean",           #rmse.test.rmse
                    z = "iteration",
                    plot.type = "line")