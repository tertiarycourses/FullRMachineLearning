# Many machine learning algorithms have hyperparameters that need to be set.

# simply pass them to makeLearner. Often suitable parameter
# values are not obvious and it is preferable to tune the hyperparameters, that is
# automatically identify values that lead to the best performance.



#### PRE -REQUISITES

# 1. the search space for every hyperparameter
# 2. the tuning method (e.g. grid or random search)
# 3. the resampling method




data(iris)
names(iris)

nr <- nrow(iris)
inTrain <- sample(1:nr, 0.6*nr)
iris.train <- iris[inTrain,1:4]
iris.test <- iris[-inTrain,1:4]


### Making Tasks
xgb_task = makeRegrTask(id ="xgb", 
                        data = iris.train, 
                        target= "Petal.Width")
xgb_task


#### Making Learner

xgb_learn = makeLearner("regr.xgboost")  


#### Making Model

xgb_model=train(xgb_learn, xgb_task)


getParamSet("regr.xgboost")


## search grid

params = makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=20),
  makeIntegerParam("max_depth",lower=1,upper=5)
)

print(params)


#### Define the tuning method

## Grid search

ctrl = makeTuneControlGrid()     # this can only deal with discrete parameter sets



#### Define resampling strategy 

rdesc = makeResampleDesc("CV", iters = 5L)



#### Define measures

msrs = list(mse, rsq)


#### Performing the Tuning



xgb_tuned <- tuneParams(learner = xgb_learn,              # this will take 2 mins
                        task = xgb_task,
                        resampling = rdesc,
                        measures = msrs,                  ## *** if error > restart R
                        par.set = params,
                        control = ctrl,
                        show.info = TRUE)   # can set to FALSE



xgb_tuned$x  
xgb_tuned$y  


##### Applying new parameters on our model

xgb_new <- setHyperPars(learner = xgb_learn, par.vals = xgb_tuned$x)
xgb_newmodel <- train(xgb_new, xgb_task)



tune_effects = generateHyperParsEffectData(xgb_tuned, partial.dep = TRUE)
tune_effects


plotHyperParsEffect(tune_effects, 
                    partial.dep.learn = "regr.xgboost",
                    x = "max_depth", 
                    y = "rsq.test.mean",           #rmse.test.rmse
                    z = "iteration",
                    plot.type = "line")
