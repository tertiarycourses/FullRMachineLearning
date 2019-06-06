################ Bagging (used for single model)

bag.xgb_learner = makeBaggingWrapper(xg.lrn,
                                     bw.iters= 50,
                                     bw.replace= TRUE,
                                     bw.size = 0.8,
                                     bw.feats = 3/4)



xgb_model <- train(bag.xgb_learner, task = xg.task)

xgb.pred <- predict(xgb_model, newdata= iris.test)

performance(xgb.pred, measures = list(mmce, acc))  # can set to FALSE






################# Ensemble (used ofr multiple models)

## learners

base.learners = list(
  makeLearner("classif.rpart"),
  makeLearner("classif.randomForest"),
  makeLearner("classif.xgboost")
)


## search grid

params = makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=20),
  makeIntegerParam("max_depth",lower=1,upper=5)
)


## resampling

rdesc <- makeResampleDesc("CV", iters = 3)


# control
ctrl = makeTuneControlRandom(maxit = 10L)


######## making ensemble

xgb_learnerENS = makeStackedLearner(base.learners = base.learners, 
                                    super.learner = "classif.svm",
                                    predict.type = "prob",             #can be NULL
                                    method = "stack.nocv", 
                                    use.feat = FALSE,
                                    parset = params)

  

xgb_model <- train(xgb_learnerENS, task = xg.task)

xgb.pred <- predict(xgb_model, newdata= iris.test)

performance(xgb.pred, measures = list(mmce, acc))  # can set to FALSE
