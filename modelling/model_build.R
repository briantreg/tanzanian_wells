library(mlr)
library(LiblineaR)



tznData = readRDS('data_preparation/prediction_set.RDS')

tznTask = makeClassifTask(data = tznData,
                target = 'status_group')

tznResampling = makeResampleDesc('CV', iters = 5L)


tznGBMCtrl = makeTuneControlRandom(maxit = 20L,)

tznGBM = makeLearner(cl = 'classif.gbm')

tznGBMPar = makeParamSet(
    makeDiscreteParam("n.trees",values = c(50,100,500,1000,5000,10000)),
    makeIntegerParam("interaction.depth",lower = 1L, upper = 15L),
    makeDiscreteParam("n.minobsinnode",values = c(100,500,750,1000,2000)),
    makeDiscreteParam("shrinkage",values = c(0.3,0.2,0.1,0.01,0.001)),
    makeNumericParam("bag.fraction",lower = 0.2, upper = 1),
    makeNumericParam("train.fraction",lower = 0.2, upper = 1)
)

tznGBM_tuned = tuneParams(learner = tznGBM,
                               task = tznTask,
                               resampling = tznResampling,
                               par.set = tznGBMPar,
                               control = tznGBMCtrl)


tznLogisticCtrl = makeTuneControlRandom(maxit = 25L)

tznLogistic = makeLearner(cl = 'classif.LiblineaRL1LogReg')

tznLogisticPar = makeParamSet(
    makeNumericParam("cost",lower = 0, upper = 2)
)

tznLogistic_tuned = tuneParams(learner = tznLogistic,
           task = tznTask,
           resampling = tznResampling,
           par.set = tznLogisticPar,
           control = tznLogisticCtrl)
