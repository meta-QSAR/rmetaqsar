#MetaQSAR research project.
#Author: Ivan Olier
#email: iaolier@gmail.com
#2013-2016

library(kernlab)
makeRLearner.regr.ksvmfp = function() {
  makeRLearnerRegr(
    cl = 'regr.ksvmfp',
    package = 'kernlab',
    par.set = makeParamSet(
    ),
    properties = c('numerics', 'factors'),
    name = 'SVM with Tanimoto Kernel',
    short.name = 'ksvmfp',
    note = ''
  )
}

trainLearner.regr.ksvmfp <- function(.learner, .task, .subset, .weights = NULL,  ...) {
  dat1 <- getTaskData(.task, .subset)
  targs.name <- getTaskTargetNames(.task)
  dset.inp <- dat1[, - which(names(dat1)==targs.name)]
  dset.resp <- dat1[, targs.name]
  kfunction <- function()
  {
    k <- function (x1,y1)
    {
      require(fingerprint)
      nb <- length(x1)
      fp.x <- new('fingerprint', nbit=nb, bits=which(x1==1))
      fp.y <- new('fingerprint', nbit=nb, bits=which(y1==1))
      distance(fp.x, fp.y, method = 'tanimoto')
    }
    class(k) <- 'kernel'
    k
  }
  kernlab::ksvm(x = as.matrix(dset.inp), y = dset.resp, kernel = kfunction(), fit = F,scale=c())
}

predictLearner.regr.ksvmfp = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model,newdata =  as.matrix(.newdata))[, 1L]
}