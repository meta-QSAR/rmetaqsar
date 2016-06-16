#MetaQSAR research project.
#Author: Ivan Olier
#email: iaolier@gmail.com
#2013-2016

deeplrn.CV <- function(h2o.data, n.folds=10, data.splits = NULL){
  ncol.h2o.data <- ncol(h2o.data)
  n.input.var <- ncol.h2o.data-2
  input.vars.cols <- 2:(ncol.h2o.data-1)
  resp.var.col <- ncol.h2o.data
  if(!is.null(data.splits)){
    n.folds <- max(data.splits$fold)
    data.df <- as.data.frame(h2o.data)
  }
  
  preds <- lapply(1:n.folds, function(fold.x){
    if(!is.null(data.splits)){
      d.split <- list()
      cond.s <- data.df$molecule_id %in% data.splits$rows_id[data.splits$fold == fold.x]
      d.split[[1]] <- h2o.data[!cond.s,]
      d.split[[2]] <- h2o.data[cond.s,]
    } else d.split <- h2o.nFoldExtractor(h2o.data, n.folds, fold.x)
    mdl <- h2o.deeplearning(x=input.vars.cols, y=resp.var.col, data =d.split[[1]], validation = d.split[[2]],
                            hidden=c(ceiling(n.input.var/3),ceiling(n.input.var/9)), epochs=10, activation="Rectifier",
                            classification=FALSE, quiet_mode = T, key = paste0("mdl_",h2o.data@key,"_xval",fold.x))
    preds.tmp <- h2o.predict.df(h2o.model = mdl, h2o.newdata = d.split[[2]])
    preds.tmp$rep <- 1
    preds.tmp$fold <- fold.x
    preds.tmp <- preds.tmp[,c("rep","fold","rows_id","truth","prediction")]
    preds.tmp
  })
  preds.rmse <- sapply(preds, function(x) sqrt(sum((x$truth - x$prediction)^2)/nrow(x)) )
  preds <- do.call("rbind", preds)
  list(predictions = preds, rmse.mean = mean(preds.rmse), rmse.sd = sd(preds.rmse))
}

h2o.predict.df<-function(h2o.model, h2o.newdata){
  preds <- as.data.frame(h2o.predict(h2o.model, newdata = h2o.newdata))
  preds <- rename(preds, c("predict" = "prediction"))
  newdata <- as.data.frame(h2o.newdata[,c(1,ncol(h2o.newdata))])
  preds$rows_id <- newdata[[1]]
  preds$truth <- newdata[[2]]
  preds <- preds[,c("rows_id","truth","prediction")]
  preds
}

rm.h2o.keys <- function(h2o.server, pattern = NULL)
{
  if(is.null(pattern)) 
    keys <- h2o.ls(h2o.server)$Key
  else
    keys <- h2o.ls(h2o.server, pattern = pattern)$Key
  
  if (!is.null(keys))
    h2o.rm(h2o.server, keys)
  invisible(keys)
}

read.perffile <- function(dataset.id, implementation.id = NULL){
  dir.name <- paste0(getExpDir(), "perf_did_", dataset.id)
  if(is.null(implementation.id)) f.name <- list.files(dir.name)[1]
  else f.name <- paste0("perf_did_",dataset.id,"_impid_",implementation.id,".csv")
  if(!is.na(f.name)) return(read.csv(paste0(dir.name,"/",f.name)))
  return(NULL)
}