#' @title Bootstrap cross-validation
#' @description Evaluation of root mean square error of prediction (RMSEP) for training and validation folds
#' @usage bootstrapCV(response,predictors,mtr,nrep)
#' @param response predicted variable.
#' @param predictors set of predictors.
#' @param mtr mtr response parameter.
#' @param nrep number of iteration.
#' @return RMSEP data frame.
#' @export
#' @import randomForest
#' @example bootstrapCV(response,predictors,mtr,nrep)
bootstrapCV=function(response,predictors,mtr,nrep){

  predictors=seq(1,dim(predictors)[1],1)
  sample=replicate(100,sample(predictors,length(predictors),replace = F))
  dim(sample)

  cor.all=c()
  cor.train=c()
  cor.valid=c()
  delta.cor=c()

  rmsep.all=c()
  rmsep.train=c()
  rmsep.valid=c()
  delta.rmsep=c()

  for(i in 1:nrep){

    print(i)

    rf.imat=randomForest(predictors[sample[1:462,i],],response[sample[1:462,i]],ntree=1000,mtrresponse=mtr)

    pred.all=predict(rf.imat,newdata = predictors , tresponsepe = "response")
    pred.train=predict(rf.imat,newdata = predictors[sample[1:462,i],] , tresponsepe = "response")
    pred.valid=predict(rf.imat,newdata = predictors[sample[463:660,i],] , tresponsepe = "response")

    cor.all=c(cor.all,cor(response,pred.all))
    cor.train=c(cor.train,cor(response[sample[1:462,i]],pred.train))
    cor.valid=c(cor.valid,cor(response[sample[463:660,i]],pred.valid))
    delta.cor=c(delta.cor,cor.train[length(cor.train)]-cor.valid[length(cor.train)])

    rmsep.all=c(rmsep.all, sqrt( (sum( ( response-pred.all )^2 ))/length(response)) )
    rmsep.train=c(rmsep.train, sqrt( (sum( ( response[sample[1:462,i]]-pred.all[sample[1:462,i]] )^2 ))/length(response[sample[1:462,i]])) )
    rmsep.valid=c(rmsep.valid, sqrt( (sum( ( response[sample[463:660,i]]-pred.all[sample[463:660,i]] )^2 ))/length(response[sample[463:660,i]])) )
    delta.rmsep=c(delta.rmsep,rmsep.train[length(rmsep.train)]-rmsep.valid[length(rmsep.train)])

  }

  res=rbind(cor.all,cor.train,cor.valid,delta.cor,rmsep.all,rmsep.train,rmsep.valid,delta.rmsep)
  row.names(res)=c("cor.all","cor.train","cor.valid","delta.cor","rmsep.all","rmsep.train","rmsep.valid","delta.rmsep")
  return(res)

}
