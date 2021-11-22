#' @title Bootstrap cross-validation (70%,30%)
#' @description Evaluation of root mean square error of prediction (RMSEP) for training and validation folds
#' @usage bootstrapCV(response,predictors,mtr,nrep)
#' @param response predicted variable.
#' @param predictors set of predictors.
#' @param mtr mtr response parameter.
#' @param nrep number of iteration.
#' @param cor.method character, "spearman" or "pearson"
#' @return RMSEP data frame.
#' @export
#' @import randomForest
#' @example bootstrapCV(response,predictors,mtr,nrep)
bootstrapCV=function(response,predictors,mtr,nrep,cor.method){

  seq.id=seq(1,dim(predictors)[1],1)
  samples=replicate(100,sample(seq.id,length(seq.id),replace = F))
  dim(sample)

  cor.all=c()
  cor.train=c()
  cor.valid=c()
  delta.cor=c()

  rmsep.all=c()
  rmsep.train=c()
  rmsep.valid=c()
  delta.rmsep=c()

  dat70perc=ceiling(70*length(IMAT)/100)

  for(i in 1:nrep){

    print(i)

    rf.imat=randomForest(predictors[samples[1:dat70perc,i],],response[samples[1:dat70perc,i]],ntree=1000,mtrresponse=mtr)

    pred.all=predict(rf.imat,newdata = predictors , tresponsepe = "response")
    pred.train=predict(rf.imat,newdata = predictors[samples[1:dat70perc,i],] , tresponsepe = "response")
    pred.valid=predict(rf.imat,newdata = predictors[samples[(dat70perc+1):length(IMAT),i],] , tresponsepe = "response")

    cor.all=c(cor.all,cor(response,pred.all,method=cor.method))
    cor.train=c(cor.train,cor(response[samples[1:dat70perc,i]],pred.train))
    cor.valid=c(cor.valid,cor(response[samples[(dat70perc+1):length(IMAT),i]],pred.valid))
    delta.cor=c(delta.cor,cor.train[length(cor.train)]-cor.valid[length(cor.train)])

    rmsep.all=c(rmsep.all, sqrt( (sum( ( response-pred.all )^2 ))/length(response)) )
    rmsep.train=c(rmsep.train, sqrt( (sum( ( response[samples[1:dat70perc,i]]-pred.all[samples[1:dat70perc,i]] )^2 ))/length(response[samples[1:dat70perc,i]])) )
    rmsep.valid=c(rmsep.valid, sqrt( (sum( ( response[samples[(dat70perc+1):length(IMAT),i]]-pred.all[samples[(dat70perc+1):length(IMAT),i]] )^2 ))/length(response[samples[(dat70perc+1):length(IMAT),i]])) )
    delta.rmsep=c(delta.rmsep,rmsep.train[length(rmsep.train)]-rmsep.valid[length(rmsep.train)])

  }

  res=rbind(cor.all,cor.train,cor.valid,delta.cor,rmsep.all,rmsep.train,rmsep.valid,delta.rmsep)
  row.names(res)=c("cor.all","cor.train","cor.valid","delta.cor","rmsep.all","rmsep.train","rmsep.valid","delta.rmsep")
  return(res)

}
