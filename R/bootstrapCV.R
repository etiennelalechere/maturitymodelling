#' @title Bootstrap cross-validation
#' @description Evaluation of root mean square error of prediction (RMSEP) for training and validation folds
#' @usage bootstrapCV(Y,mtr,n,...)
#' @param Y predicted variable.
#' @param X set of predictors.
#' @param mtr mtry parameter.
#' @param nrep number of iteration.
#' @return RMSEP data frame.
#' @export
#' @import randomForest
#' @examples bootstrapCV(IMAT,metric,2,100)
bootstrapCV=function(Y,X,mtr,nrep,...){

  x=seq(1,dim(X)[1],1)
  sample=replicate(100,sample(x,length(x),replace = F))
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

    rf.imat=randomForest(X[sample[1:462,i],],Y[sample[1:462,i]],ntree=1000,mtry=mtr)

    pred.all=predict(rf.imat,newdata = X , type = "response")
    pred.train=predict(rf.imat,newdata = X[sample[1:462,i],] , type = "response")
    pred.valid=predict(rf.imat,newdata = X[sample[463:660,i],] , type = "response")

    cor.all=c(cor.all,cor(Y,pred.all))
    cor.train=c(cor.train,cor(Y[sample[1:462,i]],pred.train))
    cor.valid=c(cor.valid,cor(Y[sample[463:660,i]],pred.valid))
    delta.cor=c(delta.cor,cor.train[length(cor.train)]-cor.valid[length(cor.train)])

    rmsep.all=c(rmsep.all, sqrt( (sum( ( Y-pred.all )^2 ))/length(Y)) )
    rmsep.train=c(rmsep.train, sqrt( (sum( ( Y[sample[1:462,i]]-pred.all[sample[1:462,i]] )^2 ))/length(Y[sample[1:462,i]])) )
    rmsep.valid=c(rmsep.valid, sqrt( (sum( ( Y[sample[463:660,i]]-pred.all[sample[463:660,i]] )^2 ))/length(Y[sample[463:660,i]])) )
    delta.rmsep=c(delta.rmsep,rmsep.train[length(rmsep.train)]-rmsep.valid[length(rmsep.train)])

  }

  res=rbind(cor.all,cor.train,cor.valid,delta.cor,rmsep.all,rmsep.train,rmsep.valid,delta.rmsep)
  row.names(res)=c("cor.all","cor.train","cor.valid","delta.cor","rmsep.all","rmsep.train","rmsep.valid","delta.rmsep")
  return(res)

}
