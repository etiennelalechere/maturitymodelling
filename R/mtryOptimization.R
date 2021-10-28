#' @title mtry optimization
#' @description Function to define mtry random forest parameter.
#' @usage mtryOptimization(response,predictors,nrep)
#' @param response predicted variable.
#' @param predictors set of predictors.
#' @param nrep number of iteration.
#' @return Plot the mtry value that minimize the mean out-of-bag error rate.
#' @export
#' @import randomForest
#' @examples # mtryOptimization(response,predictors,100)
mtryOptimization<-function(response,predictors,nrep,...){

  rf_tuned=tuneRF(predictors, response, ntreeTry=1000, improve=0.05, plot=F, doBest=FALSE)
  for(i in 1:100){
    print(i)
    rf_tuned=cbind(rf_tuned,tuneRF(predictors, response, ntreeTry=1000, improve=0.05, plot=F, doBest=FALSE))
  }
  rf_tuned
  tun=apply(rf_tuned[,-1],FUN=mean,MARGIN=1)
  plot(tun~c(2,4,8),ylab="OOBError",xlab="mtry")
  lines(tun~c(2,4,8))

  return(tun)

}
