#' @title Stabilize conditional importance values
#' @description Compute conditional importance with replicates
#' @usage stabilizeImportance(impt.init,nrep)
#' @param impt.init output of the function varImpPlot used to include the include the "true" importance related to the initial calibrated RF model
#' @param nrep integer, number of replicates
#' @return Return importance data frame
#' @export
#' @import randomForest
#' @examples # not run: impt.replicates.ord=maturitymodelling::stabilizeImportance(impt.init,nrep)
stabilizeImportance=function(impt.init,nrep){
  impt.replicates=array(NA,dim=c(nrep,length(impt.init)))
  colnames(impt.replicates)=names(impt.init)
  impt.replicates[1,]=impt.init
  for(i in 1:(nrep-1)){
    # print(paste( (round(10/(i+1),digits=0)*100) , "%",sep=""))
    print(i)
    rf.IMAT=randomForest::randomForest(metrics, IMAT,ntree=1000,importance=T,mtry=2)
    impt.replicates[(i+1),]=randomForest::varImpPlot(rf.IMAT)[,1]
  }
  impt.median=apply(impt.replicates,2,median)
  impt.replicates.ord=impt.replicates[,order(impt.median,decreasing=T)]
  return(impt.replicates.ord)
}
