#' @title Compute maturity indices
#' @description Compute maturity indices
#' @usage computeMaturityIndices(maturityAttributeData,attribute.selec,quant)
#' @param maturityAttributeData dataframe including maturity attributes
#' @param attribute.selec Colnames of maturity attributes
#' @param quant quantile used as a threshold to compute IMAT.
#' @return Return maturity index and attributes but only in absence of NA value
#' @export
#' @examples computeMaturityIndices(maturityAttributeData="terrain",attribute.selec=c("GTTGB", "GBMD30", "VBMS30"),quant=0.99)
computeMaturityIndices<-function(maturityAttributeData,attribute.selec,quant){

  maturityIndices=array(NA,dim = c(dim(maturityAttributeData)[1] , length(attribute.selec)))
  na.true=F

  for(i in 1:length(attribute.selec)){

    if(length(which(is.na(indice) == T)) > 0){
      print("NA value for maturity attribute")
      na.true=T
    }else{
      indice=as.numeric(maturityAttributeData[,which(colnames(maturityAttributeData)==attribute.selec[i])])
      indice[which(indice>quantile(indice,quant))]=quantile(indice,quant)
      maturityIndices[,i]=indice
    }

  }

  if(na.true==T){
    maturityIndices=NULL
  }else{
    IMAT=apply(maturityIndices, MARGIN = 1, mean)
    maturityIndices=cbind(IMAT,maturityIndices)
    colnames(maturityIndices)=c("IMAT",attribute.selec)
  }

  return(maturityIndices)

}
