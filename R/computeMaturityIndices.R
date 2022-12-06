#' @title Compute maturity indices
#' @description Compute maturity indices
#' @usage computeMaturityIndices(maturityAttributeData,attribute.selec,quant)
#' @param maturityAttributeData dataframe including maturity attributes
#' @param attribute.selec Colnames of maturity attributes
#' @param quant quantile used as a threshold to compute IMAT.
#' @return Return maturity index and attributes but only in absence of NA value
#' @export
#' @examples # not run: computeMaturityIndices(maturityAttributeData=terrain,attribute.selec=c("GTTGB", "GBMD30", "VBMS30"),quant=0.99)
computeMaturityIndices<-function(maturityAttributeData,attribute.selec,quant){

  maturityIndices=array(NA,dim = c(dim(maturityAttributeData)[1] , length(attribute.selec)))
  colnames(maturityIndices)=rep(NA,length(attribute.selec))
  na.true=F

  # Quantile threshold

  for(i in 1:length(attribute.selec)){

    indice=as.numeric(maturityAttributeData[,which(colnames(maturityAttributeData)==attribute.selec[i])])

    if(length(which(is.na(indice) == T)) > 0){
      print("NA value for maturity attribute")
      na.true=T
    }else{
      indice[which(indice>quantile(indice,quant))]=quantile(indice,quant)
      maturityIndices[,i]=indice
      colnames(maturityIndices)[i]=paste(attribute.selec[i],"_Q",as.character(quant),sep="")

    }

  }

  # Relative index
  maturityIndices2=apply(maturityIndices, 2, function(x){x/max(x)})

  if(na.true==T){
    maturityIndices=NULL
  }else{
    IMAT=apply(maturityIndices2, MARGIN = 1, mean)
    maturityIndices=cbind(IMAT,maturityIndices)
    colnames(maturityIndices)[1]="IMAT"
  }

  return(maturityIndices)

}
