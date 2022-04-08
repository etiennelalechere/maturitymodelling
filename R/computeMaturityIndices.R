#' @title Compute maturity indices
#' @description Compute maturity indices
#' @usage computeMaturityIndices(maturityAttributeData,attribute.selec,quant)
#' @param maturityAttributeData dataframe including maturity attributes
#' @param attribute.selec Colnames of maturity attributes
#' @param quant quantile used as a threshold to compute IMAT.
#' @return Return maturity index and attributes.
#' @export
#' @examples computeMaturityIndices(maturityAttributeData="terrain",attribute.selec,quant=0.99)
computeMaturityIndices<-function(maturityAttributeData,attribute.selec,quant){

  namecol=rep(NA,length(attribute.selec))
  maturityIndices=rep(NA,length(attribute.selec))

  for(i in 1:length(attribute.selec)){

    indice=maturityAttributeData[,which(colnames(maturityAttributeData)==attribute.selec[i])]
    indice[which(indice>quantile(indice,quant))]=quantile(indice,quant)
    maturityIndices[,i]=indice
    colnames(maturityIndices)[i]=paste(attribute.selec[i],".Q",quant,sep="")

  }

  IMAT=apply(maturityIndices, MARGIN = 2, function(x){x / max(x)})
  IMAT=apply(maturityIndices, MARGIN = 1, mean)
  maturityIndices=cbind(IMAT,maturityIndices)
  colnames(maturityIndices)[1]="IMAT"

  return(maturityIndices)

}
