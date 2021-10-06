#' @title Compute maturity indices
#' @description Compute the maturity index described in Furh et al. (in prep.) and maturity attributes (living tree basal area, standing deadwood basal area, lying deadwood volume).
#' @usage computeIMAT(useGTTGB,maturityAttributeData,diameter,quant)
#' @param useGTTGB logical, use GTGBGTTGB if set to false.
#' @param maturityAttributeData dataframe including maturity attributes, colnames must be "GTTGB", "GTGBTTGB", "GBMD20", "VBMS20", "GBMD30" or "VBMS30"
#' @param diameter 20cm or 30cm threshold for standing and lying deadwood.
#' @param quant quantile used as a threshold to compute IMAT.
#' @return Return maturity index and attributes.
#' @export
#' @examples computeIMAT(useGTTGB=TRUE,maturityAttributeData="terrain",diameter=30,quant=0.99)
computeIMAT<-function(useGTTGB,maturityAttributeData,diameter,quant){

  if(is.null(dim(maturityAttributeData))==F){

    if(useGTTGB==T){
        GTTGB=maturityAttributeData[,which(colnames(maturityAttributeData)=="GTTGB")]
    }else{
      GTTGB=maturityAttributeData[,which(colnames(maturityAttributeData)=="GTGBTTGB")]
    }
    if(diameter==20){
      GBMD=maturityAttributeData[,which(colnames(maturityAttributeData)=="GBMD20")]
      VBMS=maturityAttributeData[,which(colnames(maturityAttributeData)=="VBMS20")]
    }
    if(diameter==30){
      GBMD=maturityAttributeData[,which(colnames(maturityAttributeData)=="GBMD30")]
      VBMS=maturityAttributeData[,which(colnames(maturityAttributeData)=="VBMS30")]
    }

    GTTGB[which(GTTGB>quantile(GTTGB,quant))]=quantile(GTTGB,quant)
    GBMD[which(GBMD>quantile(GBMD,quant))]=quantile(GBMD,quant)
    VBMS[which(VBMS>quantile(VBMS,quant))]=quantile(VBMS,quant)

    IMAT=(
      GTTGB/max(GTTGB)+
        GBMD/max(GBMD)+
        VBMS/max(VBMS)
    ) / 3

    maturityIndices=data.frame(IMAT,GTTGB,GBMD,VBMS)
    if(useGTTGB==F){
      colnames(maturityIndices)[2]="GTGBGTTGB"
    }
  }else{
    maturityIndices="Invalid maturityAttributeData dataframe."
  }

  return(maturityIndices)

}
