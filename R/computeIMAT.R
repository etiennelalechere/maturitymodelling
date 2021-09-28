#' @title Compute maturity indices
#' @description Compute the maturity index described in Furh et al. (in prep.) and maturity attributes (living tree basal area, standing deadwood basal area, lying deadwood volume).
#' @usage computeIMAT(useGTTGB,diameter,quant)
#' @param useGTTGB logical, use GTGBGTTGB if set to false.
#' @param diameter 20cm or 30cm threshold for standing and lying deadwood.
#' @param quant quantile used as a threshold to compute IMAT.
#' @return Return maturity index and attributes.
#' @export
#' @examples computeIMAT(useGTTGB=T,diameter=30,quant=0.99)
computeIMAT<-function(useGTTGB,diameter,quant){

  if(useGTTGB==T){
    GTTGB=terrain$GTTGB # param
  }else{
    GTTGB=terrain$GTGBTTGB
  }
  if(diameter==20){
    GBMD=terrain$GBMD20
    VBMS=terrain$VBMS20
  }
  if(diameter==30){
    GBMD=terrain$GBMD30
    VBMS=terrain$VBMS30
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

  return(maturityIndices)

}
