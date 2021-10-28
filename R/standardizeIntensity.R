#' @title Standardize intensity by LiDAR flight
#' @description Standardization and export of mean and sd values
#' @usage standardizeIntensity(maturity.db)
#' @param maturity.db data frame or matrix
#' @return Return maturity database
#' @export
#' @examples # Not run (tables not available in package): maturity.db.stand=standardizeIntensity(maturity.db)
standardizeIntensity<-function(maturity.db){

  # maturity.db.backup=maturity.db
  norm.param=array(NA,dim=c(length(table(maturity.db$Chantier)),4))
  row.names(norm.param)=names(table(maturity.db$Chantier))
  colnames(norm.param)=c("Imean_mean","Imean_sd","Isd_mean","Isd_sd")

  for(i in 1:length(table(maturity.db$Chantier))){

    norm.param[i,]=c(
      mean(maturity.db$Imean[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),
      sd(maturity.db$Imean[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),
      mean(maturity.db$Isd[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),
      sd(maturity.db$Isd[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))])
    )

    maturity.db$Imean[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]=
      (
        round(mean(maturity.db$Imean[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),digits=6)-
          (maturity.db$Imean[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))])
      ) /   round(sd(maturity.db$Imean[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),digits=6)

    maturity.db$Isd[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]=
      (
        round(mean(maturity.db$Isd[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),digits=6)-
          (maturity.db$Isd[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))])
      ) /  round(sd(maturity.db$Isd[which(maturity.db$Chantier==names(table(maturity.db$Chantier)[i]))]),digits=6)

  }

  # Check for reproductibility (valid)
  # id=which(maturity.db$Chantier=="Geneve")
  # check.true=maturity.db$Imean[id]
  # mean(check.true)
  # sd(check.true)
  # norm.param
  # check=( maturity.db.backup$Imean[id] -  40.352901 ) / 12.657110
  # mean(check)
  # sd(check)

  write.table(norm.param,"./data/output/Intensity_standardization_parameters.csv",col.names=T,row.names=T,sep=";",dec=".")

  return(maturity.db)

}
