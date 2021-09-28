#' @title Load and clean a dataset with all variables that were tested to model maturity.
#' @description Function to load and clean data: select maturity, LiDAR and topographic variables and normalize intensity metrics. Parcel area and distance to nearest road or forest track are also included, such as LiDAR flight and observed stand type.
#' @usage loadCompleteCleanData()
#' @param deleteGTGBna logical.
#' @return Return LiDAR metric and terrain releve data frames
#' @export
#' @importFrom BBmisc normalize
#' @examples terrain=loadCompleteCleanData(deleteGTGBna = F)[[1]]; metric=loadCompleteCleanData(deleteGTGBna = F)[[2]]
loadCompleteCleanData<-function(deleteGTGBna){

  ### Load files
  terrain=read.delim("./../data/Modeles_maturite/Points_mat_metrics_prealps_IFN_20200214_final.csv",sep=";",h=T)
  terrain=terrain[order(as.character(terrain$Placette)),]
  metric=read.delim("./../data/Modeles_maturite/Points_mat_metrics_prealps_IFN_20200214_final.csv",sep=";",h=T)
  mnt=read.table("./../data/Modeles_maturite/mntPts_complet.csv",sep=";")
  slope=read.table("./../data/Modeles_maturite/slopePts_complet.csv",sep=";")
  parcelles=read.table("./../data/Modeles_maturite/placette_jmm_parcellaire.csv",sep=";",h=T)


  ### Clean data keeping 'Source' column

  # Order rows
  terrain=terrain[order(as.character(terrain$Placette)),]
  metric=metric[order(as.character(metric$Placette)),]
  parcelles=parcelles[order(as.character(parcelles$placette)),]

  # deleted duplicated rows
  doublons=c(
    which(as.character(terrain$Placette)=="Protest_146")[2],
    which(as.character(terrain$Placette)=="LPO_7")[2],
    which(as.character(terrain$Placette)=="RBI_V_350")[2],
    which(as.character(terrain$Placette)=="Protest_161")[2],
    which(as.character(terrain$Placette)=="Protest_91")[2]
  )
  terrain=terrain[-doublons,]
  metric=metric[-doublons,]


  # Add area  (or a random column)
  metric=cbind(metric,parcelles$area)
  colnames(metric)[length(colnames(metric))]="Area.parc"
  colnames(metric)
  # metric=cbind(metric,metric[,1])

  # Select metrics
  # colnames(metric)
  # dim(metric)
  metric=metric[24:dim(metric)[2]]
  # colnames(metric)
  metric=metric[,-c(7:36,43:56,67:69)]
  # manque mCF, sdCH, "p.1st.hmin" , "p.hmin"
  # Tree.mea_1, Tree.mea_3, Tree.mea_3 = "Tree.meanCrownV"       "Tree.meanCanopyH"      "Tree.meanCrownH" ???
  dim(metric)
  # colnames(metric)
  metric=metric[,-c(4,6:8,11:12,17:18)]
  # Suppression de "zskew"      "zentropy"   "itot"       "imax"       "iskew"      "ikurt"      "TreeInf10." "TreeSup10."
  # dim(metric)
  # colnames(metric)
  metric=metric[,-c(11)]
  # Delete "Type_PEUP" et "Source"
  # dim(metric)
  # colnames(metric)
  # metric=metric[,-c(14:15)]

  # Add distance to nearest road
  nn=read.delim("./../data/Modeles_maturite/Proche_ancien.csv",sep=";")
  dim(nn)
  doublons=c(
    which(as.character(nn$trrn_Pl)=="Protest_146")[2],
    which(as.character(nn$trrn_Pl)=="LPO_7")[2],
    which(as.character(nn$trrn_Pl)=="RBI_V_350")[2],
    which(as.character(nn$trrn_Pl)=="Protest_161")[2],
    which(as.character(nn$trrn_Pl)=="Protest_91")[2]
  )
  doublons
  nn=nn[order(as.character(nn$trrn_Pl)),]
  nn=nn[-doublons,]
  metric=cbind(metric,nn[,c(3)])
  colnames(metric)[length(colnames(metric))]="near_dist"
  colnames(metric)

  # Add terrain variables
  mnt=mnt[order(mnt[,1]),]
  slope=slope[order(slope[,1]),]
  metric=cbind(metric,mnt[,2],slope[,2])
  colnames(metric)[(dim(metric)[2]-1):dim(metric)[2]]=c("mnt","slope")
  colnames(metric)

  # Normalize imean
  # table(terrain$Source)
  metric$imean[which(terrain$Source=="ain")]=BBmisc::normalize(metric$imean[which(terrain$Source=="ain")],method="standardize")
  metric$imean[which(terrain$Source=="bauges73")]=BBmisc::normalize(metric$imean[which(terrain$Source=="bauges73")],method="standardize")
  metric$imean[which(terrain$Source=="bauges74")]=BBmisc::normalize(metric$imean[which(terrain$Source=="bauges74")],method="standardize")
  metric$imean[which(terrain$Source=="chartreuse")]=BBmisc::normalize(metric$imean[which(terrain$Source=="chartreuse")],method="standardize")
  metric$imean[which(terrain$Source=="vercors4M")]=BBmisc::normalize(metric$imean[which(terrain$Source=="vercors4M")],method="standardize")
  metric$imean[which(terrain$Source=="vercorsRBI")]=BBmisc::normalize(metric$imean[which(terrain$Source=="vercorsRBI")],method="standardize")
  # boxplot(metric$imean)

  metric$isd[which(terrain$Source=="ain")]=BBmisc::normalize(metric$isd[which(terrain$Source=="ain")],method="standardize")
  metric$isd[which(terrain$Source=="bauges73")]=BBmisc::normalize(metric$isd[which(terrain$Source=="bauges73")],method="standardize")
  metric$isd[which(terrain$Source=="bauges74")]=BBmisc::normalize(metric$isd[which(terrain$Source=="bauges74")],method="standardize")
  metric$isd[which(terrain$Source=="chartreuse")]=BBmisc::normalize(metric$isd[which(terrain$Source=="chartreuse")],method="standardize")
  metric$isd[which(terrain$Source=="vercors4M")]=BBmisc::normalize(metric$isd[which(terrain$Source=="vercors4M")],method="standardize")
  metric$isd[which(terrain$Source=="vercorsRBI")]=BBmisc::normalize(metric$isd[which(terrain$Source=="vercorsRBI")],method="standardize")
  # boxplot(metric$isd)

  # Delete area variable (or random variable)
  # colnames(metric)
  # metric=metric[,-which(colnames(metric)=="metric[, 1]")]

  # Select terrain columns
  colnames(terrain)
  terrain=terrain[,c(1:22)]

  # Remove treemeanC correlated with tree.densi (-0.92)
  dim(metric) # 15
  colnames(metric)
  metric=metric[,-c(which(colnames(metric)=="Tree.meanC"))] # 0.85 threshold
  colnames(metric)

  # Define colnames
  xlabs=  c( "Zmax","Zmean","Zsd","Zkurt","Imean","Isd","Tree.meanH","Tree.sdH","Tree.giniH","Tree.density","TreeSup30.density","Tree.Canopy_cover","Field.stand_type","LiDAR.flight","Parcel.area","Nearest.road_distance","Elevation","Slope"  )
  colnames(metric)=xlabs

  # Delete NA value
  if(deleteGTGBna==T){
    delete.rows=which(is.na(terrain$GTGB)==T)
    terrain=terrain[-delete.rows,]
    metric=metric[-delete.rows,]
  }

  return(list(terrain,metric))
}
