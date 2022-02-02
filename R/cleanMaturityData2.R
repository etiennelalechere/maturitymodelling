#' @title Clean database including Protest data
#' @description Correct plot names, remove duplicated plots and select defined predictor variables
#' @usage cleanMaturityData(terrain,metrics,var.selec,var.names)
#' @param terrain data frame or matrix
#' @param metrics data frame or matrix
#' @param var.selec character, names of the variables to keep in the database
#' @param var.names character, to rename variables (optional)
#' @return Return maturity database
#' @export
#' @examples # Not run (tables not available in package): maturity.db=cleanMaturityData(terrain,metrics,var.selec,var.names)
cleanMaturityData<-function(terrain,metrics,var.selec,var.names){

  # Optional naming of variables
  if(is.null(var.names)==T){var.names=var.selec}

  # Check matching of plot names in the field and the metric tables
  length(which(terrain$Placette %in% metrics$Placette == T))
  length(which(metrics$Placette %in%  terrain$Placette == T))
  subset(terrain$Placette,subset = terrain$Placette %in% metrics$Placette == F) # no matching for Protest & RBI plots, A2197, ANC09
  subset(metrics$Placette,subset = metrics$Placette %in%  terrain$Placette == F)
  metrics$Placette


  # Correction of plot names
  metric.plot.names=sub("_73_ONF","",metrics$Placette)
  metric.plot.names=sub("_74_ONF","",metric.plot.names)
  metric.plot.names=sub("_73_IRSTEA","",metric.plot.names)
  metric.plot.names=sub("_74_IRSTEA","",metric.plot.names)
  subset(metric.plot.names,subset = metric.plot.names %in%  terrain$Placette == F)
  metric.plot.names=sub("_IRSTEA","",metric.plot.names)
  subset(metric.plot.names,subset = metric.plot.names %in%  terrain$Placette == F)
  length(which(terrain$Placette %in% metric.plot.names == T))
  length(which(metric.plot.names %in%  terrain$Placette == T))
  subset(terrain$Placette,subset = terrain$Placette %in% metric.plot.names == F) # 13 plots removed
  # [1] "ANC09"       "RBI_V_40"    "RBI_V_94"    "RBI_V_67"    "RBI_V_174"   "RBI_V_65"    "RBI_V_351"   "RBI_V_93"
  # [9] "RBI_V_350"   "RBI_V_350"   "RBI_V_380"   "RBI_V_66"    "Protest_341"
  metrics$Placette=metric.plot.names


  # Select plots available in both tables
  terrain=terrain[terrain$Placette %in% metric.plot.names,]
  metrics=metrics[metrics$Placette %in% terrain$Placette,]


  # Remove duplicated plots
  terrain$Placette[duplicated(terrain$Placette)]
  # [1] "Protest_146" "LPO_7"       "Protest_91"  "Protest_161" "Protest_15"  "Protest_195" "Protest_25"
  metrics$Placette[duplicated(metrics$Placette)]
  # [1] "EM_1"        "EM_10"       "EM_13"       "EM_23"       "EM_29"       "EM_52"       "EM_56"       "EM_58"
  # [9] "EM_60"       "EM_84"       "EM_85"       "EM_9"        "EM_91"       "HB49"        "PR101"       "PR62"
  # [17] "PR94"        "Protest_101" "Protest_114" "Protest_137" "Protest_137" "Protest_139" "Protest_14"  "Protest_148"
  # [25] "Protest_15"  "Protest_159" "Protest_170" "Protest_178" "Protest_179" "Protest_195" "Protest_247" "Protest_25"
  # [33] "Protest_285" "Protest_46"  "Protest_48"  "Protest_49"  "Protest_50"  "Protest_52"  "Protest_62"  "Protest_93"
  # [41] "Protest_99"
  terrain=terrain[duplicated(terrain$Placette)==F,]
  metrics=metrics[duplicated(metrics$Placette)==F,]
  length(which(terrain$Placette %in% metrics$Placette == T))
  length(which(metrics$Placette %in%  terrain$Placette == T))
  terrain=terrain[order(as.character(terrain$Placette)),]
  metrics=metrics[order(as.character(metrics$Placette)),]
  identical(terrain$Placette,metrics$Placette)


  # Select variables
  colnames(metrics)
  metrics=metrics[,var.selec]
  colnames(metrics)= var.names
  colnames(terrain)
  terrain.selec=c("Placette","Massif","Jeu","Source","Annee","G_Tot","G_resineux","G_feuillus","GTGBTTGB","GTTGB","GBMD20","GBMD30","VBMS20","VBMS30","RMQ")
  terrain=terrain[,terrain.selec]


  # Check for NA value for maturity attributes
  print("NA for GTTGB")
  print(which(is.na(terrain$GTTGB)==T))
  print("NA for GBMD")
  print(which(is.na(terrain$GBMD30)==T))
  print("NA for VBMS")
  print(which(is.na(terrain$VBMS30)==T))


  # Combine both data sets
  maturity.db=merge(terrain,metrics,by = "Placette")


  return(maturity.db)

}
