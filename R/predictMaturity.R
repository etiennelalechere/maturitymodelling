#' @title Predict maturity from raw metric maps
#' @description Spatial prediction without filtering
#' @usage predictMaturity(model,modelName,dir)
#' @param model predictive model
#' @param modelName character used to define output names
#' @param dir character, directory name to load metric maps
#' @import raster
#' @import randomForest
#' @export
#' @examples # not run: impt.replicates.ord=maturitymodelling::predictMaturity(model=protestModel_IMAT,modelName="IMAT",dir=dir)
predictMaturity=function(model,modelName,dir){

  load(paste(dir,"metrics_allChartreuse.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model$importance)[1:(length(row.names(model$importance)))]
  predictedIMAT_Chartreuse=raster::predict(metrics.map,model,progress="text",type="response")
  raster::writeRaster(predictedIMAT_Chartreuse,filename = paste("./../data/outputs/predicted",modelName,"_Chartreuse.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  load(paste(dir,"metrics_allVercorsRBI.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(protestModel_IMAT$importance)[1:(length(row.names(protestModel_IMAT$importance)))]
  predictedIMAT_VercorsRBI=raster::predict(metrics.map,protestModel_IMAT,progress="text",type="response")
  raster::writeRaster(predictedIMAT_VercorsRBI,filename = paste("./../data/outputs/predicted",modelName,"_VercorsRBI.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  load(paste(dir,"metrics_allQuatreMontagnes.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(protestModel_IMAT$importance)[1:(length(row.names(protestModel_IMAT$importance)))]
  predictedIMAT_QuatreMontagnes=raster::predict(metrics.map,protestModel_IMAT,progress="text",type="response")
  raster::writeRaster(predictedIMAT_QuatreMontagnes,filename = paste("./../data/outputs/predicted",modelName,"_QuatreMontagnes.tif",sep=""),format="GTiff",prj=T,overwrite=T)


  load(paste(dir,"metrics_allBugey.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(protestModel_IMAT$importance)[1:(length(row.names(protestModel_IMAT$importance)))]
  predictedIMAT_Bugey=raster::predict(metrics.map,protestModel_IMAT,progress="text",type="response")
  raster::writeRaster(predictedIMAT_Bugey,filename = paste("./../data/outputs/predicted",modelName,"_Bugey.tif",sep=""),format="GTiff",prj=T,overwrite=T)


  load(paste(dir,"metrics_allBauges.rda",sep=""))
  names(metrics.map.pnr)[c(1,2,3,5,39,40,53,54,55,56,60,64,67,69)]=
    row.names(protestModel_IMAT$importance)[1:(length(row.names(protestModel_IMAT$importance)))]
  predictedIMAT_Bauges=raster::predict(metrics.map.pnr,protestModel_IMAT,progress="text",type="response")
  raster::writeRaster(predictedIMAT_Bauges,filename = paste("./../data/outputs/predicted",modelName,"_Bauges.tif",sep=""),format="GTiff",prj=T,overwrite=T)

}
