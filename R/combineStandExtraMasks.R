#' @title Combine stand and extrapolation filters
#' @description Spatial prediction without extrapolation and for selected stands
#' @usage combineStandExtraMasks(modelName)
#' @param modelName character used to define output names
#' @import raster
#' @export
#' @examples # not run: combineStandExtraMasks("IMAT")
combineStandExtraMasks=function(modelName){

  standBauges=raster::raster("./../data/outputs/predictedIMAT_Bauges_stands.tif")
  extraBauges=raster::raster("./../data/outputs/predictedIMAT_Bauges_metrics_extra.tif")
  extraBauges[which(is.na(raster::getValues(standBauges)==T))]=NA
  raster::writeRaster(extraBauges,filename = paste("./../data/outputs/predicted",modelName,"_Bauges_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  standBugey=raster::raster("./../data/outputs/predictedIMAT_Bugey_stands.tif")
  extraBugey=raster::raster("./../data/outputs/predictedIMAT_Bugey_metrics_extra.tif")
  extraBugey[which(is.na(raster::getValues(standBugey)==T))]=NA
  raster::writeRaster(extraBugey,filename = paste("./../data/outputs/predicted",modelName,"_Bugey_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  standChartreuse=raster::raster("./../data/outputs/predictedIMAT_Chartreuse_stands.tif")
  extraChartreuse=raster::raster("./../data/outputs/predictedIMAT_Chartreuse_metrics_extra.tif")
  extraChartreuse[which(is.na(raster::getValues(standChartreuse)==T))]=NA
  raster::writeRaster(extraChartreuse,filename = paste("./../data/outputs/predicted",modelName,"_Chartreuse_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  standVercorsRBI=raster::raster("./../data/outputs/predictedIMAT_VercorsRBI_stands.tif")
  extraVercorsRBI=raster::raster("./../data/outputs/predictedIMAT_VercorsRBI_metrics_extra.tif")
  extraVercorsRBI[which(is.na(raster::getValues(standVercorsRBI)==T))]=NA
  raster::writeRaster(extraVercorsRBI,filename = paste("./../data/outputs/predicted",modelName,"_VercorsRBI_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  standQuatreMontagnes=raster::raster("./../data/outputs/predictedIMAT_QuatreMontagnes_stands.tif")
  extraQuatreMontagnes=raster::raster("./../data/outputs/predictedIMAT_QuatreMontagnes_metrics_extra.tif")
  extraQuatreMontagnes[which(is.na(raster::getValues(standQuatreMontagnes)==T))]=NA
  raster::writeRaster(extraQuatreMontagnes,filename = paste("./../data/outputs/predicted",modelName,"_QuatreMontagnes_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

}
