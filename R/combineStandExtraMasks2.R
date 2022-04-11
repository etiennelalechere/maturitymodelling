#' @title Combine stand and extrapolation filters
#' @description Spatial prediction without extrapolation and for selected stands
#' @usage combineStandExtraMasks(modelName)
#' @param modelName character used to define output names
#' @import raster
#' @export
#' @examples # not run: combineStandExtraMasks("IMAT")
combineStandExtraMasks2=function(outputModelName,outputDir){

  for(j in 1:length(outputModelName)){


    standBauges=raster::raster(paste(outputDir,outputModelName[j],"_Bauges_stands.tif",sep=""))
    extraBauges=raster::raster(paste(outputDir,outputModelName[j],"_Bauges_metrics_extra.tif",sep=""))
    extraBauges[which(is.na(raster::getValues(standBauges)==T))]=NA
    raster::writeRaster(extraBauges,filename = paste(outputDir,outputModelName[j],"_Bauges_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

    standBugey=raster::raster(paste(outputDir,outputModelName[j],"_Bugey_stands.tif",sep=""))
    extraBugey=raster::raster(paste(outputDir,outputModelName[j],"_Bugey_metrics_extra.tif",sep=""))
    extraBugey[which(is.na(raster::getValues(standBugey)==T))]=NA
    raster::writeRaster(extraBugey,filename = paste(outputDir,outputModelName[j],"_Bugey_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

    standChartreuse=raster::raster(paste(outputDir,outputModelName[j],"_Chartreuse_stands.tif",sep=""))
    extraChartreuse=raster::raster(paste(outputDir,outputModelName[j],"_Chartreuse_metrics_extra.tif",sep=""))
    extraChartreuse[which(is.na(raster::getValues(standChartreuse)==T))]=NA
    raster::writeRaster(extraChartreuse,filename = paste(outputDir,outputModelName[j],"_Chartreuse_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

    standVercorsRBI=raster::raster(paste(outputDir,outputModelName[j],"_VercorsRBI_stands.tif",sep=""))
    extraVercorsRBI=raster::raster(paste(outputDir,outputModelName[j],"_VercorsRBI_metrics_extra.tif",sep=""))
    extraVercorsRBI[which(is.na(raster::getValues(standVercorsRBI)==T))]=NA
    raster::writeRaster(extraVercorsRBI,filename = paste(outputDir,modelName,"_VercorsRBI_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

    standQuatreMontagnes=raster::raster(paste(outputDir,modelName,"_QuatreMontagnes_stands.tif",sep=""))
    extraQuatreMontagnes=raster::raster(paste(outputDir,modelName,"_QuatreMontagnes_metrics_extra.tif",sep=""))
    extraQuatreMontagnes[which(is.na(raster::getValues(standQuatreMontagnes)==T))]=NA
    raster::writeRaster(extraQuatreMontagnes,filename = paste(outputDir,modelName,"_QuatreMontagnes_stands_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  }

}
