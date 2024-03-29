#' @title Filter maturity prediction according to forest mask
#' @description Spatial prediction with forest mask
#' @usage predictMaturity(modelName)
#' @param modelName character used to define output names
#' @import raster
#' @export
#' @examples # not run: predictMaturityForest("IMAT")
predictMaturityForest=function(modelName){

  mask=sf::st_read('./../data/outputs/forestBauges.shp')
  predictedIMAT_Bauges=raster::raster(paste("./../data/outputs/predicted",modelName,"_Bauges.tif",sep=""))
  predictedIMAT_Bauges_forests=raster::mask(predictedIMAT_Bauges,mask)
  raster::writeRaster(predictedIMAT_Bauges_forests,filename = paste("./../data/outputs/predicted",modelName,"_Bauges_forest.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  mask=sf::st_read('./../data/outputs/forestBugey.shp')
  predictedIMAT_Bugey=raster::raster(paste("./../data/outputs/predicted",modelName,"_Bugey.tif",sep=""))
  predictedIMAT_Bugey_forests=raster::mask(predictedIMAT_Bugey,mask)
  raster::writeRaster(predictedIMAT_Bugey_forests,filename = paste("./../data/outputs/predicted",modelName,"_Bugey_forest.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  mask=sf::st_read('./../data/outputs/forestChartreuse.shp')
  predictedIMAT_Chartreuse=raster::raster(paste("./../data/outputs/predicted",modelName,"_Chartreuse.tif",sep=""))
  predictedIMAT_Chartreuse_forests=raster::mask(predictedIMAT_Chartreuse,mask)
  raster::writeRaster(predictedIMAT_Chartreuse_forests,filename = paste("./../data/outputs/predicted",modelName,"_Chartreuse_forest.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  mask=sf::st_read('./../data/outputs/forestQuatreMontagnes.shp')
  predictedIMAT_QuatreMontagnes=raster::raster(paste("./../data/outputs/predicted",modelName,"_QuatreMontagnes.tif",sep=""))
  predictedIMAT_QuatreMontagnes_forests=raster::mask(predictedIMAT_QuatreMontagnes,mask)
  raster::writeRaster(predictedIMAT_QuatreMontagnes_forests,filename = paste("./../data/outputs/predicted",modelName,"_QuatreMontagnes_forest.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  mask=sf::st_read('./../data/outputs/forestVercors.shp')
  predictedIMAT_VercorsRBI=raster::raster(paste("./../data/outputs/predicted",modelName,"_VercorsRBI.tif",sep=""))
  predictedIMAT_VercorsRBI_forests=raster::mask(predictedIMAT_VercorsRBI,mask)
  raster::writeRaster(predictedIMAT_VercorsRBI_forests,filename = paste("./../data/outputs/predicted",modelName,"_VercorsRBI_forest.tif",sep=""),format="GTiff",prj=T,overwrite=T)
}
