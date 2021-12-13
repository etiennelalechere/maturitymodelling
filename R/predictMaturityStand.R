#' @title Filter maturity prediction according to stand composition
#' @description Spatial prediction with stand mask
#' @usage predictMaturityStand(modelName)
#' @param modelName character used to define output names
#' @import raster
#' @export
#' @examples # not run: predictMaturityStand("IMAT")
predictMaturityStand=function(modelName){

  forestBauges=sf::st_read("./../data/outputs/forestBauges.shp")
  forestBauges.selec <- forestBauges %>%
    dplyr::filter(CODE_TFV == "FF1-00-00" |
                    CODE_TFV == "FF1-09-09" |
                    CODE_TFV == "FF2G61-61" |
                    CODE_TFV == "FF31" |
                    CODE_TFV == "FF32"
    )
  predictedIMAT_Bauges=raster::raster("./../data/outputs/predictedIMAT_Bauges.tif")
  predictedIMAT_Bauges_stands=raster::mask(predictedIMAT_Bauges,forestBauges.selec)
  raster::writeRaster(predictedIMAT_Bauges_stands,filename = paste("./../data/outputs/predicted",modelName,"_Bauges_stands.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  forestBugey=sf::st_read("./../data/outputs/forestBugey.shp")
  forestBugey.selec <- forestBugey %>%
    dplyr::filter(CODE_TFV == "FF1-00-00" |
                    CODE_TFV == "FF1-09-09" |
                    CODE_TFV == "FF2G61-61" |
                    CODE_TFV == "FF31" |
                    CODE_TFV == "FF32"
    )
  predictedIMAT_Bugey=raster::raster("./../data/outputs/predictedIMAT_Bugey.tif")
  predictedIMAT_Bugey_stands=raster::mask(predictedIMAT_Bugey,forestBugey.selec)
  raster::writeRaster(predictedIMAT_Bugey_stands,filename = paste("./../data/outputs/predicted",modelName,"_Bugey_stands.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  forestChartreuse=sf::st_read("./../data/outputs/forestChartreuse.shp")
  forestChartreuse.selec <- forestChartreuse %>%
    dplyr::filter(CODE_TFV == "FF1-00-00" |
                    CODE_TFV == "FF1-09-09" |
                    CODE_TFV == "FF2G61-61" |
                    CODE_TFV == "FF31" |
                    CODE_TFV == "FF32"
    )
  predictedIMAT_Chartreuse=raster::raster("./../data/outputs/predictedIMAT_Chartreuse.tif")
  predictedIMAT_Chartreuse_stands=raster::mask(predictedIMAT_Chartreuse,forestChartreuse.selec)
  raster::writeRaster(predictedIMAT_Chartreuse_stands,filename = paste("./../data/outputs/predicted",modelName,"_Chartreuse_stands.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  forestVercorsRBI=sf::st_read("./../data/outputs/forestVercors.shp")
  forestVercorsRBI.selec <- forestVercorsRBI %>%
    dplyr::filter(CODE_TFV == "FF1-00-00" |
                    CODE_TFV == "FF1-09-09" |
                    CODE_TFV == "FF2G61-61" |
                    CODE_TFV == "FF31" |
                    CODE_TFV == "FF32"
    )
  predictedIMAT_VercorsRBI=raster::raster("./../data/outputs/predictedIMAT_VercorsRBI.tif")
  predictedIMAT_VercorsRBI_stands=raster::mask(predictedIMAT_VercorsRBI,forestVercorsRBI.selec)
  raster::writeRaster(predictedIMAT_VercorsRBI_stands,filename = paste("./../data/outputs/predicted",modelName,"_VercorsRBI_stands.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  forestQuatreMontagnes=sf::st_read("./../data/outputs/forestQuatreMontagnes.shp")
  forestQuatreMontagnes.selec <- forestQuatreMontagnes %>%
    dplyr::filter(CODE_TFV == "FF1-00-00" |
                    CODE_TFV == "FF1-09-09" |
                    CODE_TFV == "FF2G61-61" |
                    CODE_TFV == "FF31" |
                    CODE_TFV == "FF32"
    )
  predictedIMAT_QuatreMontagnes=raster::raster("./../data/outputs/predictedIMAT_QuatreMontagnes.tif")
  predictedIMAT_QuatreMontagnes_stands=raster::mask(predictedIMAT_QuatreMontagnes,forestQuatreMontagnes.selec)
  raster::writeRaster(predictedIMAT_QuatreMontagnes_stands,filename = paste("./../data/outputs/predicted",modelName,"_QuatreMontagnes_stands.tif",sep=""),format="GTiff",prj=T,overwrite=T)

}
