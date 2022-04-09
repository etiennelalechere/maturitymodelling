#' @title Filter maturity prediction according to a pre-defined stand composition
#' @description Spatial prediction with stand mask
#' @usage predictMaturityStand(modelName)
#' @param model predictive model
#' @param outputModelName character used to define output names
#' @param metricsDir character, directory name to load metric maps
#' @param metricsDataName character, names of metric datasets
#' @param forestDir character, directory name to load forest shapefiles
#' @param forestShape character, names of forest shapefiles, must be ordered according to metricsDataName
#' @import raster
#' @import randomForest
#' @export
#' @examples # not run: maturitymodelling::predictMaturityStand2(model,outputModelName,metricsDir,metricsDataName,forestDir,forestShape)
predictMaturityStand2=function(model,outputModelName,metricsDir,metricsDataName,forestDir,forestShape){

  for(i in 1:length(metricsDataName)){

    load(paste(metricsDir,metricsDataName[i],sep=""))
    names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
      row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]

    forest=sf::st_read(paste(forestDir,forestShape[i],sep=""))
    forest.selec <- forest %>%
      dplyr::filter(CODE_TFV == "FF1-00-00" |
                      CODE_TFV == "FF1-09-09" |
                      CODE_TFV == "FF2G61-61" |
                      CODE_TFV == "FF31" |
                      CODE_TFV == "FF32"
      )

    for(j in 1:length(model)){

      predicted=raster::predict(metrics.map,model[[j]],progress="text",type="response")

      predicted.msk=raster::mask(predicted,forest.selec)

      raster::writeRaster(predicted.msk,filename = paste(outputDir,"/",outputModelName[j],"_",strsplit(metricsDataName[i],split = ".rda")[[1]],"_stands",sep=""),format="GTiff",prj=T,overwrite=T)


    }
  }
}
