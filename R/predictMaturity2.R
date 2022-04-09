#' @title Predict maturity from raw metric maps
#' @description Spatial prediction without filtering
#' @usage predictMaturity(model,modelName,dir)
#' @param model predictive model
#' @param outputModelName character used to define output names
#' @param metricsDir character, directory name to load metric maps
#' @param metricsDataName character, names of metric datasets
#' @import raster
#' @import randomForest
#' @export
#' @examples # not run: impt.replicates.ord=maturitymodelling::predictMaturity2(model,outputModelName,metricsDir,metricsDataName)
predictMaturity2=function(model,outputModelName,metricsDir,metricsDataName){

  for(i in 1:length(metricsDataName)){

    load(paste(metricsDir,metricsDataName[i],sep=""))
    names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
      row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]

    for(j in 1:length(model)){

      predicted=raster::predict(metrics.map,model[[j]],progress="text",type="response")
      raster::writeRaster(predicted,filename = paste(outputDir,"/",outputModelName[j],"_",strsplit(metricsDataName[i],split = ".rda")[[1]],sep=""),format="GTiff",prj=T,overwrite=T)

    }
  }
}
