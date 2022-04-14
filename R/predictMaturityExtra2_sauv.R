#' @title Predict maturity from raw metric maps without any extrapolation
#' @description Filter metrics to predict NA in case of extrapolation
#' @usage predictMaturityExtra(model,modelName,dir)
#' @param maturityDB.selec matrix or data frame, metrics database to define ranges
#' @param model predictive model
#' @param metricsDir character, directory name to load metric maps
#' @param outputModelName character used to define output names
#' @param outputDir character, directory name to export maturity maps
#' @import raster
#' @import randomForest
#' @export
#' @examples # not run: predictMaturityExtra2(maturityDB.selec,model,metricsDir,outputDir,outputModelName)
predictMaturityExtra2<-function(maturityDB.selec,model,metricsDir,outputDir,outputModelName){

  load(paste(metricsDir,"metrics_allBauges_cor.rda",sep=""))
  names(metrics.map.pnr)[c(1,2,3,5,39,40,53,54,55,56,60,64,67,69)]=
    row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]
  metrics.map.pnr=metrics.map.pnr[[c(1,2,3,5,39,40,53,54,55,56,60,64,67,69)]]
  for(i in 1:dim(metrics.map.pnr)[3]){
    if( length(
      which(
        raster::getValues(metrics.map.pnr[[i]]) > max(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map.pnr[[i]] [
        which(
          raster::getValues(metrics.map.pnr[[i]]) > max(maturityDB.selec[,i])
        )
      ]=NA
    }
    if( length(
      which(
        raster::getValues(metrics.map.pnr[[i]]) < min(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map.pnr[[i]] [
        which(
          raster::getValues(metrics.map.pnr[[i]]) < min(maturityDB.selec[,i])
        )
      ]=NA
    }
  }

  for(j in 1:length(model)){
    predicted_filter=raster::predict(metrics.map.pnr,model[[j]],progress="text",type="response",na.rm=T)
    raster::writeRaster(predicted_filter,filename = paste(outputDir,"/",outputModelName[j],"_Bauges_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)
  }


  load(paste(metricsDir,"metrics_allBugey.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]
  metrics.map=metrics.map[[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]]
  for(i in 1:dim(metrics.map)[3]){
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
        )
      ]=NA
    }
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
        )
      ]=NA
    }
  }
  for(j in 1:length(model)){
    predicted_filter=raster::predict(metrics.map.pnr,model[[j]],progress="text",type="response",na.rm=T)
    raster::writeRaster(predicted_filter,filename = paste(outputDir,"/",outputModelName[j],"_Bugey_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)
  }


  load(paste(metricsDir,"metrics_allChartreuse.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]
  metrics.map=metrics.map[[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]]
  for(i in 1:dim(metrics.map)[3]){
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
        )
      ]=NA
    }
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
        )
      ]=NA
    }
  }
  predictedIMAT_Chartreuse_metrics_filter=raster::predict(metrics.map,model,progress="text",type="response",na.rm=T)
  for(j in 1:length(model)){
    predicted_filter=raster::predict(metrics.map.pnr,model[[j]],progress="text",type="response",na.rm=T)
    raster::writeRaster(predicted_filter,filename = paste(outputDir,"/",outputModelName[j],"_Chartreuse_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)
  }

  load(paste(metricsDir,"metrics_allVercorsRBI.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]
  metrics.map=metrics.map[[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]]
  for(i in 1:dim(metrics.map)[3]){
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
        )
      ]=NA
    }
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
        )
      ]=NA
    }
  }
  for(j in 1:length(model)){
    predicted_filter=raster::predict(metrics.map.pnr,model[[j]],progress="text",type="response",na.rm=T)
    raster::writeRaster(predicted_filter,filename = paste(outputDir,"/",outputModelName[j],"_VercorsRBI_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)
  }

  load(paste(metricsDir,"metrics_allQuatreMontagnes.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model[[j]]$importance)[1:(length(row.names(model[[j]]$importance)))]
  metrics.map=metrics.map[[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]]
  for(i in 1:dim(metrics.map)[3]){
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) > max(maturityDB.selec[,i])
        )
      ]=NA
    }
    if( length(
      which(
        raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
      )
    )
    >0){
      metrics.map[[i]] [
        which(
          raster::getValues(metrics.map[[i]]) < min(maturityDB.selec[,i])
        )
      ]=NA
    }
  }
  for(j in 1:length(model)){
    predicted_filter=raster::predict(metrics.map.pnr,model[[j]],progress="text",type="response",na.rm=T)
    raster::writeRaster(predicted_filter,filename = paste(outputDir,"/",outputModelName[j],"_QuatreMontagnes_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)
  }


}
