#' @title Predict maturity from raw metric maps without any extrapolation
#' @description Filter metrics to predict NA in case of extrapolation
#' @usage predictMaturityExtra(model,modelName,dir)
#' @param model predictive model
#' @param modelName character used to define output names
#' @param dir character, directory name to load metric maps
#' @import raster
#' @import randomForest
#' @export
#' @examples # not run: predictMaturityExtra(model,modelName,dir)
predictMaturityExtra<-function(model,modelName,dir){

  maturityDB=read.table("./../data/plots/MaturityDB_17_11_2021_1_moy.csv",sep=";",h=T)
  maturityDB.selec=maturityDB[c(19:32)]

  load(paste(dir,"metrics_allBauges.rda",sep=""))
  names(metrics.map.pnr)[c(1,2,3,5,39,40,53,54,55,56,60,64,67,69)]=
    row.names(model$importance)[1:(length(row.names(model$importance)))]
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
  predictedIMAT_Bauges_metrics_filter=raster::predict(metrics.map.pnr,model,progress="text",type="response",na.rm=T)
  raster::writeRaster(predictedIMAT_Bauges_metrics_filter,filename = paste("./../data/outputs/predicted",modelName,"_Bauges_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)


  load(paste(dir,"metrics_allBugey.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model$importance)[1:(length(row.names(model$importance)))]
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
  predictedIMAT_Bugey_metrics_filter=raster::predict(metrics.map,model,progress="text",type="response",na.rm=T)
  raster::writeRaster(predictedIMAT_Bugey_metrics_filter,filename = paste("./../data/outputs/predicted",modelName,"_Bugey_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)


  load(paste(dir,"metrics_allChartreuse.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model$importance)[1:(length(row.names(model$importance)))]
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
  raster::writeRaster(predictedIMAT_Chartreuse_metrics_filter,filename = paste("./../data/outputs/predicted",modelName,"_Chartreuse_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  load(paste(dir,"metrics_allVercorsRBI.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model$importance)[1:(length(row.names(model$importance)))]
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
  predictedIMAT_VercorsRBI_metrics_filter=raster::predict(metrics.map,model,progress="text",type="response",na.rm=T)
  raster::writeRaster(predictedIMAT_VercorsRBI_metrics_filter,filename = paste("./../data/outputs/predicted",modelName,"_VercorsRBI_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)

  load(paste(dir,"metrics_allQuatreMontagnes.rda",sep=""))
  names(metrics.map)[c(1,2,3,5,39,40,53,54,55,56,60,64,66,68)]=
    row.names(model$importance)[1:(length(row.names(model$importance)))]
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
  predictedIMAT_QuatreMontagnes_metrics_filter=raster::predict(metrics.map,model,progress="text",type="response",na.rm=T)
  raster::writeRaster(predictedIMAT_QuatreMontagnes_metrics_filter,filename = paste("./../data/outputs/predicted",modelName,"_QuatreMontagnes_metrics_extra.tif",sep=""),format="GTiff",prj=T,overwrite=T)


}
