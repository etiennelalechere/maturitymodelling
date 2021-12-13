#' @title Crop forest to lidar survey extents
#' @description Crop forest to lidar survey extents and dissolve using "CODE_TFV" attribute
#' @usage cropForest(dir)
#' @param dir character, directory name to load BD FORET layers
#' @import raster
#' @import sf
#' @import dplyr
#' @export
#' @examples # not run: cropForest()

cropForest<-function(dir){

  foret38=sf::st_read(paste(dir2,"/38/FORMATION_VEGETALE.shp",sep=""), options = "ENCODING=WINDOWS-1252")
  foret73=sf::st_read(paste(dir2,"/73/FORMATION_VEGETALE.shp",sep=""), options = "ENCODING=WINDOWS-1252")
  foret26=sf::st_read(paste(dir2,"/26/FORMATION_VEGETALE.shp",sep=""), options = "ENCODING=WINDOWS-1252")
  foret01=sf::st_read(paste(dir2,"/01/FORMATION_VEGETALE.shp",sep=""), options = "ENCODING=WINDOWS-1252")
  foret74=sf::st_read(paste(dir2,"/74/FORMATION_VEGETALE.shp",sep=""), options = "ENCODING=WINDOWS-1252")

  # Crop forest layers to LiDAR survey and correct for overlapping polygons dissolving stands
  foret017374=rbind(foret01,foret73,foret74)
  predictedIMAT_Bugey=raster::raster("./../data/outputs/predictedIMAT_Bugey.tif")
  sf::st_transform(foret017374,sf::st_crs(predictedIMAT_Bugey))
  foret017374.crop=sf::st_crop(foret017374,raster::extent(predictedIMAT_Bugey))
  over=sf::st_overlaps(foret017374.crop)
  print("Overlapping polygons:")
  which(lapply(over,FUN=length)!=0)
  foret017374.crop.diss = foret017374.crop %>% group_by(CODE_TFV) %>% summarize()
  over=sf::st_overlaps(foret017374.crop.diss)
  print("Overlapping polygons:")
  which(lapply(over,FUN=length)!=0)

  foret2638=rbind(foret26,foret38)
  predictedIMAT_VercorsRBI=raster::raster("./../data/outputs/predictedIMAT_VercorsRBI.tif")
  sf::st_transform(foret2638,sf::st_crs(predictedIMAT_VercorsRBI))
  foret2638.crop=sf::st_crop(foret2638,raster::extent(predictedIMAT_VercorsRBI))
  over=sf::st_overlaps(foret2638.crop)
  print("Overlapping polygons:")
  which(lapply(over,FUN=length)!=0)
  foret2638.crop.diss.Vercors <- foret2638.crop %>% group_by(CODE_TFV) %>% summarize()
  over=sf::st_overlaps(foret2638.crop.diss.Vercors)
  print("Overlapping polygons:")
  which(lapply(over,FUN=length)!=0)

  predictedIMAT_QuatreMontagnes=raster::raster("./../data/outputs/predictedIMAT_QuatreMontagnes.tif")
  sf::st_transform(foret38,sf::st_crs(predictedIMAT_QuatreMontagnes))
  foret38.crop.QM=sf::st_crop(foret38,raster::extent(predictedIMAT_QuatreMontagnes))
  foret38.crop.diss.QM <- foret38.crop.QM %>% group_by(CODE_TFV) %>% summarize()

  predictedIMAT_Chartreuse=raster::raster("./../data/outputs/predictedIMAT_Chartreuse.tif")
  sf::st_transform(foret38,sf::st_crs(predictedIMAT_Chartreuse))
  foret38.crop.Chartreuse=sf::st_crop(foret38,raster::extent(predictedIMAT_Chartreuse))
  foret38.crop.diss.Chartreuse <- foret38.crop.Chartreuse %>% group_by(CODE_TFV) %>% summarize()

  foret7374=rbind(foret73,foret74)
  sf::st_transform(foret7374,sf::st_crs(predictedIMAT_Bauges))
  foret7374.crop=sf::st_crop(foret7374,raster::extent(predictedIMAT_Bauges))
  over=sf::st_overlaps(foret7374.crop)
  which(lapply(over,FUN=length)!=0)
  foret7374.crop.diss.Bauges <- foret7374.crop %>% group_by(CODE_TFV) %>% summarize()
  over=sf::st_overlaps(foret7374.crop.diss.Bauges)
  which(lapply(over,FUN=length)!=0)

  sf::st_write(foret017374.crop.diss,"./../data/outputs/forestBugey.shp")
  sf::st_write(foret2638.crop.diss.Vercors,"./../data/outputs/forestVercors.shp")
  sf::st_write(foret38.crop.diss.QM,"./../data/outputs/forestQuatreMontagnes.shp")
  foret38.crop.diss.Chartreuse2 <- sf::st_collection_extract(foret38.crop.diss.Chartreuse, "POLYGON")
  sf::st_write(foret38.crop.diss.Chartreuse2,"./../data/outputs/forestChartreuse.shp")
  sf::st_write(foret7374.crop.diss.Bauges,"./../data/outputs/forestBauges.shp")

}
