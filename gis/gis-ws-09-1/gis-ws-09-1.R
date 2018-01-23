mainDir <- "E:/Master/gis_09/"
inDir <- "input/"
outDir <- "output/"
results<-"results/"
Fusion<-"E:/gIs/FUSION/"
library(sp)
library(raster)
library(rgdal)

i1=list.files(paste0(mainDir,inDir),pattern=".las$", full.names=FALSE)
#i=list.files(paste0(mainDir,inDir),pattern=".las$", full.names=FALSE)
d=80          #d=value equal to gridmetrics colnumber
y=c(35)       #y=value in gridmetrics colnumber
h=c(1,3,10)   #h=cellsize
Percentage=function(laslist,d,y){
  for (i in laslist){
    system(paste0(Fusion,"catalog.exe ",mainDir,inDir,i," ",
                  mainDir,outDir,i,".html"))
    extent<-read.csv(paste0(mainDir,outDir,i,".csv"))
    system(paste0(paste0(Fusion,"clipdata.exe"," /class:2 ",
                         mainDir,inDir,i," ",
                         mainDir,outDir,i,"_groundpts.las "),
                  paste(extent$MinX,extent$MinY,extent$MaxX,extent$MaxY)))
    system(paste0(Fusion,"gridsurfacecreate.exe ", 
                  mainDir,outDir,i,"_gridsurf.dtm "
                  ,"1 M M 1 32 0 0 ",mainDir, outDir,i,"_groundpts.las"))
    system(paste0(Fusion,"gridmetrics.exe ",mainDir,outDir,i,"_gridsurf.dtm ",0," " ,
                  1," ",mainDir,results,i,"_stattab.csv ",mainDir,inDir,i))
    for (j in y){
      system(paste0(Fusion,"csv2grid.exe ",mainDir,results,i,
                    "_stattab_all_returns_intensity_stats.csv ",j," ",mainDir,results,i,"_",d,"perc.asc"))
      tempo=raster(paste0(mainDir,results,i,"_",d, "perc.asc"))
      ex=extent(c(477248, 477468, 5631722,5631891))
      ras_crop <- crop(tempo,ex)
      projection(ras_crop) <- CRS("+init=epsg:25832")
      writeRaster(ras_crop,paste0(mainDir,results,i,"_",d,"perc."),format="GTiff")
    }
  }
}
CanopyHeightModel=function(laslist,h){
  for (i in laslist){
    for (j in h){
      system(paste0(Fusion, "CanopyModel.exe ","/ground:",mainDir,outDir,i,"_gridsurf.dtm ",
                    "/gridxy:477248,5631722,477468,5631891 ",
                    mainDir,results,i,"_",j,"m_CHM.dtm ",j," M M 1 32 0 0 ",mainDir,inDir,i))
      system(paste0(Fusion,"DTM2ASCII.exe ",mainDir,results,i,"_",j,"m_CHM.dtm ",mainDir,results,i,"_",j,"m_CHM.asc"))
      tempo=raster(paste0(mainDir,results,i,"_",j, "m_CHM.asc"))
      projection(tempo) <- CRS("+init=epsg:25832")
      writeRaster(tempo,paste0(mainDir,results,i,"_",j,"m_CHM."),format="GTiff")
    }
  }
}
TreeSeg=function(laslist,h,d){
  for (i in laslist){
    for (j in h){
      system(paste0(Fusion, "TreeSeg.exe ","/shape ",
                    mainDir,results,i,"_",j,"m_CHM.dtm ",mainDir,results,i,"_",d,"perc.tif ",
                    mainDir,results,i,"_",j, "_treeseg_",d,"perc.csv "))
      tempo=raster(paste0(mainDir,results,i,"_",j,"_treeseg_",d,"perc", "_Basin_Map.asc"))
      projection(tempo) <- CRS("+init=epsg:25832")
      writeRaster(tempo,paste0(mainDir,results,i,"_",j,"_",d,"_Basin_Map."),format="GTiff")
      shapes <- readOGR(paste0(mainDir,results,i,"_",j,"_treeseg_",d,"perc","_Polygons.shp"))
      projection(shapes) <- CRS("+init=epsg:25832")
      writeOGR(shapes,dsn=paste0(mainDir,results,i,"_",j,"m_",d,"perc","_Polygons.", ".shp"),driver = "ESRI Shapefile",layer="treesize")
      shapes <- readOGR(paste0(mainDir,results,i,"_",j,"_treeseg_",d,"perc","_HighPoints.shp"))
      projection(shapes) <- CRS("+init=epsg:25832")
      writeOGR(shapes,dsn=paste0(mainDir,results,i,"_",j,"m_",d,"perc","_HighPoints.shp", ".shp"),driver = "ESRI Shapefile",layer="treepoint")
    }
  }
}
Percentage(laslist = i1,d=d,y=y)
CanopyHeightModel(laslist = i1,h=h)
TreeSeg(laslist = i1,h=h,d=d)