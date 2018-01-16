missingExtents<-function(catalogTable, Name){
  
  for (i in 3:nrow(catalogTable)){
    
    findrows <- which(catalogTable$MinX ==0|catalogTable$MinY==0|
                        catalogTable$MaxX==0|catalogTable$MaxY==0)
    
    for (j in findrows){
      coor<-substr(catalogTable[j,1],nchar(as.character(catalogTable[j,1]))-10, nchar(as.character(catalogTable[j,1])))
      
      xmin<-paste0(substr(coor, 1,3), "000")
      
      ymin<-paste0(substr(coor, 4,7), "000")
      
      catalogTable[j,]$MinX=as.numeric(xmin)
      
      catalogTable[j,]$MinY=as.numeric(ymin)
    }}
  write.csv(catalogTable,paste0(mainDir, outDir,Name,".csv"))
}
###########################################################################

statistics<-function(lasList){
  for (i in lasList){
    system(paste0(Fusion, "catalog.exe ",mainDir, inDir, i," ",
                  mainDir, outDir,i, ".html"))
    extent<-read.csv(paste0(mainDir, outDir,i,".csv"))
    if(extent$MinX==0){
      missingExtents(catalogTable=extent, Name=i)
      extent<-read.csv(paste0(mainDir, outDir,i,".csv"))
    }
    system(paste0(paste0(Fusion, "clipdata.exe"," /class:2 ",
                         mainDir, inDir, i," ",
                         mainDir, outDir,i, "_GroundPts.las "),
                  paste(extent$MinX,extent$MinY,extent$MaxX,extent$MaxY))
    )
    system(paste0(Fusion, "gridsurfacecreate.exe ", 
                  mainDir, outDir, i,"_GridSurf.dtm ",
                  "1 M M 1 32 0 0 ",mainDir, outDir,i, "_GroundPts.las"))
    system(paste0(Fusion, "gridmetrics.exe ",mainDir,outDir,i,"_GridSurf.dtm ",0," " ,1," " ,mainDir, results,i, 
                  "_vdr.csv ", mainDir,inDir,i))
  }}

#######################################################################################

DSM<-function(lasList){
  for (i in lasList){
    system(paste0(Fusion, "csv2grid.exe ", mainDir, results,i,"_vdr_all_returns_intensity_stats.csv ",38," ", 
                  mainDir, results, i,"_dsm.asc"))
    tempo=raster(paste0(mainDir,results,i, "_dsm.asc"))
    projection(tempo) <- CRS("+init=epsg:25832")
    writeRaster(tempo,paste0(mainDir,results,i, "_DSM."),format="GTiff" )
  }}

#######################################################################################

DGM<-function(lasList){
  for (i in lasList){
    system(paste0(Fusion, "csv2grid.exe ", mainDir, results,i,"_vdr_all_returns_intensity_stats.csv ",6," ", 
                  mainDir, results, i,"_dgm.asc"))
    tempo=raster(paste0(mainDir,results,i, "_dgm.asc"))
    projection(tempo) <- CRS("+init=epsg:25832")
    writeRaster(tempo,paste0(mainDir,results,i, "_DGM."),format="GTiff" )
  }}

#######################################################################################

CHM<-function(lasList){
  for (i in lasList){
    CH=(raster(paste0(mainDir,results,i,"_DSM.tif")))-
      (raster(paste0(mainDir,results,i,"_DGM.tif")))
    writeRaster(CH,paste0(mainDir,results,i,"_CH."),format="GTiff" )
    
    var_CH=focal(CH,w=matrix(1/81,nrow = 9,ncol = 9),na.rm=T,fun=var)
    writeRaster(var_CH,paste0(mainDir,results,i,"_CH_var."),format="GTiff" )
  }}

########################################################################################################

GAP<-function(lasList){
  for (i in lasList){
    system(paste0(Fusion, "csv2grid.exe ", mainDir, results,i,"_vdr_all_returns_intensity_stats.csv ",26," ", 
                  mainDir, results, i,"_gap.asc"))
    tempo=raster(paste0(mainDir,results,i, "_gap.asc"))
    projection(tempo) <- CRS("+init=epsg:25832")
    writeRaster(tempo,paste0(mainDir,results,i, "_GAP."),format="GTiff" )
    GaP=(raster(paste0(mainDir,results,i,"_GAP.tif")))
    mean_GaP=focal(GaP,w=matrix(1/81,nrow = 9,ncol = 9),na.rm=T,fun=mean)
    writeRaster(mean_GaP,paste0(mainDir,results,i, "_GaP_mean."),format="GTiff" )
    var_GaP=focal(GaP,w=matrix(1/81,nrow = 9,ncol = 9),na.rm=T,fun=var)
    writeRaster(var_GaP,paste0(mainDir,results,i, "_GaP_var."),format="GTiff" )
}}

#######################################################################################

SLP<-function(lasList){
  for (i in lasList){
    SlP=(raster(paste0(mainDir,results,i,"_CH.tif")))
    SlP_meaner=focal(SlP,w=matrix(1/625,nrow=25,ncol=25),na.rm=T,fun=mean)
    SlP_CH=terrain(SlP_meaner,opt="slope",unit="degrees",neighbors = 8)
    writeRaster(SlP_CH,paste0(mainDir,results,i, "_SlP."),format="GTiff" )
}}

##########################################################################################