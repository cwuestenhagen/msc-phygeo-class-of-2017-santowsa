mainDir <- "E:/gIs/horizont/" 
inDir <- "input/"
outDir<- "output/"
results<-"hor_class/"
Fusion<-"E:/gIs/FUSION/"
library(sp)
library(raster)
library(rgdal)
rasterOptions(maxmemory=1e+08)
source(paste0(mainDir,"function_horizon.R"))

################################################################################

lasfiles<-list.files(paste0(mainDir,inDir),pattern=".las$", full.names=FALSE)
#einfügen #height und #cellsize und #col 
#bei uns dann height=0, cellsize=1 und col=38 bzw col=6 beio dgm, etc bla bla 
statistics(lasList = lasfiles)
DSM(lasList = lasfiles)
DGM(lasList = lasfiles)
CHM(lasList = lasfiles)
GAP(lasList = lasfiles)
SLP(lasList = lasfiles)