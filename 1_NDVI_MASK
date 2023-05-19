#---------------------------------------------------------------#
#                       SCRIPT NUMBER 1                         #
#        SCRIPT TO GENERATE A MASK FROM VALUES OF NDVI>0.2      #
#---------------------------------------------------------------#

HUELLAS <- c("HUELLA_204030", "HUELLA_204031","HUELLA_205030")

for (huella in HUELLAS){
  #---------------------------------------------------------------#
  if (huella == "HUELLA_204031") {
    
    setwd("C:/PERTURBACIONES_FOREST_GALI/AREA_ESTUDIO_SUR")
    limites_exact<-st_read("GALICIA_SURR.shp")
    
    set_aoi(limites_exact$geometry)
    view_aoi()
    
    extent(limites_exact)
    
    st_crs(limites_exact) <- 32629
    limites<- st_transform(limites_exact, crs=32629)
    extent(limites_exact)
    
    PSEUDO_MASK_GAL <- raster("GALICIA_SUR_MASK_FIJA.TIF")
    PSEUDO_MASK_GAL<-crop(PSEUDO_MASK_GAL,extent(limites))
    PSEUDO_MASK_GAL <- reclassify(PSEUDO_MASK_GAL, c(-Inf,0.5,1))
    
    
    PSEUDO_MASK <- raster("RIOS_SUR_MASK_FIJA.TIF")
    PSEUDO_MASK <- crop(PSEUDO_MASK,extent(limites))
    
  }else{
    
    setwd("C:/PERTURBACIONES_FOREST_GALI/AREA_ESTUDIO")
    limites<-st_read("GALICIA_NORTE_DEF.shp")
    
    set_aoi(limites$geometry)
    view_aoi()
    
    extent(limites)
    
    limites<- st_transform(limites, crs=32629)
    extent(limites)
    
    PSEUDO_MASK_GAL <- raster("PSEUDO_MASK_GAL_NORTE.TIF")
    PSEUDO_MASK_GAL<-crop(PSEUDO_MASK_GAL,extent(limites))
    PSEUDO_MASK_GAL <- reclassify(PSEUDO_MASK_GAL, c(-Inf,0.5,1))
    
    PSEUDO_MASK <- raster("PSEUDO_MASK_RIOS_GAL_NORTE.TIF")
    PSEUDO_MASK<-crop(PSEUDO_MASK,extent(limites))
  }
  #---------------------------------------------------------------#
  #---------------------------------------------------------------#
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DATOS_TOTAL/NDVI_",substr(huella, start = 8, stop = 13)))
  
  imaxes <- dir()
  imaxes <- str_subset(imaxes, "tif$")
  
  nomes <- c()
  
  for (nu in 1:length(imaxes)) {
    
    nom <- imaxes[nu]
    nom_ <- substr(nom, start = 6, stop = 17)
    
    nomes[nu] <- nom_
  }
  
  #---------------------------------------------------------------#
  
  for (i in 1:length(imaxes)) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DATOS_TOTAL/NDVI_",substr(huella, start = 8, stop = 13)))
    
    j <- imaxes[i]
    NDVI01 <- raster(j)
    
    b <- nomes[i] 
    
    #-------------#
    NDVI01<-crop(NDVI01,extent(limites))
    
    NDVI01 <- mask(NDVI01, PSEUDO_MASK_GAL)
    NDVI01 <- mask(NDVI01, PSEUDO_MASK)
    
    #-------------#
    
    NDVI01[NDVI01<-1] = -1
    NDVI01[NDVI01>1] = 1
    
    NDVI01[is.na(NDVI01)] <- -9999
    
    
    mask.NDVI01 <- reclassify(NDVI01, c(-Inf,0.21,NA, 0.21,Inf,1))
    
    #-------------# 
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK_NDVI_SIMPLE/PLOT"))
    tiff(b)
    plot(mask.NDVI01,col="red",legend=FALSE, main="Active Vegetation Mask ")
    dev.off()
    
    #--------------#
    
    proj4string(mask.NDVI01) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    #--------------#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK_NDVI_SIMPLE/MASK"))
    writeRaster(mask.NDVI01,
                filename = b,
                format = "GTiff",
                datatype= 'FLT4S')
    
  }
}
