
#---------------------------------------------------------------#
#                       SCRIPT NUMBER 2                         #
#        SCRIPT TO OBTAIN FOREST AREAS BASED ON K-MEANS         #
#---------------------------------------------------------------#

# --- OBTAINING FOREST VEGETATION ZONES USING AUTOMATIC K-MEANS CLASSIFICATION CRITERIA,#
#     WHICH IS APPLIED TO THE RED BAND OF SATELLITE IMAGE --- #

# ------------------------------------------------------------------------------------- #

HUELLAS <- c("HUELLA_204030", "HUELLA_204031","HUELLA_205030")

for (huella in HUELLAS){
  
  if (huella == "HUELLA_204031") {
    
    setwd("C:/PERTURBACIONES_FOREST_GALI/AREA_ESTUDIO_SUR")
    limites <- st_read("GALICIA_SURR.shp")
    
    set_aoi(limites$geometry)
    view_aoi()
    
    extent(limites)
    
    st_crs(limites) <- 32629
    limites <- st_transform(limites, crs=32629)
    extent(limites)
    
  }else{
    
    setwd("C:/PERTURBACIONES_FOREST_GALI/AREA_ESTUDIO")
    limites <-st_read("GALICIA_NORTE_DEF.shp")
    
    set_aoi(limites$geometry)
    view_aoi()
    
    extent(limites)
    
    limites <- st_transform(limites, crs=32629)
    extent(limites)
    
  }
  #---------------------------------------------------------------#
  #---------------------------------------------------------------#
  
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DATOS_TOTAL/MULTI_BANDA_",substr(huella, start = 8, stop = 13)))
  
  imaxes <- dir()
  imaxes <- str_subset(imaxes, "tif$")
  
  #---------------------------------------------------------------#
  
  for (a in 1:length(imaxes)) {
    
    j <- imaxes[a]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DATOS_TOTAL/MULTI_BANDA_",substr(huella, start = 8, stop = 13)))
    IMAG1 <- stack(j)
    IMAG1 <- IMAG1[[4]]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK_NDVI_SIMPLE/MASK"))
    NDVI <- raster(j)
    
    #-------------#
    
    IMAG1<-crop(IMAG1,extent(limites))
    NDVI<-crop(NDVI,extent(limites))
    
    IMAG1.mask <- mask(IMAG1, NDVI)
    IMAG1.mask[is.na(IMAG1.mask)] <- -9999
    
    #-------------#
    
    valores <-getValues(IMAG1.mask)
    
    # --- THE ALGORITHM IS CREATED --- #
    # --- 6 CLASSES ARE CONSIDERED - 1 OF NULL VALUES --- #
    # --- MAX 500 INTERACTIONS --- #
    
    set.seed(1)
    cluster.kmeans <- kmeans(na.omit(valores), centers=6, iter.max=500, nstart=6,
                             algorithm = "Lloyd")
    
    #-------------#
    # THE VALUES OF THE CLASSIFICATION ARE INSERTED IN THE RASTER # 
    
    km.valores <- setValues(IMAG1.mask, cluster.kmeans$cluster)
    
    # --- EVALUATION OF RESULTS --- #
    
    cluster.kmeans$centers
    table(cluster.kmeans$cluster) # number of pixels #
    
    cluster.kmeans$withinss # pixel sum squared to center (+smallest best) #
    cluster.kmeans$betweenss # sum of square distance between two centers (+higher best) #
    
    km.valores_<-as.data.frame(km.valores)
    IMAG1.mask_<-as.data.frame(IMAG1.mask)
    no.super<-cbind(km.valores_,IMAG1.mask_)
    names(no.super) <- c("cluster","ValueBands")
    
    #-------------#
    no.super[no.super$ValueBands == -9999, ] <- NA
    na.omit(no.super)
    #-------------#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/BOXPLOT"))
    
    tiff(j)
    boxplot(no.super$ValueBands~no.super$cluster,
            ylab="Value Band",xlab="Number of cluster",
            main="Unsupervised Clasification by Kmeans")
    dev.off()
    
    #-------------#
    # ELBOW METHOD #
    
    vK<- 2:10
    pruebas <- 10 #repetición 10 veces
    
    media.tot.ss <-integer(length(vK)) #vector vacío 
    
    for(v in vK){
      v.tot.ss <- integer(pruebas)
      for(i in 1:pruebas){
        k.temp <- kmeans(na.omit(valores), centers=v, iter.max = 500)
        v.tot.ss[i] <-k.temp$tot.withinss
      }
      media.tot.ss[v-1] <- mean(v.tot.ss) #media de las 10 repeticiones
    }
    
    warnings()
    
    #-------------#
    # ELBOW CHART #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GRAFICAS"))
    
    tiff(j)
    plot(vK, media.tot.ss, type="b", main="ELBOW CURVE",
         ylab = "SUM OF THE SQUARED DISTANCES",
         xlab = "NUMBER OF CLUSTERS")
    dev.off()
    
    #-------------#
    # TO SAVE GTIFF #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GTIFF_CLASIF"))
    
    proj4string(km.valores) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    writeRaster(km.valores,
                filename = j,
                format = "GTiff",
                datatype= 'FLT4S')
    
  }
}


#================================================================================================================#
#--------------------------------------- BINARIZATION TO OBTAIN MASK 2  -----------------------------------------#
#================================================================================================================#
# --- CREATION OF CSV --- #

# --- THE NAMES OF EACH FILE ARE SAVED WITH CSV EXTENSION.#
# IT IS NECESSARY TO IDENTIFY THE RESPECTIVE CLUSTERS OF  #
# LOWER REFLECTANCE FROM THE BOXPLOT --- #

for (huella in HUELLAS){
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GTIFF_CLASIF"))
  
  csv_name <- dir()
  csv_name <- str_subset(csv_name, "tif$")
  
  csv_nameS_ <- c()
  
  for (nu in 1:length(csv_name)) {
    
    csv_name_ <- csv_name[nu]
    csv_name_ <- substr(csv_name_, start = 1, stop = 8)
    
    csv_nameS_[nu] <- csv_name_
  }
  
  csv_nameS_ <- as.data.frame(csv_nameS_)

  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_1"))
  csv_nameS_$V1 <- NA
  write.csv2(csv_nameS_, file = "ValuesMask_1Group.csv") 
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_2"))
  csv_nameS_$V2 <- NA
  write.csv2(csv_nameS_, file = "ValuesMask_2Group.csv")
  
}

# --- In these csv's both the values belonging to the first mask "V1" (cluster with lower reflectance) #
# and those of the second mask "V1 and V2" (two clusters with lower reflectance) must be placed --- #

#================================================================================================================#
# --- CREATION OF MASKS --- #

for (huella in HUELLAS){
  
  # --- LOAD FILES --- #
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GTIFF_CLASIF"))
  
  imaxes <- dir()
  imaxes <- str_subset(imaxes, "tif$")
  
  #--------------------#
  # --- FOR ONLY 1 CLUSTER --- #
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_1"))
  
  Clust_1Gr <- as.data.frame(read.csv("ValuesMask_1Group.csv", sep = ";"))
  mask.forestBandaR <- as.list(Clust_1Gr$V1)
  nrow(Clust_1Gr)
  
  for (m in 1:length(imaxes)){
    
    n <- imaxes[m]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GTIFF_CLASIF"))
    MASK2 <- raster(n)
    #-------#
    
    #-------#
    g <- mask.forestBandaR[[m]]
    
    a <- as.numeric(g-0.2)
    b <- as.numeric(g-0.1)
    c <- as.numeric(g+0.1)
    d <- as.numeric(g+0.2)
    #-------#  
    
    #-------#  
    MASK2 <- reclassify(MASK2, c(-Inf,a,NA, b,c,1, d,Inf,NA))
    #-------#
    
    proj4string(MASK2) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_1/GTIFF_MASK2_1"))
    writeRaster(MASK2,
                filename = n,
                format = "GTiff",
                datatype= 'FLT4S')
    
    #------# 
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_1/PLOT_MASK2_1"))
    tiff(n)
    plot(MASK2, col="blue",legend=FALSE, main= "Mask by K-Means for 1 Class")
    dev.off()
    
  }
  
  
  #--------------------#
  # --- FOR 2 CLUSTERS --- #
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_2"))
  
  Clust_2Gr <- as.data.frame(read.csv("ValuesMask_2Group.csv", sep = ";"))
  mask.forest_MATOBandaR <- list()
  
  #------# 
  for (m in 1:nrow(Clust_2Gr)){
    
    n_V <- c(Clust_2Gr[[3]][[m]],Clust_2Gr[[4]][[m]])
    mask.forest_MATOBandaR[[length(mask.forest_MATOBandaR) + 1]] <- n_V
    
  }
  
  #------# 
  
  for (p in 1:length(imaxes)){
    
    q <- imaxes[p]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GTIFF_CLASIF"))
    MASK2_2 <- raster(q)
    #-------#
    
    x <- mask.forest_MATOBandaR[[p]][[1]]
    y <- mask.forest_MATOBandaR[[p]][[2]]
    
    
    a1 <- as.numeric(x-0.2)
    b1 <- as.numeric(x-0.1)
    c1 <- as.numeric(x+0.1)
    d1 <- as.numeric(x+0.2)
    
    a2 <- as.numeric(y-0.2)
    b2 <- as.numeric(y-0.1)
    c2 <- as.numeric(y+0.1)
    d2 <- as.numeric(y+0.2)
    
    MASK2_2 <- reclassify(MASK2_2, c(-Inf,a1,NA, b1,c1,1, d1,a2,NA, b2,c2,1, d2,Inf,NA))
    
    #-------#
    proj4string(MASK2_2) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_2/GTIFF_MASK2_2"))
    writeRaster(MASK2_2,
                filename = q,
                format = "GTiff",
                datatype= 'FLT4S')
    
    #-------#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_2/PLOT_MASK2_2"))
    tiff(q)
    plot(MASK2_2, col="blue",legend=FALSE, main= "Mask by K-Means for 2 Classes")
    dev.off()
    
  }
}
  







