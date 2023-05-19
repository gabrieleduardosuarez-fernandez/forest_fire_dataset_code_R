
#----------------------------------------------------------------------#
#                          SCRIPT NUMBER 6                             #
#      FINAL GROUPING OF K-MEANS TO VERIFY IF IT IS FIRE OR NOT        #
#----------------------------------------------------------------------#

huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (HUELLA in huella_list) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
  
  nomes <- dir()
  nomes <- str_subset(nomes, "shp$")
  
  for (i in 1:length(nomes)) {
    
    shape <- nomes[i]
    
    #································································#
    # -------------- IMPORT OF DEFINITIVE PERIMETERS --------------- #
    #································································#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
    PERIMETRO_GENERAL <- st_read(shape)
    
    
    #································································#
    # ------- RASTER IMPORT MULTIBAND IMAGES RGB AND INFRA --------- #
    #································································#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/DATOS_TOTAL/MULTI_BANDA",substr(HUELLA, start = 7, stop = 13)))
    IMAGEN_BANDAS <- stack(paste0(substr(shape, start = 1, stop = 8),".TIF"))
    
    proj4string(IMAGEN_BANDAS) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    
    # --- TRIMMING AND MASKING --- #  
    
    IMAGEN_BANDAS_MASK <- mask(IMAGEN_BANDAS,PERIMETRO_GENERAL)
    IMAGEN_BANDAS_MASK <- crop(IMAGEN_BANDAS_MASK, PERIMETRO_GENERAL)
    
    
    # --- SELECTION OF INTERESTING BANDS - SWIR1 Y NIR --- # 
    
    IMAGEN_BANDAS_MASK_SWIR <- IMAGEN_BANDAS_MASK[[2]]
    IMAGEN_BANDAS_MASK_SWIR[IMAGEN_BANDAS_MASK_SWIR>0.5] = 0.5
    IMAGEN_BANDAS_MASK_SWIR[is.na(IMAGEN_BANDAS_MASK_SWIR)] <- -9999
    
    
    IMAGEN_BANDAS_MASK_NIR <- IMAGEN_BANDAS_MASK[[3]]
    IMAGEN_BANDAS_MASK_NIR[IMAGEN_BANDAS_MASK_NIR>0.5] = 0.5
    IMAGEN_BANDAS_MASK_NIR[is.na(IMAGEN_BANDAS_MASK_NIR)] <- -9999
    
    
    # ================================================================================================#
    #·································································································#
    # ---------------------------- K-MEANS CLASIFICATION FOR SWIR_1 Y NIR --------------------------- #
    #·································································································#
    # ================================================================================================#
    
    # --- FOR SWIR1 --- #
    
    Value <-getValues(IMAGEN_BANDAS_MASK_SWIR)
    
    #THE ALGORITHM IS CREATED. 
    #6 CLASSES ARE CONSIDERED - 1 CLASS OF NULL VALUES
    #MAX 500 INTERACTIONS 
    set.seed(1)
    cluster.kmeans <- kmeans(na.omit(Value), centers=6, iter.max=500, nstart=6,
                             algorithm = "Lloyd")
    
    
    #THE VALUES OF THE CLASSIFICATION ARE INTRODUCED IN THE RASTER# 
    km.valores <- setValues(IMAGEN_BANDAS_MASK_SWIR, cluster.kmeans$cluster)
    
    
    #RESULTS EVALUATION#
    
    cluster.kmeans$centers
    table(cluster.kmeans$cluster) #number of pixels#
    
    cluster.kmeans$withinss # pixel sum squared to center (+smallest best) #
    cluster.kmeans$betweenss # sum of square distance between two centers (+higher best) #
    
    
    km.valores_<-as.data.frame(km.valores)
    IMAGEN_BANDAS_MASK_SWIR_<-as.data.frame(IMAGEN_BANDAS_MASK_SWIR)
    no.super<-cbind(km.valores_,IMAGEN_BANDAS_MASK_SWIR_)
    names(no.super) <- c("cluster","ValueBands")
    
    #---------------------------------------------#
    no.super[no.super$ValueBands == -9999, ]<- NA
    na.omit(no.super)
    #---------------------------------------------#
    
    #SAVE BOXPLOT#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_SWIR/BOXPLOT"))
    
    tiff(paste0(substr(shape, start = 1, stop = 8),".TIF"))
    boxplot(no.super$ValueBands~no.super$cluster,
            ylab="Value Band",xlab="Number of cluster",
            main="Unsupervised Clasification by Kmeans - SWIR")
    abline(h=0.23,col='red', lwd = 2)
    dev.off()
    
    
    # TO SAVE GTIFF #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_SWIR/GTIFF_CLUSTER_KMEANS"))
    
    writeRaster(km.valores,
                filename = paste0(substr(shape, start = 1, stop = 8),".TIF"),
                format = "GTiff",
                datatype= 'FLT4S')
    
    
    #--------------------------------------------------#
    #SHAPE PERIMETERS - CLASSIFIED ACCORDING TO CLUSTER#
    
    CLASIFIC_NIVELES_SWIR <- km.valores
    
    Sampling_BANDA_SWIR <-  raster::extract(CLASIFIC_NIVELES_SWIR, PERIMETRO_GENERAL, fun = modal, df = TRUE, sp =TRUE)
    Sampling_BANDA_SWIR <-  as(Sampling_BANDA_SWIR, "sf")
    
    
    #SAVE SHAPE CLASSIFIED BY CLUSTER#
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_SWIR/SHAPE_PERI_CLUSTERIZADO"))
    
    st_write(Sampling_BANDA_SWIR,
             nomes[i])
    
    
    #-------------------------------------------#
    #-------------------------------------------#
    
    # --- FOR NIR --- #
    
    Value <-getValues(IMAGEN_BANDAS_MASK_NIR)
    
    
    #THE ALGORITHM IS CREATED. 
    #6 CLASSES ARE CONSIDERED - 1 CLASS OF NULL VALUES
    #MAX 500 INTERACTIONS 
    set.seed(1)
    cluster.kmeans <- kmeans(na.omit(Value), centers=6, iter.max=500, nstart=6,
                             algorithm = "Lloyd")
    
    
    # THE VALUES OF THE CLASSIFICATION ARE INSERTED IN THE RASTER # 
    km.valores <- setValues(IMAGEN_BANDAS_MASK_NIR, cluster.kmeans$cluster)
    
    
    #RESULTS EVALUATION#
    
    cluster.kmeans$centers
    table(cluster.kmeans$cluster) # number of pixels #
    
    cluster.kmeans$withinss # pixel sum squared to center (+smallest best) #
    cluster.kmeans$betweenss # sum of square distance between two centers (+higher best) #
    
    
    km.valores_<-as.data.frame(km.valores)
    IMAGEN_BANDAS_MASK_NIR_<-as.data.frame(IMAGEN_BANDAS_MASK_NIR)
    no.super<-cbind(km.valores_,IMAGEN_BANDAS_MASK_NIR_)
    names(no.super) <- c("cluster","ValueBands")
    
    #---------------------------------------------#
    no.super[no.super$ValueBands == -9999, ]<- NA
    na.omit(no.super)
    #---------------------------------------------#
    
    #SAVE BOXPLOT#
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_NIR/BOXPLOT"))
    
    tiff(paste0(substr(shape, start = 1, stop = 8),".TIF"))
    boxplot(no.super$ValueBands~no.super$cluster,
            ylab="Value Band",xlab="Number of cluster",
            main="Unsupervised Clasification by Kmeans - NIR")
    abline(h=0.20,col='red', lwd = 2)
    dev.off()
    
    
    # TO SAVE GTIFF #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_NIR/GTIFF_CLUSTER_KMEANS"))
    
    writeRaster(km.valores,
                filename = paste0(substr(shape, start = 1, stop = 8),".TIF"),
                format = "GTiff",
                datatype= 'FLT4S')
    
    #-------------------------------------------#
    #SHAPE PERIMETERS - CLASSIFIED ACCORDING TO CLUSTER#
    
    CLASIFIC_NIVELES_NIR <- km.valores
    
    Sampling_BANDA_NIR <-  raster::extract(CLASIFIC_NIVELES_NIR, PERIMETRO_GENERAL, fun = modal, df = TRUE, sp =TRUE)
    Sampling_BANDA_NIR <-  as(Sampling_BANDA_NIR, "sf")
    
   
    #SAVE SHAPE CLASSIFIED BY CLUSTER#
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_NIR/SHAPE_PERI_CLUSTERIZADO"))
    
    st_write(Sampling_BANDA_NIR,
             nomes[i])
    
  }
  
}



#_____________________________________________________________________________________________________________________________________#

#=====================================================================================================================================#
#------------------------------------------------------ CREATION OF CSV --------------------------------------------------------------#
#=====================================================================================================================================#

# --- THE NAMES OF EACH FILE ARE SAVED WITH CSV EXTENSION.#
# IT IS NECESSARY TO IDENTIFY THE RESPECTIVE CLUSTERS OF  #
# HIGHER REFLECTANCE THAN 0.25 FOR SWIR AND 0.21 FOR NIR  #
# FROM THE BOXPLOT --- #

for (huella in huella_list){
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
  
  csv_name <- dir()
  csv_name <- str_subset(csv_name, "shp$")
  
  csv_nameS_ <- c()
  
  for (nu in 1:length(csv_name)) {
    
    csv_name_ <- csv_name[nu]
    csv_name_ <- substr(csv_name_, start = 1, stop = 8)
    
    csv_nameS_[nu] <- csv_name_
  }
  
  csv_nameS_ <- as.data.frame(csv_nameS_)
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_NIR"))
  csv_nameS_$V5 <- NA
  csv_nameS_$V4 <- NA
  csv_nameS_$V3 <- NA
  csv_nameS_$V2 <- NA
  csv_nameS_$V1 <- NA
  write.csv2(csv_nameS_, file = "valoresNoCorresp_NIR.csv") 
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_SWIR"))
  write.csv2(csv_nameS_, file = "valoresNoCorresp_SWIR.csv")
  
}

# --- In these csv should be placed all the clusters that do not meet the condition for SWIR and NIR -
# Empty values must be filled with zero ---#


#_____________________________________________________________________________________________________________________________________#

#=====================================================================================================================================#
#------------------------------------------- OBTAINING THE MASK FIRE ACCORDING TO KMEANS ---------------------------------------------#
#=====================================================================================================================================#

# THE RESPECTIVE K-MEANS VALUES ARE USED TO ELIMINATE THOSE AREAS OR REGIONS THAT DO NOT BELONG TO A FOREST FIRE, #
# BY CONSIDERING THE AREAS WITH HIGHER REFLECTANCE THAN THE CONDITIONS FOR SWIR1 (KMEANS>0.25) AND FOR  NIR (KMEANS>0.21), #
# ELIMINATING POINTS WHERE CONDITION FOR SWIR1 AND NIR COINCIDE. ---#

#----------------------------------------------------------------#
#----- VALUES NOT CORRESPONDING TO FIRE ACCORDING TO KMEANS ------#

huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")


for (HUELLA in huella_list) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
  
  nomes <- dir()
  nomes <- str_subset(nomes, "shp$")
  
  # -------------------------------#
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_NIR"))
  NIR_ <-  as.data.frame(read.csv("valoresNoCorresp_NIR.csv", sep = ";"))
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_SWIR"))
  SWIR_ <- as.data.frame(read.csv("valoresNoCorresp_SWIR.csv", sep = ";")) 
  
  valoresNoCorresp_NIR <- list()
  valoresNoCorresp_SWIR <- list()
  
  if (nrow(NIR_) != nrow(SWIR_)) {
    
    print("ERROR IN CSV - DIFFERENT EXTENSION NIR AND SWIR")
    break
  }
  
  
  for (m in 1:nrow(NIR_)){
    
    
    if(NIR_[[2]][[m]] != SWIR_[[2]][[m]]) {
      
      print("DIFFERENT FILE NAMES IN LIST CREATION")
      break
    }
    
    conjunto_NIR <- c(NIR_[[3]][[m]],NIR_[[4]][[m]],NIR_[[5]][[m]],NIR_[[6]][[m]],NIR_[[7]][[m]])
    conjunto_SWIR <- c(SWIR_[[3]][[m]],SWIR_[[4]][[m]],SWIR_[[5]][[m]],SWIR_[[6]][[m]],SWIR_[[7]][[m]])
    
    valoresNoCorresp_NIR[[length(valoresNoCorresp_NIR) + 1]] <- conjunto_NIR
    valoresNoCorresp_SWIR[[length(valoresNoCorresp_SWIR) + 1]] <- conjunto_SWIR
    
  }
  
  
  #----------------------------------------------------------------#
  
  #································································#
  # ------------ SELECTION OF FIRES ON DISTURBANCES -------------- #
  #································································#
  
  for (i in 1:length(nomes)) {
    
    j <- paste0(substr(nomes[i],start = 1, stop = 8),".TIF")
    n <- nomes[i]
    
    if (substr(n, start = 1, stop = 8) != NIR_[[2]][[i]]) {
      print("ERROR IN NAMES FOR")
      print(n)
      break
    }
    
    #======================================================================#
    #---------- IMPORT AND SELECTION OF NON-FIRE RELATED VALUES  ----------#
    #======================================================================#
    
    # ---IMPORT ALL PERIMETERS--- #
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
    PERIMETROS_GENERAL <- st_read(n)
    
    # ---FOR NIR--- #
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_NIR/SHAPE_PERI_CLUSTERIZADO"))
    KMEANS_NIR <- st_read(n)
    names(KMEANS_NIR) <- c("supf_ha","ValorPoly","geometry")
    
    aNIR <- valoresNoCorresp_NIR[[i]][[1]]
    bNIR <- valoresNoCorresp_NIR[[i]][[2]]
    cNIR <- valoresNoCorresp_NIR[[i]][[3]]
    dNIR <- valoresNoCorresp_NIR[[i]][[4]]
    eNIR <- valoresNoCorresp_NIR[[i]][[5]]
    
    
    K_MEANS_NIR_ERRONEOS <- KMEANS_NIR[which(KMEANS_NIR$ValorPoly == aNIR | KMEANS_NIR$ValorPoly == bNIR | 
                                               KMEANS_NIR$ValorPoly == cNIR | KMEANS_NIR$ValorPoly == dNIR |
                                               KMEANS_NIR$ValorPoly == eNIR),] 
    
    
    if(length(K_MEANS_NIR_ERRONEOS$geometry) != 0) {
      
      PUNTOS_NO_LUME_NIR <- PERIMETROS_GENERAL[K_MEANS_NIR_ERRONEOS,]
      
      
    } else {
      
      PUNTOS_LUME_SURE <- PERIMETROS_GENERAL
      
      st_crs(PUNTOS_LUME_SURE) <- 32629
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
      st_write(PUNTOS_LUME_SURE,
               n)
      
      print("There are not another disturbance for NIR - All are FIRE-DISTURBANCE NIR")
      
      next
      
    }
    
    
    # ---FOR SWIR--- #
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/PERIMETROS_FINALES_CLASIF_KMEANS/PARA_SWIR/SHAPE_PERI_CLUSTERIZADO"))
    KMEANS_SWIR <- st_read(n)
    names(KMEANS_SWIR) <- c("supf_ha","ValorPoly","geometry")
    
    aSWIR <- valoresNoCorresp_SWIR[[i]][[1]]
    bSWIR <- valoresNoCorresp_SWIR[[i]][[2]]
    cSWIR <- valoresNoCorresp_SWIR[[i]][[3]]
    dSWIR <- valoresNoCorresp_SWIR[[i]][[4]]
    eSWIR <- valoresNoCorresp_SWIR[[i]][[5]]
    
    K_MEANS_SWIR_ERRONEOS <- KMEANS_SWIR[which(KMEANS_SWIR$ValorPoly == aSWIR | KMEANS_SWIR$ValorPoly == bSWIR |
                                                 KMEANS_SWIR$ValorPoly == cSWIR | KMEANS_SWIR$ValorPoly == dSWIR |
                                                 KMEANS_SWIR$ValorPoly == eSWIR),]
    
    
    if(length(K_MEANS_SWIR_ERRONEOS$geometry) != 0) {
      
      PUNTOS_NO_LUME_SWIR <- PERIMETROS_GENERAL[K_MEANS_SWIR_ERRONEOS,]
      
    } else {
      
      PUNTOS_LUME_SURE <- PERIMETROS_GENERAL
      
      st_crs(PUNTOS_LUME_SURE) <- 32629
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
      st_write(PUNTOS_LUME_SURE,
               n)
      
      print("There are not another disturbance for SWIR - All are FIRE-DISTURBANCE SWIR")
      
      next
      
    } 
    
    
    #----------------------------------------------------------------#
    #================================================================#
    #-------------- INTERSECTION NIR - SWIR POINTS  -----------------#
    #================================================================#
    
    #································································#
    COMB_NIR_SWIR_NO_LUME <- PUNTOS_NO_LUME_SWIR[PUNTOS_NO_LUME_NIR,]
    #································································#
    
    
    #----------------------------------------------------------------#
    #================================================================#
    #------------------ OBTAINING FIRE PERIMETERS  ------------------#
    #================================================================#
    
    if(length(COMB_NIR_SWIR_NO_LUME$geometry) != 0) {
      
      COMB_NIR_SWIR_NO_LUME_ <- as(COMB_NIR_SWIR_NO_LUME,"Spatial")
      PERIMETROS_GENERAL_ <- as(PERIMETROS_GENERAL, "Spatial")
      
      #············································································································#
      PUNTOS_LUME_SURE <- PERIMETROS_GENERAL_[is.na(sp::over(PERIMETROS_GENERAL_,sp::geometry(COMB_NIR_SWIR_NO_LUME_))),]
      PUNTOS_LUME_SURE <- st_as_sf(PUNTOS_LUME_SURE)
      #············································································································#
    } else {
      PUNTOS_LUME_SURE <- PERIMETROS_GENERAL
      print("There are not another disturbance - All are FIRE-DISTURBANCES")
    }
    
    
    #--------------------#
    # -- SAVE RESULTS -- #
    #--------------------#
    
    # --- FOR FIRES --- #
    if(length(COMB_NIR_SWIR_NO_LUME$geometry) != 0) {
      
      st_crs(PUNTOS_LUME_SURE) <- 32629
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
      st_write(PUNTOS_LUME_SURE,
               n)
      
    }else{
      "There are NOT another disturbance - All are FIRE-DISTURBANCES"
    }
    
    
    # --- FOR NO FIRES --- #
    
    if(length(COMB_NIR_SWIR_NO_LUME$geometry) != 0) {
      
      st_crs(COMB_NIR_SWIR_NO_LUME) <- 32629
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_NO_LUME"))
      st_write(COMB_NIR_SWIR_NO_LUME,
               n)
      
    }else{
      "There are not COMB_NIR_SWIR_NO_LUME - THERE AREN'T FIRES"
    }
    
  }
}


#====================================================================#
#====================================================================#
# -------------------- SEVERITY DISTRIBUTION ----------------------- #
#====================================================================#
#====================================================================#

huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (HUELLA in huella_list) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
  
  nomes <- dir()
  nomes <- str_subset(nomes, "shp$")
  
  for (i in 1:length(nomes)) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
    BURN_ <- st_read(nomes[i])
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/DIF_NBR/DIFERENCIAS_NBR"))
    dNBR <- raster(paste0(substr(nomes[i], start = 1, stop = 8),".TIF"))
    
    if(nrow(BURN_) == 0){
      print("without areas for")
      print(nomes[i])
      next
    }
    
    dif_NBR_seve_peri <- mask(dNBR,BURN_)
    dif_NBR_seve_peri <- crop(dif_NBR_seve_peri, BURN_) 
    
    # ------- #
    colors_severity = c("purple","red","yellow")
    
    NBR_severidad <- c(-Inf, -.50, 1, 
                       -.50, -.25, 2,
                       -.25, -.10, 3,
                       -.10,  .10, 4, 
                       .10,  .27, 5, 
                       .27,  .44, 6, 
                       .44,  .66, 7,
                       .66, +Inf, 8)
    
    class.matrix <- matrix(NBR_severidad, ncol = 3, byrow = TRUE) 
    # ------- #
    
    dif_NBR_seve_peri <- reclassify(dif_NBR_seve_peri, NBR_severidad,  right=NA)
    dif_NBR_seve_peri <- ratify(dif_NBR_seve_peri)
    
    
    dif_NBR_seve_peri[dif_NBR_seve_peri < 5.5] <- NA
    #mapview(dif_NBR_seve_peri,col.regions=rev(colors_severity))
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/RASTER_SEVERIDAD/LUME_SI"))
    
    proj4string(dif_NBR_seve_peri) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    writeRaster(dif_NBR_seve_peri,
                filename = paste0(substr(nomes[i], start = 1, stop = 8),".TIF"), 
                format = "GTiff",
                datatype= 'FLT4S')
  }
  
  # ------- #
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_NO_LUME"))
  
  nomes <- dir()
  nomes <- str_subset(nomes, "shp$")
  
  for (i in 1:length(nomes)) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_NO_LUME"))
    BURN_ <- st_read(nomes[i])
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/DIF_NBR/DIFERENCIAS_NBR"))
    dNBR <- raster(paste0(substr(nomes[i], start = 1, stop = 8),".TIF"))
    
    if(nrow(BURN_) == 0){
      print("without areas for")
      print(nomes[i])
      next
    }
    
    
    dif_NBR_seve_peri <- mask(dNBR,BURN_)
    dif_NBR_seve_peri <- crop(dif_NBR_seve_peri, BURN_) 

    
    # ------- #
    colors_severity = c("purple","red","yellow")
    
    NBR_severidad <- c(-Inf, -.50, 1, 
                       -.50, -.25, 2,
                       -.25, -.10, 3,
                       -.10,  .10, 4, 
                       .10,  .27, 5, 
                       .27,  .44, 6, 
                       .44,  .66, 7,
                       .66, +Inf, 8)
    
    class.matrix <- matrix(NBR_severidad, ncol = 3, byrow = TRUE) 
    # ------- #
    
    dif_NBR_seve_peri <- reclassify(dif_NBR_seve_peri, NBR_severidad,  right=NA)
    dif_NBR_seve_peri <- ratify(dif_NBR_seve_peri)
    
    dif_NBR_seve_peri[dif_NBR_seve_peri < 5.5] <- NA
    
    #mapview(dif_NBR_seve_peri,col.regions=rev(colors_severity))
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/RASTER_SEVERIDAD/LUME_NO"))
    
    proj4string(dif_NBR_seve_peri) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    writeRaster(dif_NBR_seve_peri,
                filename = paste0(substr(nomes[i], start = 1, stop = 8),".TIF"), 
                format = "GTiff",
                datatype= 'FLT4S')
  }
}





























