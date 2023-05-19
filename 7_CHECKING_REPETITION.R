

#----------------------------------------------------------------------#
#                          SCRIPT NUMBER 7                             #
#          VERIFICATION OF REPETITION OR PERIODICITY OF FIRES          #
#----------------------------------------------------------------------#

# --- optional script to verify if the detected burned area belongs to a #
# previous disturbance. Search for those identified areas whose change in #
# dNBR may be due to climatic seasonality. This step requires at the end #
# a visual inspection to decide whether the identified area is a disturbance #
# or not --- #


huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (HUELLA in huella_list) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
  
  archivos <- dir()
  archivos <- str_subset(archivos, "shp$")
  
  for (i in archivos) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/1_CLASIF_FINAL_LUMES_KMEANS/RESULTADOS/SHAPE/PUNTOS_LUME"))
    
    peri <- st_read(i)
    
    # ------- #
    if(nrow(peri)==0){
      print(paste0("WITHOUT POLYGON FOR", i))
      next
    }
    # ------- #
    
    peri <- select(peri, geometry)
    
    peri$supf_ha <- as.numeric(st_area(peri)/10000)
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/RESULT_AREAS/",HUELLA))
    
    st_crs(peri) <- 32629
    st_write(peri,
             delete_layer = TRUE,
             i)
  }
}

# --------------------------------------------------- #
# --------------------------------------------------- #

for(HUELLA in huella_list){
  
  # ------------------------------------------------- #
  
  if (HUELLA == "HUELLA_204031") {
    
    setwd("C:/PERTURBACIONES_FOREST_GALI/AREA_ESTUDIO_SUR")
    limites_exact<-st_read("GALICIA_SURR.shp")
    
    set_aoi(limites_exact$geometry)
    view_aoi()
    
    extent(limites_exact)
    
    st_crs(limites_exact) <- 32629
    limites<- st_transform(limites_exact, crs=32629)
    extent(limites_exact)
    
  }else{
    
    setwd("C:/PERTURBACIONES_FOREST_GALI/AREA_ESTUDIO")
    limites<-st_read("GALICIA_NORTE_DEF.shp")
    
    set_aoi(limites$geometry)
    view_aoi()
    
    extent(limites)
    
    limites<- st_transform(limites, crs=32629)
    extent(limites)
  }
  
  # ------------------------------------------------- #
  
  #------------------------------------------------------------------------------------------#
  #------------------------- CREATION OF EMPTY DATAFRAME FOR ERROR --------------------------#
  #------------------------------------------------------------------------------------------#
  setwd("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/COMPROBACION_REPETICIONES")
  MATRIX <- read_excel("MATRIX_MAS_YEAR.xlsx") 
  
  MATRIX<-data.frame(MATRIX) 
  
  #------------------------------------------------------------------------------------------#
  #------------------------------------ VALIDATION ------------------------------------------#
  #------------------------------------------------------------------------------------------#
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/RESULT_AREAS/",HUELLA))

  nomes <- dir()
  nomes <- str_subset(nomes, "shp$")
  
  
  for (i in 3:length(nomes)) {
    
    imagen1 <- nomes[i]
    
    mes1 <- substr(imagen1, start = 5, stop = 6)
    mes1 <- as.numeric(mes1)
    year1 <- substr(imagen1, start = 1, stop = 4)
    year1 <- as.numeric(year1) 
    
    j <- i-2
    mes2 <- substr(nomes[j], start = 5, stop = 6)
    mes2 <- as.numeric(mes2)
    year2 <- substr(nomes[j], start = 1, stop = 4)
    year2 <- as.numeric(year2)
    
    # ---- TRANSFORMATION AND GROUPING OF MONTHS ---- #
    
    if(mes1 == 11 | mes1 == 12 | mes1 == 01) {
      mes1 <- 20
    } else if (mes1 == 02 | mes1 == 03 | mes1 == 04) {
      mes1 <- 21
    } else if (mes1 == 05 | mes1 == 06 | mes1 == 07) {
      mes1 <- 22
    }else{
      mes1 <- 23
    }
    
    # ------------------------------------------------ #
    
    if(mes2 == 11 | mes2 == 12 | mes2 == 01) {
      mes2 <- 20
    } else if (mes2 == 02 | mes2 == 03 | mes2 == 04) {
      mes2 <- 21
    } else if (mes2 == 05 | mes2 == 06 | mes2 == 07) {
      mes2 <- 22
    }else{
      mes2 <- 23
    }
    
    
    # -------------------------------------------------- #
    
    if (mes1 == mes2) {
      imagen2 <- nomes[j]
      
    } else if (mes1 != mes2) {
      j <- j-1
      
      if (j == 0) {
        
        NOMBRE_MAS_YEAR <- imagen1
        MATRIX = rbind(MATRIX, NOMBRE_MAS_YEAR)
        next}
    }
    
    
    while (mes1 != mes2) {
      
      mes2 <- substr(nomes[j], start = 5, stop = 6)
      mes2 <- as.numeric(mes2)
      
      # --- TRANSFORMACION MES --- #
      if(mes2 == 11 | mes2 == 12 | mes2 == 01) {
        mes2 <- 20
      } else if (mes2 == 02 | mes2 == 03 | mes2 == 04) {
        mes2 <- 21
      } else if (mes2 == 05 | mes2 == 06 | mes2 == 07) {
        mes2 <- 22
      }else{
        mes2 <- 23
      }
      
      # ------------------------------------------------- #
      
      year2 <- substr(nomes[j], start = 1, stop = 4)
      year2 <- as.numeric(year2)
      
      imagen2 <- nomes[j]
      
      j <- j-1
      
      if ((year1-year2) > 5) {
        
        NOMBRE_MAS_YEAR <- imagen1
        MATRIX = rbind(MATRIX, NOMBRE_MAS_YEAR)
        
        break}
      
      # ------------------------------------------------ #
      
      if (j == 0) {
        
        NOMBRE_MAS_YEAR <- imagen1
        MATRIX = rbind(MATRIX, NOMBRE_MAS_YEAR)
        
        break}
      
    }
    
    
    # -------- CONDITION OUTSIDE THE "WHILE" ------- #
    
    if ((year1-year2)>5) {
      next
    }else if (j == 0){
      next}
    # ---------------------------------------------- #
    
    
    # ------------ PROCESSING OF NBRs -------------- #
    
    # --- IMPORT NBR --- #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/DATOS_TOTAL/NBR_",substr(HUELLA, start = 8, stop = 13)))
    
    NBR_1 <- raster(paste0("NBR_",substr(imagen1, start = 1, stop = 8),".TIF"))
    NBR_2 <- raster(paste0("NBR_",substr(imagen2, start = 1, stop = 8),".TIF"))
    
    NBR_1 <- crop(NBR_1,extent(limites))
    NBR_2 <- crop(NBR_2,extent(limites))
    
    
    # --- IMPORT CLOUD MASK --- #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",HUELLA,"/IMAGENES_NUBES_BITS_CONJUNTO/BINA_SUAVIZADA/TIFF"))
    
    CLOUDS1 <- raster(paste0(substr(imagen1, start = 1, stop = 8),".TIF"))
    CLOUDS1 <- reclassify(CLOUDS1, c(-Inf,0.5,NA, 0.6,Inf,1))
    CLOUDS1 <- crop(CLOUDS1,extent(limites))
    
    CLOUDS2 <- raster(paste0(substr(imagen2, start = 1, stop = 8),".TIF"))
    CLOUDS2 <- reclassify(CLOUDS2, c(-Inf,0.5,NA, 0.6,Inf,1))
    CLOUDS2 <- crop(CLOUDS2,extent(limites))
    
    # --- DIFFERENCE BETWEEN NBRs --- #
    
    NBR_TOTAL <- NBR_2 - NBR_1
    
    NBR_TOTAL <- mask(NBR_TOTAL, CLOUDS1)
    NBR_TOTAL <- mask(NBR_TOTAL, CLOUDS2)
    
    # --- OBTAINING PERIMETERS --- #
    NBR_TOTAL[NBR_TOTAL < 0.27] <- NA # 6 LEVEL - Moderate-low Severity #
    
    PERIMETROs <- as_Spatial(st_as_sf(st_as_stars(NBR_TOTAL),
                                      as_points = FALSE, merge = TRUE))
    
    # GEOMETRY CHECKING #
    rgeos::gIsValid(PERIMETROs)
    
    # INTERSECTION ELIMINATION #
    PERIMETROs <- rgeos::gBuffer(PERIMETROs, byid = TRUE, width = 0 ) #SE APLICA DIST BUFFER 0
    
    # VALIDITY OF GEOMETRIES (CHECKING) #
    rgeos::gIsValid(PERIMETROs)
    
    PERIMETROs<-st_as_sf(PERIMETROs) # BECOMES AN SIMPLE FEATURE #
    PERIMETROs<-st_union(PERIMETROs) # DISSOLVES GEOMETRIES #
    PERIMETROs<-st_cast(PERIMETROs,"POLYGON",do_split=TRUE) # SEPARATES GEOMETRY # 
    PERIMETROs<-st_as_sf(PERIMETROs) # IDENTIFY GEOMETRY #
    
    PERIMETROs$supf_ha<-as.numeric(st_area(PERIMETROs)/10000)
    PERIMETROs<-PERIMETROs[which(PERIMETROs$supf_ha >= 12),] # The minimum general surface area is reduced by 20% #
    
    
    # --- IMPORT CALCULATED PERIMETERS --- #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/RESULT_AREAS/",HUELLA))
    perimetros_ORIGINALES_CALCU <- st_read(imagen1)
    
    perimetros_ORIGINALES_CALCU <- select(perimetros_ORIGINALES_CALCU, geometry)
    perimetros_ORIGINALES_CALCU$supf_ha<-as.numeric(st_area(perimetros_ORIGINALES_CALCU)/10000)
    perimetros_ORIGINALES_CALCU <- perimetros_ORIGINALES_CALCU[perimetros_ORIGINALES_CALCU$supf_ha != 0, ] 
    perimetros_ORIGINALES_CALCU <- st_transform(perimetros_ORIGINALES_CALCU, crs=32629)
    
    # ----------------------------------------- #
    
    PERIMETROs <- st_transform(PERIMETROs, crs=32629)
    
    PERIMETROs.buffer <- st_buffer(PERIMETROs, 60)
    PERIMETROs.buffer <- st_transform(PERIMETROs.buffer, crs=32629)
    
    # ============================================================================================================================ #
    
    peri_ORIGI_INTERSEC <- perimetros_ORIGINALES_CALCU[PERIMETROs.buffer,]
    
    # --- CONDITION WITHOUT INTERSECTION --- #
    if(nrow(peri_ORIGI_INTERSEC) == 0) {
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/COMPROBACION_REPETICIONES/",HUELLA))
      
      st_write(perimetros_ORIGINALES_CALCU,
               paste0(substr(imagen1, start = 1, stop = 8),"_",substr(imagen2, start = 1, stop = 8),".shp"))
      next
    }
    # ------------------------------------- #
    
    perimetros_ORIGINALES_CALCU <- as(perimetros_ORIGINALES_CALCU, "Spatial")
    peri_ORIGI_INTERSEC <- as(peri_ORIGI_INTERSEC, "Spatial")
    
    if (length(perimetros_ORIGINALES_CALCU) == length(peri_ORIGI_INTERSEC)) {
      next
    }
    
    PUNTOS_DESCARTE <- perimetros_ORIGINALES_CALCU[is.na(sp::over(perimetros_ORIGINALES_CALCU,sp::geometry(peri_ORIGI_INTERSEC))),]
    PUNTOS_DESCARTE <- st_as_sf(PUNTOS_DESCARTE)
    
    # ============================================================================================================================ #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/COMPROBACION_REPETICIONES/",HUELLA))
    
    st_write(PUNTOS_DESCARTE,
             paste0(substr(imagen1, start = 1, stop = 8),"_",substr(imagen2, start = 1, stop = 8),".shp"))
    
  }
  
  # --- SAVE MATRIX WITH LIST OF NAMES --- #
  
  # --- Matrix with the names of the files that do not have another previous image in the previous 5 years for the same seasonality --- #
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/COMPROBACION_REPETICIONES/",HUELLA))
  write.csv(MATRIX, 
            "MATRIX_ARCHIVES_MORE_5_YEARS.csv")
  
}
