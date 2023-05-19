
#----------------------------------------------------------------------#
#                          SCRIPT NUMBER 8                             #
#      ELIMINATION OF OVERLAPPING BURNED AREAS BETWEEN FOOTPRINTS      #
#----------------------------------------------------------------------#
setwd("C:/")

# -------- #
# --- Do not run if script 7 was run / NEXT "bucle for" should be running --- #

huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (huella in huella_list) {
  
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

# -------- #

# --- OBTAINING LISTS --- #

huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (huella in huella_list) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/RESULT_AREAS/",huella))
  
  archivos <- dir()
  archivos <- str_subset(archivos, "shp$")
  
  # ------------------------------------------- #
  if (huella == "HUELLA_204030") {
    IMAG_204030 <- archivos
    print(huella)
    
  } else if (huella == "HUELLA_204031"){
    IMAG_204031 <- archivos
    print(huella)
    
  } else if (huella == "HUELLA_205030") {
    IMAG_205030 <- archivos
    print(huella)
  }
  # ------------------------------------------- # 
}

IMAG_204030_DATES <- list()
IMAG_204031_DATES <- list()
IMAG_205030_DATES <- list()


for (lista_huella in huella_list) {

  if (lista_huella == "HUELLA_204030") {
    huella <- IMAG_204030
    
  } else if (lista_huella == "HUELLA_204031"){
    huella <- IMAG_204031
    
  } else if (lista_huella == "HUELLA_205030") {
    huella <- IMAG_205030
  }

  
  for (i in 1:length(huella)) {

    if (lista_huella == "HUELLA_204030") {
      fecha <- substr(IMAG_204030[i], start = 1, stop = 8)
      fecha <- as.Date(fecha,format="%Y%m%d")
      IMAG_204030_DATES[[i]] <- fecha

      } else if (lista_huella == "HUELLA_204031"){
      fecha <- substr(IMAG_204031[i], start = 1, stop = 8)
      fecha <- as.Date(fecha,format="%Y%m%d")
      IMAG_204031_DATES[[i]] <- fecha

      } else if (lista_huella == "HUELLA_205030") {
      fecha <- substr(IMAG_205030[i], start = 1, stop = 8)
      fecha <- as.Date(fecha,format="%Y%m%d")
      IMAG_205030_DATES[[i]] <- fecha
    
      }
    }
  }


# =============================================================================== #
# ========== FOOTPRINT COMPARISON - CREATION OF THE COMPARISON LIST  ============ #
# =============================================================================== #

funcion_superposicion <- function(x,y) {
  
  lista_prueba <- list(list())
  r <- 1
  
  for (p in 1:length(x)){
    
    lista_prueba[[p]] <- x[[p]]
    
    for (q in 1:length(y)) {
      
      if(x[[p]] > y[[r]]){
        
        lista_prueba[[p]][[q]] <- y[[r]]
        r <- r+1
        
        if (r > length(y)) {
          break
        }
        
      }else if (x[[p]] < y[[r]]) {
        
        lista_prueba[[p]][[q]] <- y[[r]]
        break
        
      }else if (x[[p]] == y[[r]]) {
        
        lista_prueba[[p]][[q]] <- y[[r]]
        r <- r+1
        break
      }
    }
    
    if (r > length(y)) {
      break
    }
    
  }
  return(lista_prueba)
}



IMAG_204030_PARA_205030 <- funcion_superposicion(IMAG_205030_DATES,IMAG_204030_DATES)
IMAG_204031_PARA_205030 <- funcion_superposicion(IMAG_205030_DATES,IMAG_204031_DATES)
IMAG_204031_PARA_204030 <- funcion_superposicion(IMAG_204030_DATES,IMAG_204031_DATES)



# =============================================================================== #
# ============================== OVERLAPPING ==================================== #
# =============================================================================== #

# --- OVERLAPPING FUNCTION --- #
funcion_superposicion_REVISION_PERI <- function(x1,x2,y,huellaX,huellaY) {
  
  for (i in 1:length(x1)) {
    
    a <- x2[i]
    
    # --- CONDITION --- #
    if(i > length(y)) {break}
    # ----------------- #
    
    for (j in 1:length(y[[i]])) {
      
      # ---IMPORT ALL PERIMETERS OF X--- #
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/RESULT_AREAS/HUELLA_",huellaX))

      PERIMETROS_X <- st_read(a)
      
      # --- CONDITION --- #
      if(nrow(PERIMETROS_X) == 0) {
        next}
      # ----------------- #
      
      PERIMETROS_X <- as(PERIMETROS_X,"Spatial")
      PERIMETROS_X <-PERIMETROS_X[which(PERIMETROS_X$supf_ha >= 15),]
      
      # --- #
      
      n <- y[[i]][[j]] 
      n <- paste0(format(n,'%Y'),format(n,'%m'),format(n,'%d'),".shp")
      
      # ---IMPORT ALL PERIMETERS OF Y--- #
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/RESULT_AREAS/HUELLA_",huellaY))
      PERIMETROS_Y <- st_read(n)
      
      # --- CONDITION --- #
      if(nrow(PERIMETROS_Y) == 0) {
        next}
      # ----------------- #
      
      PERIMETROS_Y <- as(PERIMETROS_Y, "Spatial")
      
      #············································································································#
      
      PUNTOS_COINCIDENTES_Y <- PERIMETROS_Y[PERIMETROS_X,]
      PUNTOS_COINCIDENTES_Y <- st_as_sf(PUNTOS_COINCIDENTES_Y)
      
  
      
      PUNTOS_COINCIDENTES_X <- PERIMETROS_X[PERIMETROS_Y,]
      PUNTOS_COINCIDENTES_X <- st_as_sf(PUNTOS_COINCIDENTES_X)
      
      
      #············································································································#
      
      if (nrow(PUNTOS_COINCIDENTES_Y) == 0 | nrow(PUNTOS_COINCIDENTES_X) == 0) { 
        next
      }
      
      
      #············································································································#
      df <- data.frame(matrix(ncol = 2, nrow = 0))
      nombres_columnas <- c("supf_ha", "geometry")
      colnames(df) <- nombres_columnas
      
      PUNTOS_COINCIDENTES_Y_ <- df
      PUNTOS_COINCIDENTES_X_ <- df
      #············································································································#
      
      
      for (m in 1:nrow(PUNTOS_COINCIDENTES_X)) {
        
        for (m2 in 1:nrow(PUNTOS_COINCIDENTES_Y)){
          
          cond <- PUNTOS_COINCIDENTES_X[m,][PUNTOS_COINCIDENTES_Y[m2,],]
          
          if (length(cond$geometry) != 0){
            
            if(PUNTOS_COINCIDENTES_X[m,]$supf_ha >= PUNTOS_COINCIDENTES_Y[m2,]$supf_ha) {
              PUNTOS_COINCIDENTES_X_ <- rbind(PUNTOS_COINCIDENTES_X_,PUNTOS_COINCIDENTES_X[m,])
              
            } else if (PUNTOS_COINCIDENTES_X[m,]$supf_ha < PUNTOS_COINCIDENTES_Y[m2,]$supf_ha) {
              PUNTOS_COINCIDENTES_Y_ <- rbind(PUNTOS_COINCIDENTES_Y_,PUNTOS_COINCIDENTES_Y[m2,])
            }
          }
        }
      }
      
      
      
      if (nrow(PUNTOS_COINCIDENTES_X_) == 0) {
        PUNTOS_NO_COINCIDENTES_Y <- PERIMETROS_Y
        PUNTOS_NO_COINCIDENTES_Y <- st_as_sf(PUNTOS_NO_COINCIDENTES_Y) 
      }else{
        PUNTOS_COINCIDENTES_X_ <- as(PUNTOS_COINCIDENTES_X_, "Spatial")
        PUNTOS_NO_COINCIDENTES_Y <- PERIMETROS_Y[is.na(sp::over(PERIMETROS_Y,sp::geometry(PUNTOS_COINCIDENTES_X_))),]
        PUNTOS_NO_COINCIDENTES_Y <- st_as_sf(PUNTOS_NO_COINCIDENTES_Y) 
      }
      
      
      if (nrow(PUNTOS_COINCIDENTES_Y_) == 0) {
        PUNTOS_NO_COINCIDENTES_X <- PERIMETROS_X
        PUNTOS_NO_COINCIDENTES_X <- st_as_sf(PUNTOS_NO_COINCIDENTES_X)
      }else{
        PUNTOS_COINCIDENTES_Y_ <- as(PUNTOS_COINCIDENTES_Y_, "Spatial")
        PUNTOS_NO_COINCIDENTES_X <- PERIMETROS_X[is.na(sp::over(PERIMETROS_X,sp::geometry(PUNTOS_COINCIDENTES_Y_))),]
        PUNTOS_NO_COINCIDENTES_X <- st_as_sf(PUNTOS_NO_COINCIDENTES_X)
      }
      
      
      
      # -------- #
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/HUELLA_",huellaX))
      
      st_crs(PUNTOS_NO_COINCIDENTES_X) <- 32629
      st_write(PUNTOS_NO_COINCIDENTES_X,
               delete_layer = TRUE,
               a)
      
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/HUELLA_",huellaY))
      
      st_crs(PUNTOS_NO_COINCIDENTES_Y) <- 32629
      st_write(PUNTOS_NO_COINCIDENTES_Y,
               delete_layer = TRUE,
               n)
    }
  }
} 

# --- 205030 VS 204030 --- #
funcion_superposicion_REVISION_PERI(IMAG_205030_DATES,IMAG_205030,IMAG_204030_PARA_205030,"205030","204030")

# --- 205030 VS 204031 --- #
funcion_superposicion_REVISION_PERI(IMAG_205030_DATES,IMAG_205030,IMAG_204031_PARA_205030,"205030","204031")

# --- 204030 VS 204031 --- #
funcion_superposicion_REVISION_PERI(IMAG_204030_DATES,IMAG_204030,IMAG_204031_PARA_204030,"204030","204031")




# =================================================================================== #
# ========================= FINAL CORRECTED PERIMETERS ============================== #
# =================================================================================== #

huella_list <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (huella in huella_list) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/",huella))
  
  archivos <- dir()
  archivos <- str_subset(archivos, "shp$")
  
  for (i in archivos) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/",huella))
    peri <- st_read(i)
    peri <- select(peri, geometry)
    
    # ----- #
    if(nrow(peri) == 0) {next}
    # ----- #
    
    peri$supf_ha <- as.numeric(st_area(peri)/10000)
    peri<-peri[which(peri$supf_ha >= 15),]
    
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/DEFINITIVE_DATABASE/",huella))
    st_crs(peri) <- 32629
    st_write(peri,
             i)
  }
}


#______________________________________________________________________________________________________________________________________#
# ==================================================================================================================================== #
# ==================================================================================================================================== #
# ------------------------------------------------------ DATABASE ARRANGEMENT -------------------------------------------------------- #
# ==================================================================================================================================== #
# ==================================================================================================================================== #

huella <- c("HUELLA_204030", "HUELLA_204031", "HUELLA_205030")

for (HUELLA in huella) {
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/DEFINITIVE_DATABASE/",HUELLA))
  
  folders <- dir()
  folders <- str_subset(folders, "shp$")

  
  for (h in folders) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/DEFINITIVE_DATABASE/",HUELLA))
    
    FIRE <- st_read(h)
    FIRE$supf_ha <- as.numeric(FIRE$supf_ha)
    
    # --- OTROS DATOS A AGREGAR --- #
    
    year <- as.character(substr(h, start = 1,stop = 4))
    month <- as.character(substr(h, start = 5,stop = 6))
    day <- as.character(substr(h, start = 7,stop = 8))
    
    platform <- as.character("LANDSAT")
    
    if (year <= 2012) {mision <- as.character(5)
    }else if (year > 2012){
      mision <- as.character(8)
    }
    
    footprint <- as.character(substr(HUELLA, start = 8, stop = 13))
    file_name <- as.character(substr(h, start = 1, stop = 8))
    
    # ------------------------------ #
    FIRE$file_name <- file_name
    
    FIRE$year <- year
    FIRE$month <- month
    FIRE$day <- day
    
    
    FIRE$platform <- platform
    FIRE$mision <- mision
    FIRE$footprint <- footprint
    
    
    FIRE <-  subset(FIRE, select=c(file_name, year, month, day, platform, mision, footprint, supf_ha, geometry))
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_CORRECTED_TOTAL_RESULT/1_DEFINITIVE_PERIMETERS/DEFINITIVE_DATABASE/",HUELLA))
    
    st_crs(FIRE) <- 32629
    st_write(FIRE,
             append = FALSE,
             h)
  }
}





