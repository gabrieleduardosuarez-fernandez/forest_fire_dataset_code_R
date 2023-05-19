#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#
#================================= DEFINICION HUELLAS ==========================================#
#-----------------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------------#

HUELLAS <- c("204030","204031","205030")

#------------------------------------------------------------------------------------------#
#===================== IMPORTAR DATOS ESTADISTICA MINISTERIO ==============================#
#------------------------------------------------------------------------------------------#
setwd("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/TABLAS_MINISTE")

localizacion <- read_excel("pif_localizacion.xlsx")
perdidas <- read_excel("pif_perdidas.xlsx")
tiempos <- read_excel("pif_tiempos.xlsx")

U1 <- merge(x = localizacion, y = perdidas, by = c("numeroparte", "numeroparte")) 
datos_incendio_tabla <- merge(x = U1, y = tiempos, by = c("numeroparte", "numeroparte"))
LUME_conocido_DF<-data.frame(datos_incendio_tabla) 

LUME_conocido_DF <- LUME_conocido_DF[!is.na(LUME_conocido_DF$x),]
LUME_conocido_DF <- LUME_conocido_DF[!is.na(LUME_conocido_DF$y),]

LUME_conocido_DF <- subset(LUME_conocido_DF,LUME_conocido_DF$idcomunidad == 3 & LUME_conocido_DF$huso == 29)

LUME_conocido_DF$SUPERFICIE <- LUME_conocido_DF$superficiearboladatotal + LUME_conocido_DF$superficienoarboladatotal
LUME_conocido_DF$FECHA <- as.Date(substr(LUME_conocido_DF$extinguido, start = 1, stop = 10))

LUME_conocido_DF <- subset(LUME_conocido_DF,LUME_conocido_DF$SUPERFICIE >= 15)

LUME_conocido_DF <- select(LUME_conocido_DF,x,y,SUPERFICIE, FECHA)


#------------------------------------------------------------------------------------------#
#---------------------- DEFINICION GEOMETRIA EXACTA PARA RECORTE --------------------------#
#------------------------------------------------------------------------------------------#

for (HUELLA in HUELLAS){
  
  
  setwd("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/HUELLAS_AREAS")
  lim <- paste0("GALICIA_",HUELLA,".shp")
  limites_exact<-st_read(lim)
  
  set_aoi(limites_exact$geometry)
  view_aoi()
  
  extent(limites_exact)
  
  st_crs(limites_exact) <- 32629
  limites<- st_transform(limites_exact, crs=32629)
  extent(limites_exact)
  #------------------------------------------------------------------------------------------#
  
  #------------------------------------------------------------------------------------------#
  #---------------------- CREACION DEL DATAFRAME VACIO PARA ERROR- --------------------------#
  #------------------------------------------------------------------------------------------#
  
  setwd("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/VARIAS_ERROR")  
  MATRIX <- read_excel("MATRIX_CONFUSION.xlsx") 
  
  MATRIX<-data.frame(MATRIX) 
  
  #------------------------------------------------------------------------------------------#
  #==========================================================================================#
  #------------------------------------------------------------------------------------------#
  #==========================================================================================#
  
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI_FINAL_CORRECTED/Z_DATASET_DEFINITIVO_PERI/PERIMETROS_DEFINITIVO_HUELLA_",HUELLA))
  
  num <- dir()
  num <- str_subset(num, "shp$")
  num <- substr(num, start = 1, stop = 8)
  
  
  dates <- c(as.Date("1900/01/01", format="%Y/%m/%d"))
  for (a in 1:length(num)){
    
    i <- num[a]
    
    num_dat <- paste0(substr(i, start = 1, stop = 4),"/",substr(i, start = 5, stop = 6),"/",substr(i, start = 7, stop = 8))  
    num_dat <- as.Date(num_dat, format="%Y/%m/%d")
    
    dates[a] <- num_dat
  }
  
  DATES = subset(dates,dates >"1999-01-01" & dates <="2015-12-31")
  
  
  NOMBRES_SHAPE <- c()
  NOMBRE_CSV <- c()
  for (b in 1:length(DATES)){
    
    Name_ <- toString(DATES[b])
    
    Name_or <- paste0(substr(Name_, start = 1, stop = 4),substr(Name_, start = 6, stop = 7),substr(Name_, start = 9, stop = 10))  
    
    NOMBRES_SHAPE[length(NOMBRES_SHAPE) + 1] <- paste0(Name_or,".shp")
    NOMBRE_CSV[length(NOMBRE_CSV) + 1] <- paste0(Name_or,".CSV")
    
  }
  
  
  
  for (i in 1:(length(NOMBRES_SHAPE)-1)) {
    
    j <- as.numeric(i+1)
    
    a <- DATES[i]
    b <- DATES[j]
    
    # ----- SE ESCOGE RANGO DE FECHAS ----- #
    LUME_FECHA = subset(LUME_conocido_DF,LUME_conocido_DF$FECHA >a & LUME_conocido_DF$FECHA <=b)
    
    #---------------------------------------#
    # ----------- GUARDAR CSV ------------- #
    
    c <- NOMBRE_CSV[j]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/VARIAS_ERROR/MATRIZ_TEMPORALES/GENERAL/HUELLA_",HUELLA))
    write.csv(LUME_FECHA, 
              c)
    #---------------------------------------#
    
    LUME_FECHA_pol <- st_as_sf(as.data.frame(LUME_FECHA), coords = c('x', 'y'), crs = 32629)
    
    LUME_FECHA_pol<-st_union(LUME_FECHA_pol) #DISUELVE GEOMETRIAS EN UNA
    LUME_FECHA_pol<-st_cast(LUME_FECHA_pol,"POINT",do_split=TRUE) #SEPARA GEOMETRIA 
    LUME_FECHA_pol<-st_as_sf(LUME_FECHA_pol)
    
    
    # RECORTE POR EXTENSION DE AREA ESTUDIO #
    LUME_FECHA_pol_ <-LUME_FECHA_pol[limites_exact,]
    
    #=========================================================================#
    #-------------------------------------------------------------------------#
    LUME_FECHA_pol_.buffer <-st_buffer(LUME_FECHA_pol_,3000) #EQUIV 100 PIXELES
    #-------------------------------------------------------------------------#
    #=========================================================================#
    
    extent(LUME_FECHA_pol_.buffer)
    LUME_FECHA_pol.buffer<- st_transform(LUME_FECHA_pol_.buffer, crs=32629)
    extent(LUME_FECHA_pol_.buffer)
    
    # --------GUARDAR BUFFER PUNTOS-------- #  
    
    d <- NOMBRES_SHAPE[j]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/VARIAS_ERROR/PUNTOS_MINISTERIO_BUFFER/GENERAL/HUELLA_",HUELLA))
    st_write(LUME_FECHA_pol_.buffer,
             d)
    #---------------------------------------#
    
    # **************************************************************************************** #
    #------------------------------------------------------------------------------------------#
    #========================= IMPORTAR PERIMETROS CALCULADOS =================================#
    #------------------------------------------------------------------------------------------#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/1_PERI_TOTALES_CORRE_REV_15HA/HUELLA_",HUELLA))
    INCENDIO <- st_read(d)
    
    extent(INCENDIO)
    st_crs(INCENDIO) <- 32629
    INCENDIO<- st_transform(INCENDIO, crs=32629)
    extent(INCENDIO)
    
    INCENDIO <- select(INCENDIO,geometry)
    INCENDIO$supf_ha<-as.numeric(st_area(INCENDIO)/10000)
    
    #--- CORTAR PARA AREA ESTUDIO ---#
    #-----------------------------------#
    INCENDIO_ <-INCENDIO[limites_exact,]
    INCENDIO_ <-INCENDIO_[which(INCENDIO_$supf_ha >= 15),] #PARA ELIMINAR DE LA VECINDAD LOS INCENDIOS MENORES A 15HA
    #-----------------------------------#
    
    #====================================================#
    # ************************************************** #
    #----------------------------------------------------#
    VERD_POSITIVOS <-INCENDIO_[LUME_FECHA_pol_.buffer,]
    VERD_POSITIVOS2 <-LUME_FECHA_pol_.buffer[INCENDIO_,]
    #----------------------------------------------------#
    # ************************************************** #
    #====================================================#
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/VARIAS_ERROR/VERDADEROS_POSITIVOS/VERD_POSIT1/HUELLA_",HUELLA))
    st_write(VERD_POSITIVOS,
             d)
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/VARIAS_ERROR/VERDADEROS_POSITIVOS/VERD_POSIT2/HUELLA_",HUELLA))
    st_write(VERD_POSITIVOS2,
             d)
    
    #===========================================================================================================#  
    # ********************************************************************************************************* #  
    #===========================================================================================================#   
    
    #===========================================================================================================# 
    #============================================ ANALISIS RESULTADOS ==========================================#
    #===========================================================================================================# 
    
    Fila_M_confu = list()
    
    #-------------------------------------------------------#
    VERDADEROS_POSITIVOS <- length(VERD_POSITIVOS$geometry)
    VERDADEROS_POSITIVOS2 <- length(VERD_POSITIVOS2$x)
    
    TOTAL_MINISTERIO <- length(LUME_FECHA_pol_.buffer$x)
    
    TOTAL_ESTUDIO <- length(INCENDIO_$geometry)
    
    #-------------------------------------------------------#
    
    Fila_M_confu = c(Fila_M_confu, d, VERDADEROS_POSITIVOS, VERDADEROS_POSITIVOS2, TOTAL_MINISTERIO, TOTAL_ESTUDIO)
    MATRIZ_CONFUSION <- data.frame(d, VERDADEROS_POSITIVOS, VERDADEROS_POSITIVOS2, TOTAL_MINISTERIO, TOTAL_ESTUDIO)
    
    #-------------------------------------------------------#
    
    
    MATRIX = rbind(MATRIX, MATRIZ_CONFUSION)
    
  }
  
  
  setwd("C:/PERTURBACIONES_FOREST_GALI/DATOS_INCENDIO_MINISTERIO/VARIAS_ERROR")
  write.csv(MATRIX, 
            paste0("MATRIX_CONFUSION_DEFINITIVA_GENERAL_",HUELLA,".csv"))
  
}

