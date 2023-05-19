
#---------------------------------------------------------------#
#                       SCRIPT NUMBER 4                         #
#           SCRIPT TO MAKE NBR DIFERENCE BETWEEN IMAGES         #
#---------------------------------------------------------------#

# --- OBTAINING THE DIFFERENCES BETWEEN TWO NBR INDEX IMAGES
#     THE PREVIOUS IMAGE MINUS THE NEXT IMAGE IS SUBTRACTED --- #
#---------------------------------------------------------------#

HUELLAS <- c("HUELLA_204030", "HUELLA_204031","HUELLA_205030")

for (huella in HUELLAS){
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DATOS_TOTAL/NBR_",substr(huella, start = 8, stop = 13)))
  imaxes <- dir()
  imaxes <- str_subset(imaxes, "tif$")
  
  
  nombres <- c()
  
  for (nu in 1:length(imaxes)) {
    
    nom <- imaxes[nu]
    nom_ <- substr(nom, start = 5, stop = 16)
    
    nombres[nu] <- nom_
  }
  
  
  for (i in 1:(length(imaxes)-1)) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DATOS_TOTAL/NBR_",substr(huella, start = 8, stop = 13)))
    
    j <- imaxes[i]
    NBR01 <- raster(j)
    
    q <- i + 1 
    t <- imaxes[q] 
    NBR02 <- raster(t)
    
    r <- nombres[q]
    
    # ------ VALUE CORRECTION ------ #
    
    NBR01[NBR01<-1] = -1
    NBR01[NBR01>1] = 1
    
    NBR02[NBR02<-1] = -1
    NBR02[NBR02>1] = 1
    
    # ------- NBR HISTOGRAM -------- #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DIF_NBR/HISTOGRAMAS"))
    
    tiff(r)
    par(mfrow=c(1,2))
    hist(NBR01, main = 'PRE-NBR', breaks = 200, xlab= "PIXEL VALUE")
    hist(NBR02, main = 'POST-NBR', breaks = 200, xlab= "PIXEL VALUE") 
    dev.off()
    
    # -------- SUBTRACTION -------- #
    
    dif_NBR <- NBR01 - NBR02
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DIF_NBR/DIFERENCIAS_NBR"))
    
    proj4string(dif_NBR) <- 
      CRS("+proj=utm +zone=29 +north +ellps=WGS84 +datum=WGS84")
    
    
    writeRaster(dif_NBR,
                filename = r, 
                format = "GTiff",
                datatype= 'FLT4S')
    
    
    # ---- DIFFERENCE AS A MAP ---- #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DIF_NBR/DIF_MAPA"))
    
    tiff(r)
    par(mfrow=c(1,1))
    plot(dif_NBR, main = "NBR DIFFERENCE")
    dev.off()
    
  }
}
