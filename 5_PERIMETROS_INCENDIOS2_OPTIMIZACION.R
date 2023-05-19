
#---------------------------------------------------------------#
#                       SCRIPT NUMBER 5                         #
#             SCRIPT TO GET DISTURBANCES PERIMETERS             #
#---------------------------------------------------------------#

HUELLAS <- c("HUELLA_204030", "HUELLA_204031","HUELLA_205030")

#-------------------------------------------#

for (huella in HUELLAS){ 

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
  
  #-------------------------------------------#
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DIF_NBR/DIFERENCIAS_NBR"))
  imaxes <- dir()
  imaxes <- str_subset(imaxes, "tif$")
  
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/GTIFF_CLASIF"))
  MASCARAS <- dir()
  MASCARAS <- str_subset(MASCARAS, "tif$")
  MASCARAS <- MASCARAS[1:(length(MASCARAS)-1)]
  
  
  PERI <- c()
  for (pe in 1:length(imaxes)) {
    
    nom <- imaxes[pe]
    nom <- substr(nom, start = 1, stop = 8)
    
    PERI[pe] <- paste0(nom,".shp")
  }
  
  
  # ----------------------------------------- #
  # ========================================= #
  # ----------------------------------------- #
  # ········· MINIMUM BURNED AREA ··········· #
  
  # --- ENTER THE MINIMUM FIRE AREA TO BE OBTAINED ---#
  
  Superf_min <- 15
  
  #-------------------------------------------#
  
  Superf_min <- as.numeric(Superf_min)
  Superf_min_20 <- as.numeric(0.5)
  Superf_min_20_ <- as.numeric(12)
  
  #-------------------------------------------#
  # ========================================= #
  # ----------------------------------------- #
  
  for (i in 1:length(imaxes)) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/DIF_NBR/DIFERENCIAS_NBR"))
    j <- imaxes[i]
    dif_NBR <- raster(j)
    
    # ··· STANDARDIZATION AND CLASSIFICATION NBR DIFFERENCE ··· #
    
    NBR_severidad <- c(-Inf, -.50, 1, # VERY HIGH REGROWTH
                       -.50, -.25, 2, # HIGH REGROWTH
                       -.25, -.10, 3, # LOW REGROWTH
                       -.10,  .10, 4, # UNBURNED
                       .10,  .27, 5,  # LOW SEVERITY
                       .27,  .40, 6,  # MODERATE-LOW SEVERITY
                       .40,  .44, 6.5,
                       .44,  .48, 7,  # MODERATE-HIGH SEVERITY
                       .48,  .66, 7.5,
                       .66, +Inf, 8)  # HIGH SEVERITY  
    
    class.matrix <- matrix(NBR_severidad, ncol = 3, byrow = TRUE) # MATRIX SEVERITY RANGES
    
    dif_NBR_class <- reclassify(dif_NBR, NBR_severidad,  right=NA) # THRESHOLDS NBR DIFFERENCE WITH SEVERITY LEVELS
    dif_NBR_class <- crop(dif_NBR_class,extent(limites))
    
    # ··· THE MAP OF THE THRESHOLD DIFFERENCE IS CREATED ··· #
    
    leyenda=c("Very high regrowth after fire",
              "High regrowth after fire",
              "Low regrowth after fire",
              "Unburned",
              "Low severity",
              "Moderate-low severity",
              "Moderate-high severity",
              "High severity")
    
    my_colors=c("darkolivegreen",   # VERY HIGH REGROWTH
                                "green",          # HIGH REGROWTH 
                                "limegreen",      # LOW REGROWTH
                                "gray",           # UNBURNED
                                "yellow2",        # LOW SEVERITY
                                "orange2",        # MODERATE-LOW SEVERITY
                                "orange2",
                                "red",            # MODERATE-HIGH SEVERITY
                                "red",
                                "purple")         # HIGH SEVERITY
                                
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/DIF_NBR_RECLASSIFY"))
    
    tiff(j)
    
    plot(dif_NBR_class, col = my_colors,
         main = 'Difference NBR Classified', legend=FALSE)
    legend("topright", inset=0.05, legend =rev(leyenda), fill = rev(my_colors), cex=0.5) 
    
    dev.off()
    
    
    # --------------------------------------------------- #
    # ··· APPLICATION OF THE MASKS FROM THE PRE-IMAGE ··· #
    # --------------------------------------------------- #
    
    k <- MASCARAS[i]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK_NDVI_SIMPLE/MASK"))
    MASK1 <- raster(k)
    
    MASK1 <- crop(MASK1,extent(limites))
    MASK1 <- mask(MASK1, PSEUDO_MASK_GAL)
    MASK1 <- mask(MASK1, PSEUDO_MASK)
    
    dif_NBR.mask.1 <- mask(dif_NBR_class, MASK1)
    
    #-------------------------------------------#
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PLOT_MASK_VERIFIC/MASK1"))
    
    tiff(j)
    plot(dif_NBR.mask.1,col = my_colors, main = 'Fire severity in vegetated areas (NDVI>0.2)', legend=FALSE)
    legend("topright", inset=0.05, legend =rev(leyenda), fill = rev(my_colors), cex=0.5)
    dev.off()
    
    #-------------------------------------------#
    # ·········· MASK 2 FOR ONE CLASS ········· #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_1/GTIFF_MASK2_1"))
    MASK2 <- raster(k)
    MASK2 <- crop(MASK2,extent(limites))
    
    dif_NBR.mask.2 <- mask(dif_NBR.mask.1, MASK2)
    
    #-------------------------------------------#
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PLOT_MASK_VERIFIC/MASK2"))
    
    tiff(j)
    plot(dif_NBR.mask.2, col = my_colors, main = 'Fire severity in forest areas (according to K-Means Classification)', legend=FALSE)
    legend("topright", inset=0.05, legend =rev(leyenda), fill = rev(my_colors), cex=0.5)
    dev.off()
    
    #-------------------------------------------#
    # ········· MASK 2 FOR TWO CLASSES ········ #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/MASK2_KMEANS_COMUN/MASK2_TOTALES/MASK2_2/GTIFF_MASK2_2"))
    
    MASK2_2 <- raster(k)
    MASK2_2 <- crop(MASK2_2,extent(limites))
    
    dif_NBR.mask.2_2 <- mask(dif_NBR.mask.1, MASK2_2)
    
    #-------------------------------------------#
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PLOT_MASK_VERIFIC/MASK2_2"))
    
    tiff(j)
    plot(dif_NBR.mask.2_2, col = my_colors, main = 'Fire severity in forest and shrubland areas (according to K-Means Classification)', legend=FALSE)
    legend("topright", inset=0.05, legend =rev(leyenda), fill = rev(my_colors), cex=0.5)
    dev.off()
    
    # ---------------------------------------------------- #
    # ··· APPLICATION OF THE MASKS FROM THE POST-IMAGE ··· #
    # ---------------------------------------------------- #
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/IMAGENES_NUBES_BITS_CONJUNTO/BINA_SUAVIZADA/TIFF"))
    
    MASK3 <- raster(j)
    MASK3 <- crop(MASK3,extent(limites))
    MASK3 <- reclassify(MASK3, c(-Inf,0.5,NA, 0.6,Inf,1))
    
    #-------------------------------------------#
    # ··············· FOR MASK 2 ·············· #
    
    dif_NBR.mask.3 <- mask(dif_NBR.mask.2, MASK3)
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PLOT_MASK_VERIFIC/MASK3"))  
    
    tiff(j)
    plot(dif_NBR.mask.3, col = my_colors, main = 'Fire severity in forest areas (according to K-Means Classification) without clouds', legend=FALSE)
    legend("topright", inset=0.05, legend =rev(leyenda), fill = rev(my_colors), cex=0.5)
    dev.off()
    
    #-------------------------------------------#
    # ·············· FOR MASK 2_2 ············· #
    
    dif_NBR.mask.3_2 <- mask(dif_NBR.mask.2_2, MASK3)
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PLOT_MASK_VERIFIC/MASK3_2"))  
    
    tiff(j)
    plot(dif_NBR.mask.3_2, col = my_colors, main = 'Fire severity in forest and shrubland areas (according to K-Means Classification) without clouds', legend=FALSE)
    legend("topright", inset=0.05, legend =rev(leyenda), fill = rev(my_colors), cex=0.5)
    dev.off()
    #-------------------------------------------#
    #=====================================================#
    # --------------------------------------------------- #
    # ·············· PERIMETER OBTAINING ················ #
    # --------------------------------------------------- #
    
    peri_limitante <- dif_NBR.mask.3     # WITH DNBR=8, WILL BE USED TO ACTUALLY IDENTIFY BURNED FOREST AREAS #
    peri_limitante_2 <- dif_NBR.mask.3_2 # WITH DNBR=8, WILL BE USED TO ACTUALLY IDENTIFY BURNED AREAS IN BUSH AND FOREST #
    
    peri_limitante_CASE2 <- dif_NBR.mask.3_2  # WITH DNBR=7.5 AND AREA >12 #
    
    peri_total <- dif_NBR.mask.3_2 # ONCE ZONES ARE IDENTIFIED, TRUE PERIMETER SELECTED BY LOCALIZATION WITH PREVIOUS PERIMETERS #
    peri_total_CASE2 <- dif_NBR.mask.3_2 
    
    
    # -------------------------------------------------------------------------------------------------------------------------- #
    # ========================================================================================================================== #
    # ·························································································································· #
    # ---------------------- PERIMETERS UNDER CASE 1 - START CONDITION: dNBR=8 / AREA=0.5 / LOWEST REFLECTANCE ----------------- #
    # ·························································································································· #  
    # ========================================================================================================================== #  
    # -------------------------------------------------------------------------------------------------------------------------- #  
    
    for (random1 in 1:1) {
      
      #=====================================================# 
      # --------------------------------------------------- #
      # ······· RESTRICTIVE PERIMETER 1 OF LOCATION ······· #
      
      peri_limitante[peri_limitante < 7.75]<-NA  # DNBR = 8 HIGH SEVERITY #
      
      PERIMETRO_limitante <- as_Spatial(st_as_sf(st_as_stars(peri_limitante),
                                                 as_points = FALSE, merge = TRUE))
      
      # GEOMETRY CHECKING #
      rgeos::gIsValid(PERIMETRO_limitante)
      
      # INTERSECTION ELIMINATION #
      PERIMETRO_limitante <- rgeos::gBuffer(PERIMETRO_limitante, byid = TRUE, width = 0 ) # WITHOUT BUFFER (= 0) #
      
      # VALIDITY OF GEOMETRIES (CHECKING) #
      rgeos::gIsValid(PERIMETRO_limitante)
      
      PERIMETRO_limitante<-st_as_sf(PERIMETRO_limitante) # BECOMES AN SIMPLE FEATURE #
      PERIMETRO_limitante<-st_union(PERIMETRO_limitante) # DISSOLVES GEOMETRIES #
      PERIMETRO_limitante<-st_cast(PERIMETRO_limitante,"POLYGON",do_split=TRUE) # SEPARATES GEOMETRY #
      PERIMETRO_limitante<-st_as_sf(PERIMETRO_limitante) # IDENTIFY GEOMETRY #
      
      PERIMETRO_limitante$supf_ha<-as.numeric(st_area(PERIMETRO_limitante)/10000)
      PERIMETRO_limitante2<-PERIMETRO_limitante[which(PERIMETRO_limitante$supf_ha > Superf_min_20),] # MINIMUM AREA #
      
      # ----- CONDITION ------ #
      if(length(PERIMETRO_limitante2$supf_ha) == 0) {
        print("No SEVERE AREAS matches were found for the LIMITING AREA in:")
        print(j)
        next}
      # --------------------- #
      
      PERIMETRO_limitante.buffer<-st_buffer(PERIMETRO_limitante2,60) #BUFFER OF 60m - 2 PIXEL LANDSAT
      
      interseccion_PERIMETRO_limitante <-st_intersects(PERIMETRO_limitante.buffer,PERIMETRO_limitante)
      interseccion_PERIMETRO_limitante <-unlist(interseccion_PERIMETRO_limitante,recursive =FALSE)
      interseccion_PERIMETRO_limitante <-unique(interseccion_PERIMETRO_limitante)
      
      PERIMETRO_limitante.def<-PERIMETRO_limitante[interseccion_PERIMETRO_limitante,]
      
      #=====================================================# 
      # --------------------------------------------------- #
      # ······· RESTRICTIVE PERIMETER 2 OF LOCATION ······· #
      
      peri_limitante_2[peri_limitante_2 < 7.75]<-NA  #DIF_NBR = 8 SEVERO
      
      PERIMETRO_limitante_2 <- as_Spatial(st_as_sf(st_as_stars(peri_limitante_2),
                                                   as_points = FALSE, merge = TRUE))
      
      # GEOMETRY CHECKING #
      rgeos::gIsValid(PERIMETRO_limitante_2)
      
      # INTERSECTION ELIMINATION #
      PERIMETRO_limitante_2 <- rgeos::gBuffer(PERIMETRO_limitante_2, byid = TRUE, width = 0 ) # WITHOUT BUFFER (= 0) #
      
      # VALIDITY OF GEOMETRIES (CHECKING) #
      rgeos::gIsValid(PERIMETRO_limitante_2)
      
      PERIMETRO_limitante_2<-st_as_sf(PERIMETRO_limitante_2) # BECOMES AN SIMPLE FEATURE #
      PERIMETRO_limitante_2<-st_union(PERIMETRO_limitante_2) # DISSOLVES GEOMETRIES #
      PERIMETRO_limitante_2<-st_cast(PERIMETRO_limitante_2,"POLYGON",do_split=TRUE) # SEPARATES GEOMETRY # 
      PERIMETRO_limitante_2<-st_as_sf(PERIMETRO_limitante_2) # IDENTIFY GEOMETRY #
      
      PERIMETRO_limitante_2$supf_ha<-as.numeric(st_area(PERIMETRO_limitante_2)/10000)
      PERIMETRO_limitante_2.2<-PERIMETRO_limitante_2[which(PERIMETRO_limitante_2$supf_ha > Superf_min_20),] # MINIMUM AREA #
      
      # ----- CONDITION ------ #
      if(length(PERIMETRO_limitante_2.2$supf_ha) == 0) {
        print("No SEVERE AREAS matches were found for the LIMITING AREA 2 in:")
        print(j)
        next}
      # --------------------- #
      
      PERIMETRO_limitante_2.buffer<-st_buffer(PERIMETRO_limitante_2.2,60) #BUFFER OF 60m - 2 PIXEL LANDSAT
      
      interseccion_PERIMETRO_limitante_2 <-st_intersects(PERIMETRO_limitante_2.buffer,PERIMETRO_limitante_2)
      interseccion_PERIMETRO_limitante_2 <-unlist(interseccion_PERIMETRO_limitante_2,recursive =FALSE)
      interseccion_PERIMETRO_limitante_2 <-unique(interseccion_PERIMETRO_limitante_2)
      
      PERIMETRO_limitante_2.def<-PERIMETRO_limitante_2[interseccion_PERIMETRO_limitante_2,]
      
      #============================================================# 
      # ---------------------------------------------------------- #
      # ······· GENERAL RESTRICTIVE PERIMETER FOR LOCATION ······· #
      
      PERIMETRO_limitante_DEFINITIVO.def <- PERIMETRO_limitante_2.def[PERIMETRO_limitante.def,]   
      
      # ----- SAVE RESULTS ------ #  
      
      p <- PERI[i]
      
      #--- RESTRICTIVE PERIMETER 1---#
      st_crs(PERIMETRO_limitante.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERI_LIMITANTE/LIMITANTE_1"))
      st_write(PERIMETRO_limitante.def,
               p)
      
      #--- RESTRICTIVE PERIMETER 2 ---#
      st_crs(PERIMETRO_limitante_2.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERI_LIMITANTE/LIMITANTE_2"))
      st_write(PERIMETRO_limitante_2.def,
               p)
      
      #--- GENERAL RESTRICTIVE PERIMETER ---#
      st_crs(PERIMETRO_limitante_DEFINITIVO.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERI_LIMITANTE/LIMITANTE_DEFINITIVO"))
      st_write(PERIMETRO_limitante_DEFINITIVO.def,
               p)
      #=====================================================# 
      # --------------------------------------------------- #
      # ········ TOTAL PERIMETER OF AFFECTED AREA ········· #   
      
      peri_total[peri_total < 6.25] <- NA  # HIGH SEVERITY - MODERATE-HIGH SEVERITY - MODERATE-LOW SEVERITY 2 #
      
      PERIMETRO_total <- as_Spatial(st_as_sf(st_as_stars(peri_total),
                                             as_points = FALSE, merge = TRUE))
      
      # GEOMETRY CHECKING #
      rgeos::gIsValid(PERIMETRO_total)
      
      # INTERSECTION ELIMINATION #
      PERIMETRO_total <- rgeos::gBuffer(PERIMETRO_total, byid = TRUE, width = 0 ) # WITHOUT BUFFER (= 0) #
      
      # VALIDITY OF GEOMETRIES (CHECKING) #
      rgeos::gIsValid(PERIMETRO_total)
      
      PERIMETRO_total<-st_as_sf(PERIMETRO_total) # BECOMES AN SIMPLE FEATURE #
      PERIMETRO_total<-st_union(PERIMETRO_total) # DISSOLVES GEOMETRIES #
      PERIMETRO_total<-st_cast(PERIMETRO_total,"POLYGON",do_split=TRUE) # SEPARATES GEOMETRY # 
      PERIMETRO_total<-st_as_sf(PERIMETRO_total) # IDENTIFY GEOMETRY #
      
      PERIMETRO_total$supf_ha<-as.numeric(st_area(PERIMETRO_total)/10000)
      PERIMETRO_total2<-PERIMETRO_total[which(PERIMETRO_total$supf_ha >= Superf_min),] # GENERAL MINIMUM AREA #
      
      # ----- CONDITION ------ #
      if(length(PERIMETRO_total2$supf_ha) == 0) {
        print("No TOTAL AREA matches were found in:")
        print(j)
        next}
      # --------------------- #
      
      PERIMETRO_total.buffer<-st_buffer(PERIMETRO_total2,60) #BUFFER OF 60m - 2 PIXEL LANDSAT
      
      interseccion_PERIMETRO_total <-st_intersects(PERIMETRO_total.buffer,PERIMETRO_total)
      interseccion_PERIMETRO_total <-unlist(interseccion_PERIMETRO_total,recursive =FALSE)
      interseccion_PERIMETRO_total <-unique(interseccion_PERIMETRO_total)
      
      PERIMETRO_total.def <-PERIMETRO_total[interseccion_PERIMETRO_total,]
      
      # ----- SAVE RESULTS ------ #
      
      p <- PERI[i]
      
      st_crs(PERIMETRO_total.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERI_TOTAL"))
      st_write(PERIMETRO_total.def,
               p)   
      
      #===============================================================# 
      # ------------------------------------------------------------- #
      # ········ SELECTION OF DEFINITIVE GENERAL PERIMETERS ········· #
      
      PERIMETRO_TOTAL_GENERAL.def <-PERIMETRO_total.def[PERIMETRO_limitante_DEFINITIVO.def,]
      PERIMETRO_TOTAL_GENERAL.def <-PERIMETRO_TOTAL_GENERAL.def[which(PERIMETRO_TOTAL_GENERAL.def$supf_ha > Superf_min),]
      
      # ----- CONDITION ------ #
      if(length(PERIMETRO_TOTAL_GENERAL.def$supf_ha) == 0) {
        print("FINAL POLYGON could not be generated - NO INTERSECTION:")
        print(j)
        next}
      # --------------------- #
      
      # ----- SAVE RESULTS ------ #
      
      p <- PERI[i]
      
      st_crs(PERIMETRO_TOTAL_GENERAL.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERIMETRO_DEFINITIVO"))
      st_write(PERIMETRO_TOTAL_GENERAL.def,
               p)
      
    }
    
    # -------------------------------------------------------------------------------------------------------------------------- #
    # ========================================================================================================================== #
    # ·························································································································· #
    # -------------------- PERIMETERS UNDER CASE 2 - START CONDITION: dNBR=7.5 / AREA=12 / 2 LOWER REFLECTANCE ----------------- #
    # ·························································································································· #  
    # ========================================================================================================================== #  
    # -------------------------------------------------------------------------------------------------------------------------- # 
    
    for (random2 in 1:1){
      
      #=====================================================# 
      # --------------------------------------------------- #
      # ········ SECOND CASE RESTRICTIVE PERIMETER  ······· #
      
      peri_limitante_CASE2[peri_limitante_CASE2 < 7.25]<-NA  # DNBR = 7.5 Moderate-high severity' #
      
      PERI_limitante_CASE2 <- as_Spatial(st_as_sf(st_as_stars(peri_limitante_CASE2),
                                                  as_points = FALSE, merge = TRUE))
      
      # GEOMETRY CHECKING #
      rgeos::gIsValid(PERI_limitante_CASE2)
      
      # INTERSECTION ELIMINATION #
      PERI_limitante_CASE2 <- rgeos::gBuffer(PERI_limitante_CASE2, byid = TRUE, width = 0 ) # WITHOUT BUFFER (= 0) #
      
      # VALIDITY OF GEOMETRIES (CHECKING) #
      rgeos::gIsValid(PERI_limitante_CASE2)
      
      PERI_limitante_CASE2<-st_as_sf(PERI_limitante_CASE2) # BECOMES AN SIMPLE FEATURE #
      PERI_limitante_CASE2<-st_union(PERI_limitante_CASE2) # DISSOLVES GEOMETRIES #
      PERI_limitante_CASE2<-st_cast(PERI_limitante_CASE2,"POLYGON",do_split=TRUE) # SEPARATES GEOMETRY #
      PERI_limitante_CASE2<-st_as_sf(PERI_limitante_CASE2) # IDENTIFY GEOMETRY #
      
      PERI_limitante_CASE2$supf_ha<-as.numeric(st_area(PERI_limitante_CASE2)/10000)
      PERI_limitante_CASE2_2<-PERI_limitante_CASE2[which(PERI_limitante_CASE2$supf_ha > Superf_min_20_),] # MINIMUM AREA CASE 2#
      
      # ----- CONDITION ------ #
      if(length(PERI_limitante_CASE2_2$supf_ha) == 0) {
        print("No SEVERE AREAS matches were found for the LIMITING AREA CASE 2 in:")
        print(j)
        next}
      # --------------------- #
      
      PERI_limitante_CASE2.buffer<-st_buffer(PERI_limitante_CASE2_2,60) #BUFFER OF 60m - 2 PIXEL LANDSAT
      
      interseccion_PERI_limitante_CASE2 <-st_intersects(PERI_limitante_CASE2.buffer,PERI_limitante_CASE2)
      interseccion_PERI_limitante_CASE2 <-unlist(interseccion_PERI_limitante_CASE2,recursive =FALSE)
      interseccion_PERI_limitante_CASE2 <-unique(interseccion_PERI_limitante_CASE2)
      
      PERIMETRO_limitante_CASE2.def<-PERI_limitante_CASE2[interseccion_PERI_limitante_CASE2,]
      
      # ----- SAVE RESULTS ------ #  
      
      p <- PERI[i]
      
      #--- RESTRICTIVE PERIMETER 1---#
      st_crs(PERIMETRO_limitante_CASE2.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/RESULTS_INTERMEDIOS_NBR7/PERIMETRO_LIMITANTE_NBR7"))
      st_write(PERIMETRO_limitante_CASE2.def,
               p)
      
      #=====================================================# 
      # --------------------------------------------------- #
      # ········ TOTAL PERIMETER OF AFFECTED AREA ········· #   
      
      peri_total_CASE2[peri_total_CASE2 < 6.25] <- NA  # HIGH SEVERITY - MODERATE-HIGH SEVERITY - MODERATE-LOW SEVERITY 2 #
      
      PERIMETRO_total_CASE2 <- as_Spatial(st_as_sf(st_as_stars(peri_total_CASE2),
                                                   as_points = FALSE, merge = TRUE))
      
      # GEOMETRY CHECKING #
      rgeos::gIsValid(PERIMETRO_total_CASE2)
      
      # INTERSECTION ELIMINATION #
      PERIMETRO_total_CASE2 <- rgeos::gBuffer(PERIMETRO_total_CASE2, byid = TRUE, width = 0 ) # WITHOUT BUFFER (= 0) #
      
      # VALIDITY OF GEOMETRIES (CHECKING) #
      rgeos::gIsValid(PERIMETRO_total_CASE2)
      
      PERIMETRO_total_CASE2<-st_as_sf(PERIMETRO_total_CASE2) # BECOMES AN SIMPLE FEATURE #
      PERIMETRO_total_CASE2<-st_union(PERIMETRO_total_CASE2) # DISSOLVES GEOMETRIES #
      PERIMETRO_total_CASE2<-st_cast(PERIMETRO_total_CASE2,"POLYGON",do_split=TRUE) # SEPARATES GEOMETRY # 
      PERIMETRO_total_CASE2<-st_as_sf(PERIMETRO_total_CASE2) # IDENTIFY GEOMETRY #
      
      PERIMETRO_total_CASE2$supf_ha<-as.numeric(st_area(PERIMETRO_total_CASE2)/10000)
      PERIMETRO_total_CASE2_2<-PERIMETRO_total_CASE2[which(PERIMETRO_total_CASE2$supf_ha > Superf_min),] # GENERAL MINIMUM AREA #
      
      # ----- CONDITION ------ #
      if(length(PERIMETRO_total_CASE2_2$supf_ha) == 0) {
        print("No TOTAL AREA CASE 2 matches were found in:")
        print(j)
        next}
      # --------------------- #
      
      PERIMETRO_total_CASE2.buffer<-st_buffer(PERIMETRO_total_CASE2_2,60) #BUFFER OF 60m - 2 PIXEL LANDSAT
      
      interseccion_PERIMETRO_total_CASE2 <-st_intersects(PERIMETRO_total_CASE2.buffer,PERIMETRO_total_CASE2)
      interseccion_PERIMETRO_total_CASE2 <-unlist(interseccion_PERIMETRO_total_CASE2,recursive =FALSE)
      interseccion_PERIMETRO_total_CASE2 <-unique(interseccion_PERIMETRO_total_CASE2)
      
      PERIMETRO_total_CASE2.def <-PERIMETRO_total_CASE2[interseccion_PERIMETRO_total_CASE2,]
      
      # ----- SAVE RESULTS ------ #
      
      p <- PERI[i]
      
      st_crs(PERIMETRO_total_CASE2.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/RESULTS_INTERMEDIOS_NBR7/PERIMETROS_GENERAL_NBR7"))
      st_write(PERIMETRO_total_CASE2.def,
               p)   
      
      #======================================================================# 
      # -------------------------------------------------------------------- #
      # ········ SELECTION OF DEFINITIVE GENERAL PERIMETERS CASE 2 ········· #
      
      PERIMETRO_TOTAL_GENERAL_CASE2.def <-PERIMETRO_total_CASE2.def[PERIMETRO_limitante_CASE2.def,]
      PERIMETRO_TOTAL_GENERAL_CASE2.def <-PERIMETRO_TOTAL_GENERAL_CASE2.def[which(PERIMETRO_TOTAL_GENERAL_CASE2.def$supf_ha > Superf_min),]
      
      # ----- CONDITION ------ #
      if(length(PERIMETRO_TOTAL_GENERAL_CASE2.def$supf_ha) == 0) {
        print("FINAL POLYGON CASE 2 could not be generated - NO INTERSECTION:")
        print(j)
        next}
      # --------------------- #
      
      # ----- SAVE RESULTS ------ #
      
      p <- PERI[i]
      
      st_crs(PERIMETRO_TOTAL_GENERAL_CASE2.def) <- 32629
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/PERIMETROS_DEFINITIVOS_NBR7"))
      st_write(PERIMETRO_TOTAL_GENERAL_CASE2.def,
               p)
    }
  }
  
  # -------------------------------------------------------------------------------------------------------------------------- #
  # ========================================================================================================================== #
  # ·························································································································· #
  # -------------------------------------- INTERSECTION AND DEFINITION OF FINAL PERIMETERS ----------------------------------- #
  # ·························································································································· #  
  # ========================================================================================================================== #  
  # -------------------------------------------------------------------------------------------------------------------------- # 
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERIMETRO_DEFINITIVO"))
  PERI_CASE1_list <- dir()
  PERI_CASE1_list <- str_subset(PERI_CASE1_list, "shp$")
  
  setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/PERIMETROS_DEFINITIVOS_NBR7"))
  PERI_CASE2_list <- dir()
  PERI_CASE2_list <- str_subset(PERI_CASE2_list, "shp$")
  
  # --------------------- #
  COMMON <- intersect(PERI_CASE1_list, PERI_CASE2_list)
  
  NO_COMMON_PERI_CASE1 <- setdiff(PERI_CASE1_list, PERI_CASE2_list)
  NO_COMMON_PERI_CASE2 <- setdiff(PERI_CASE2_list, PERI_CASE1_list)
  # --------------------- #
  
  if (length(NO_COMMON_PERI_CASE1) != 0){
    
    for (MN1 in NO_COMMON_PERI_CASE1){
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERIMETRO_DEFINITIVO"))
      PERIMETRO_TOTAL_GENERAL.def_ <- st_read(MN1)
      PERIMETRO_TOTAL_GENERAL.def_ <- PERIMETRO_TOTAL_GENERAL.def_[which(PERIMETRO_TOTAL_GENERAL.def_$supf_ha >= Superf_min),]
      
      st_crs(PERIMETRO_TOTAL_GENERAL.def_) <- 32629
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
      st_write(PERIMETRO_TOTAL_GENERAL.def_,
               MN1)
    }
  }
  
  # --------------------- #
  
  if (length(NO_COMMON_PERI_CASE2) != 0) {
    
    for (MN2 in NO_COMMON_PERI_CASE2){
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/PERIMETROS_DEFINITIVOS_NBR7"))
      PERIMETRO_TOTAL_GENERAL.def_ <- st_read(MN2)
      PERIMETRO_TOTAL_GENERAL.def_ <- PERIMETRO_TOTAL_GENERAL.def_[which(PERIMETRO_TOTAL_GENERAL.def_$supf_ha >= Superf_min),]
      
      st_crs(PERIMETRO_TOTAL_GENERAL.def_) <- 32629
      
      setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
      st_write(PERIMETRO_TOTAL_GENERAL.def_,
               MN2)
    }
  }
  
  # --------------------- #
  
  for (MN in COMMON) {
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/PERIMETROS1_GENERAL/PERIMETROS/SHAPES_DEF/PERIMETRO_DEFINITIVO"))
    PERIMETRO_TOTAL_GENERAL.def_ <- st_read(MN)
    PERIMETRO_TOTAL_GENERAL.def_ <- PERIMETRO_TOTAL_GENERAL.def_[which(PERIMETRO_TOTAL_GENERAL.def_$supf_ha >= Superf_min),]
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/PERIMETROS_DEFINITIVOS_NBR7"))
    PERIMETRO_TOTAL_GENERAL_CASE2.def_ <- st_read(MN)
    PERIMETRO_TOTAL_GENERAL_CASE2.def_ <- PERIMETRO_TOTAL_GENERAL_CASE2.def_[which(PERIMETRO_TOTAL_GENERAL_CASE2.def_$supf_ha >= Superf_min),]
    
    # --------------------- #
    
    PERIMETER_CASE1 <- as(PERIMETRO_TOTAL_GENERAL.def_, "Spatial")
    PERIMETER_CASE2 <- as(PERIMETRO_TOTAL_GENERAL_CASE2.def_, "Spatial")
    
    PUNTOS_NO_COINCIDENTES_case2 <- PERIMETER_CASE2[is.na(sp::over(PERIMETER_CASE2,sp::geometry(PERIMETER_CASE1))),]
    
    # --------------------- #
    
    PUNTOS_NO_COINCIDENTES_case2 <- st_as_sf(PUNTOS_NO_COINCIDENTES_case2)
    PERIMETER_CASE1 <- st_as_sf(PERIMETER_CASE1)
    
    # --------------------- #
    
    LUME_TOTAL <- rbind(PERIMETER_CASE1, PUNTOS_NO_COINCIDENTES_case2)
    LUME_TOTAL$supf_ha<-as.numeric(st_area(LUME_TOTAL)/10000)
    LUME_TOTAL<-LUME_TOTAL[which(LUME_TOTAL$supf_ha >= Superf_min),] 
    
    # ---- SAVE RESULTS --- #
    
    st_crs(LUME_TOTAL) <- 32629
    
    setwd(paste0("C:/PERTURBACIONES_FOREST_GALI/",huella,"/1_PERIMETROS_NIVEL7/RESULTADOS_LUME_TOTAL"))
    st_write(LUME_TOTAL,
             MN)
  }  
}
  



  



