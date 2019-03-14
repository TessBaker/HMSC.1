rm(list = ls())
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(sp)
library(rgdal)
library(maps)
library(maptools)
library(ape)
library(raster)
library(letsR)
library(sf)

setwd("~/Desktop/HMSC.1/Data /TERRESTRIAL_MAMMALS")

#### SPECIES DISTRIBUTION DATA ####

#Load world map and subset to North America
data(wrld_simpl)
NorthAmerica <- wrld_simpl[wrld_simpl$NAME %in% c("Canada","United States", "Mexico"),]
ext <- extent(NorthAmerica)
ext[2] <- -53 #Reduce eastern extent
ext[3] <- 18 #Reduce southern extent
NorthAmerica <- crop(NorthAmerica, ext)
# Species list 
QC_Species_list <- c("Alces alces", "Blarina brevicauda","Canis latrans","Canis lupus","Castor canadensis","Condylura cristata",
                     "Didelphis virginiana","Dicrostonyx hudsonius", "Erethizon dorsatum","Glaucomys sabrinus" ,
                     "Glaucomys volans","Gulo gulo","Lepus americanus","Lepus arcticus","Lontra canadensis",
                     "Lynx canadensis","Lynx rufus","Marmota monax","Martes americana","Martes pennanti",
                     "Mephitis mephitis","Microtus chrotorrhinus","Microtus pennsylvanicus","Microtus pinetorum",
                     "Mustela erminea","Mustela frenata","Mustela nivalis","Myodes gapperi","Napaeozapus insignis", "Neotamias minimus",
                     "Neovison vison","Odocoileus virginianus","Ondatra zibethicus","Parascalops breweri","Peromyscus leucopus",
                     "Peromyscus maniculatus","Phenacomys ungava","Procyon lotor","Rangifer tarandus","Sciurus carolinensis",
                     "Sorex arcticus","Sorex cinereus","Sorex fumeus","Sorex hoyi","Sorex maritimensis","Sorex palustris",
                     "Sylvilagus floridanus","Synaptomys borealis","Synaptomys cooperi", "Tamias striatus",
                     "Tamiasciurus hudsonicus","Urocyon cinereoargenteus","Ursus americanus","Ursus maritimus",
                     "Vulpes lagopus","Vulpes vulpes","Zapus hudsonius")
#WordlClim Data
w <- getData('worldclim', var = 'bio', res= 5)
w <- crop(w, ext)

# Load shapefile of terrestrial mammals
#Terrestrial_mammals <- readShapeSpatial("TERRESTRIAL_MAMMALS.shp")
T_mammals <- readOGR(dsn = ".", layer="TERRESTRIAL_MAMMALS")

#Subset to QC_species_list 
QC_mammals <- subset(T_mammals, binomial %in% QC_Species_list)

#Crop based on extent of NorthAmerica
QC_mammals_cropped <- crop(QC_mammals, ext)

#Create presence absense matrix; resolution set to match worldclim data; extent based 
#on worldclim data extent values

mammals_pa <- lets.presab(QC_mammals_cropped, xmn = -179.1667,
                          xmx = -53, ymn = 18, ymx = 83.08333,
                          resol = 0.08333333)

#write.csv(mammals_pa$Presence_and_Absence_Matrix, 
          #"Mammals_presence_absence_matrix.csv")
#Make sure all species are present
setdiff(QC_Species_list, mammals_pa$Species_name)

#Match resolution of world clim data; make sure dimensions match

#mammal_ras_resample <- resample(mammals_pa$Richness_Raster, w, "bilinear")

#Crop extent (showing entire northern hemisphers)

sp.raster <- rasterize(sp.raster <- rasterize(QC_mammals_cropped,w))



#w.pts <- rasterToPoints(w, spatial = TRUE)
#w.pts@data <- data.frame(w.pts)
#colnames(w.pts@data)[20:21] <- c("Longitude(x)", "Latitude(y)")
#data <- merge(w.pts@data, mammals_pa$Presence_and_Absence_Matrix)

for(species in QC_Species_list){sp.rasters <- stack(species$Richness_Raster)}

sp.raster <- rasterize(QC_mammals_cropped,w)

QC_mammals_cropped.1 <- QC_mammals_cropped[which(QC_mammals_cropped$presence == 1),]
QC_mammals_cropped.1 <- QC_mammals_cropped.1[which(QC_mammals_cropped.1$origin ==1),]
QC_mammals_cropped.1$binomial <- droplevels(QC_mammals_cropped.1$binomial) 


#Creating a raster at desired resolution (based on worldclim raster values)
r <- raster(nrows = 781, ncols = 1514, xmn = -179.1667, xmx = -53,
            ymn = 18, ymx = 83.08333, crs = crs(w), 
            resolution = c(0.08333333, 0.08333333), vals = NULL)

sp.stack <- stack()

for(sp in QC_Species_list){
  sp.stack <- addLayer(sp.stack,
                       rasterize(QC_mammals_cropped[QC_mammals_cropped$binomial == sp, ],
                                 r,
                                 field = "binomial",
                                 fun = function(x,...) 1))
}

b <- sum(sp.stack, na.rm= TRUE)


sp.1.stack <- stack()
for(species in QC_Species_list){
  sp.1.stack <- addLayer(sp.1.stack,
                         rasterize(QC_mammals_cropped.1[QC_mammals_cropped.1$binomial == species, ],
                                   r,
                                   field = "binomial",
                                   fun = function(x,...) 1))
}

w.sp <- stack(sp.stack, w)

w.sp.2 <- stack(sp.1.stack, w)

#convert to data frame

w.sp.pts <- rasterToPoints(w.sp, spatial = TRUE)
w.sp.pts@data <- data.frame(w.sp.pts)

pts <- rasterToPoints(sp.1.stack, spatial = TRUE)
pts@data <- data.frame(pts)


##### NEW WAY ####

presence.absence.raster <- function(mask.raster, species.data, raster.label =""){
  require(raster)
  
  #set background cells in the raster to 0
  mask.raster[!is.na(mask.raster)] <- 0
  
  #set cells that contain points to 1
  species.raster <- rasterize(species.data, mask.raster, field = 1)
  species.raster <- merge(species.raster, mask.raster)
  
  #label raster
  names(species.raster) <- raster.label
  return(species.raster)
}

library(biomod2)

#Point data 

species.dat <- as.data.frame(mammals_pa$Presence_and_Absence_Matrix)
sp.label <- "Vulpes vulpes"
#extract data and keep only x and y coordinates
xy.dat <- species.dat[,c("Longitude(x)", "Latitude(y)", QC_Species_list)]

vulpes.vulpes <- xy.dat[xy.dat$`Vulpes vulpes`==1,c("Longitude(x)","Latitude(y)")]
alces.alces <- xy.dat[xy.dat$`Alces alces`==1, c("Longitude(x)", "Latitude(y)")]

#Raster of North America
my.ras <- raster(system.file("external/bioclim/current/bio3.grd", package = "biomod2"))
my.ras <- crop(my.ras, ext)
my.ras <- resample(my.ras, w)
#Create presence absence raster
pa.raster <- presence.absence.raster(mask.raster = my.ras, 
                                     species.data = vulpes.vulpes,
                                     raster.label = sp.label)
pa.raster.2 <- presence.absence.raster(mask.raster = my.ras, 
                                     species.data = alces.alces,
                                     raster.label = "Alces alces")

cb <- stack(pa.raster, pa.raster.2)

#Names
names <- c("Longitude(x)","Latitude(y)", "Alces_alces", "Blarina_brevicauda","Canis_latrans","Canis_lupus","Castor_canadensis","Condylura_cristata",
  "Didelphis_virginiana","Dicrostonyx_hudsonius", "Erethizon_dorsatum","Glaucomys_sabrinus" ,
  "Glaucomys_volans","Gulo_gulo","Lepus_americanus","Lepus_arcticus","Lontra_canadensis",
  "Lynx_canadensis","Lynx_rufus","Marmota_monax","Martes_americana","Martes_pennanti",
  "Mephitis_mephitis","Microtus_chrotorrhinus","Microtus_pennsylvanicus","Microtus_pinetorum",
  "Mustela_erminea","Mustela_frenata","Mustela_nivalis","Myodes_gapperi","Napaeozapus_insignis", "Neotamias_minimus",
  "Neovison_vison","Odocoileus_virginianus","Ondatra_zibethicus","Parascalops_breweri","Peromyscus_leucopus",
  "Peromyscus_maniculatus","Phenacomys_ungava","Procyon_lotor","Rangifer_tarandus","Sciurus_carolinensis",
  "Sorex_arcticus","Sorex_cinereus","Sorex_fumeus","Sorex_hoyi","Sorex_maritimensis","Sorex_palustris",
  "Sylvilagus_floridanus","Synaptomys_borealis","Synaptomys_cooperi", "Tamias_striatus",
  "Tamiasciurus_hudsonicus","Urocyon_cinereoargenteus","Ursus_americanus","Ursus_maritimus",
  "Vulpes_lagopus","Vulpes_vulpes","Zapus_hudsonius")

colnames(species.dat) <- names

#Success
#Now for all species:


sp.stack <- stack()
for(species in names){
  sp.label <- species
  sp.dat <- xy.dat[xy.dat$species==1, c("Longitude(x)", "Latitude(y)")]
  pa.ras <- presence.absence.raster(mask.raster = my.ras,
                          species.data = sp.dat,
                          raster.label = sp.label)
  sp.stack <- stack(sp.stack, pa.ras)
}



