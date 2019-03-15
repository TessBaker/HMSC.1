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
library(biomod2)

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

#Function to create presence/absence raster from 
#presence/absence matrix
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



#Point data (species data converted from matrix to data frame for subsetting)
species.dat <- as.data.frame(mammals_pa$Presence_and_Absence_Matrix)

#extract data and keep only x and y coordinates
xy.dat <- species.dat[,c("Longitude(x)", "Latitude(y)", QC_Species_list)]

#Raster of North America
my.ras <- raster(system.file("external/bioclim/current/bio3.grd", package = "biomod2"))
my.ras <- crop(my.ras, ext)
my.ras <- resample(my.ras, w)

#Loop through all species and stack rasters
sp.stack <- stack()
for(i in seq_along(QC_Species_list)){
  sp.label <- QC_Species_list[i]
  sp.dat <- xy.dat[,c(1,2,i+2)]
  sp.dat <- sp.dat[sp.dat[3]==1, c("Longitude(x)", "Latitude(y)")]
  pa.ras <- presence.absence.raster(mask.raster = my.ras,
                                    species.data = sp.dat,
                                    raster.label = sp.label)
  sp.stack <- stack(sp.stack, pa.ras)
}


w.sp.stack <- stack(sp.stack, w)

w.sp.stack.pts <- rasterToPoints(w.sp.stack, spatial = TRUE)
w.sp.stack.pts@data <- as.data.frame(w.sp.stack.pts)
w.sp.stack.pts@data <- na.omit(w.sp.stack.pts@data)

write.csv(w.sp.stack.pts@data, "mammals_presence_absence_IUCN.csv")

