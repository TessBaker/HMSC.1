rm(list = ls())
library(plyr)
library(data.table)
library(dplyr)
library(NLP)
library(tm)
library(ggplot2)
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


world.clim.data <- getData('worldclim', var = 'bio', res = 5)
plot(world.clim.data,1)

#dbio <- extract(world.clim.data, dat[,5:6]) >> grabbing bioclim variables based on coordinates
#full.dat <- data.frame(cbind(dat, dbi)) >> combines original data set with bioclim variables

states <- c('Connecticut', 'Deleware', 'Maine', 'Maryland', 'Massachusets',
            'Minnesota', 'New Hampshire', 'New Jersey', 'New York', 'Pennsylvania',
            'Rhode Island', 'Vermont', 'Virginia', 'Wisconsin')


#shapefile of U.S. boundries (URL : https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html)
us_states <- readOGR("~/Desktop/cb_2017_us_state_5m/cb_2017_us_state_5m.shp")

my.states <- us_states[us_states$NAME %in% states,]
extent <- extent(my.states)

cropped.world.clim <- crop(world.clim.data, extent)
#plot(cropped.world.clim,1) #looks good

resolution <- c(xres(cropped.world.clim), yres(cropped.world.clim))

my.states.lim <- bbox(my.states)

#get species in this geographic area

# Load shapefile of terrestrial mammals
#Terrestrial_mammals <- readShapeSpatial("TERRESTRIAL_MAMMALS.shp")
T_mammals <- readOGR(dsn = ".", layer="TERRESTRIAL_MAMMALS")

northeast.us.mammals <- lets.presab(T_mammals, xmn = my.states.lim[1,1],
                                    xmx = my.states.lim[1,2],
                                    ymn = my.states.lim[2,1],
                                    ymx = my.states.lim[2,2],
                                    resol = resolution) 


#grabbibg bioclim variables based on coordinates
dbio.1 <- extract(cropped.world.clim, northeast.us.mammals$Presence_and_Absence_Matrix[,1:2]) 
 

#Convert to data frames and combine
mammals.pa.df <- data.frame(northeast.us.mammals$Presence_and_Absence_Matrix)
bioclim.var.df <- data.frame(dbio.1)
full.data <- cbind(mammals.pa.df, dbio.1)

write.csv(full.data, "Northeast_full_mammals_climate_var_th.csv")
 
### County by species 
###### 

us.counties.2 <- readOGR("/Users/students/Desktop/MAMMTERR/UScounties", "UScounties")
my.counties.2 <- subset(us.counties.2, us.counties.2$STATE_NAME %in% states)

## New and hopefully improved plan

lin <- as(my.counties.2, "SpatialLinesDataFrame")
pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))

#Add column with County, State, subset to relevant columns and change column names

map.df <- data.frame(pts)

county.names <- c()
for(counties in map.df$NAME){
  county.names <- c(county.names, paste(counties, "County", sep = " "))
}

map.df$NAME <- county.names

map.df <- map.df %>% group_by(NAME, STATE_NAME) %>%
  mutate(State.County = paste(NAME, STATE_NAME, sep = ", "))

map.df <- data.frame(map.df)

map.df <- cbind(map.df[11], map.df[1:2], map.df[9:10])

colnames(map.df) <- c("State.County", "County.Name", "State.Name", "Long.x", "Lat.y")

#map.df.matrix <- as.matrix(map.df)

#add column that has county name, state

#county.state.df <- map.df %>% group_by(id, STATE_NAME) %>%
  #mutate(State.County = paste(id, STATE_NAME, sep = ", "))



#subset to remove unnecessary information

#county.state.df <- cbind(county.state.df[1:2], county.state.df[6], 
                         #county.state.df[8], county.state.df[12])

#county.names <- county.state.df$State.County #22075, 520 unique

#county.state.matrix <- as.matrix(county.state.df)
#x <- as.matrix(county.state.df[1])
#y <- as.matrix(county.state.df[2])
#### Now to try and combine county data with species data/bioclim data
#sp.points.df <- county.state.df
#coordinates(sp.points.df) <- ~ x + y 

#TRY

sp.clim.mat <- as.matrix(full.data)
#sp.raster <- raster(sp.clim.mat,
                        #xmn = min(x), xmx = max(x),
                        #ymn = min(y), ymx = max(y), 
                        #crs = crs(world.clim.data), template = NULL) 
#sp.raster <- raster(sp.clim.mat)

#new.ras <- raster(nrow = dim(county.state.matrix), ncol = dim(sp.clim.mat)[2])
#crs(new.ras) <- crs(world.clim.data)
#extent(new.ras) <- c(min(x), max(x), min(y), max(y))
#sp.ras.2 <- rasterize(coordinates, new.ras, field = sp.clim.mat)

#spdf <- SpatialPointsDataFrame(full.data[c("Longitude.x.", "Latitude.y.")], 
                               #data = full.data[3:140], 
                               #proj4string = crs(world.clim.data))

#species.raster <- rasterFromXYZ(spdf)

#species.climate.raster <- stack()
#for(i in ncol(spdf)){
  #r <- rasterFromXYZ(spdf[i])
  #species.climate.raster <- stack(species.climate.raster, r)
#}


########### Species raster from matrix (created from IUCN shapefile) to get species by county ##

spec.pa <- full.data[1:121]
spec.pa <- as.matrix(spec.pa)
species.raster <- rasterFromXYZ(spec.pa, crs = crs(world.clim.data),
                                digits = 5)
species.raster.layers <- unstack(species.raster)

species.raster.stack <- stack()
for(raster in species.raster.layers){
  species.raster.stack <- stack(species.raster.stack, raster)
}

### ^ Not sure above is necessary
species.climate.raster.2 <- rasterFromXYZ(sp.clim.mat, crs = crs(world.clim.data),
                                          digits = 5)
species.climate.raster.layers <- unstack(species.climate.raster.2)

species.climate.raster.stack <- stack()
for(ras in species.climate.raster.layers){
  species.climate.raster.stack <- stack(species.climate.raster.stack, ras)
}

#species.by.county <- extract(species.climate.raster.stack, county.state.df[,1:2])

#species.by.county <- extract(species.climate.raster.stack, map.df[,4:5])

climate.by.county <- extract(world.clim.data, map.df[,4:5]) #correct dimensions for county data
spec.by.county <- extract(species.raster.stack, map.df[,4:5]) #Correct dimensions !!

#convert to data frames 
climate.by.county <- as.data.frame(climate.by.county)
spec.by.county <- as.data.frame(spec.by.county)
my.species <- c()
for(name in northeast.us.mammals$Species_name){
  my.species <- c(my.species, name)
}
colnames(spec.by.county) <- my.species

#Finally, combine 

species.climate.by.county <- cbind(map.df, spec.by.county, climate.by.county) #looks fine (13173); no NA

clim.by.county.sub <- cbind(map.df, climate.by.county) #fine (13173)
species.pa.by.county <- cbind(map.df, spec.by.county)

############### OBTAINING AVERAGES ########### 

bio.clim.var.avg <- aggregate(clim.by.county.sub, by = clim.by.county.sub[1], FUN = "mean") #520
species.pa.avg <- aggregate(species.pa.by.county, by = species.pa.by.county[1], FUN = "mean")

final.df <- cbind(species.pa.avg[5:6], species.pa.avg[1], 
                  species.pa.avg[7:125], bio.clim.var.avg[6:24]) #520


final.no.na <- final.df
final.no.na <- na.omit(final.no.na) #lost 8 counties:

#[1] "Danville County, Virginia"     "Halifax County, Virginia"      "Henry County, Virginia"       
#[4] "Mecklenburg County, Virginia"  "Patrick County, Virginia"      "Pittsylvania County, Virginia"
#[7] "Suffolk County, New York"      "Washington County, Maine"   

#final.no.na.names <- final.no.na$State.County
#[17] "Washington County, Wisconsin" 

### Now combining other covariates 

### Will try merging with final.no.na

#### 1. Incidence Data 

Lyme.incidence <- read.csv("./Lyme.Data/LD-Case-Counts-by-County-00-17.csv")
Lyme.incidence.2017 <- cbind(Lyme.incidence[1:2], Lyme.incidence[22])
Lyme.incidence.2017 <- subset(Lyme.incidence.2017, Lyme.incidence.2017$Stname %in% states)

write.csv(Lyme.incidence.2017, "Lyme_incidence_2017.csv")

Updated.Lyme.2017 <- read.csv("./Lyme.Data/Updated_Lyme_incidence_2017.csv")

#add column with State, County 

Updated.Lyme.2017 <- Updated.Lyme.2017 %>% group_by(Ctyname, Stname) %>%
     mutate(State.County = paste(Ctyname, Stname, sep = ", "))

##try to merge
data.try.1 <- merge(final.no.na, Updated.Lyme.2017, by = "State.County") #512 !!!! 


###### 2. Health facilities per county

Health.fac <- read.csv("./Lyme.Data/Health_care_facilities_county.csv")
state.abbrev <- c('CT', 'DE', 'ME', 'MD', 'MA', 'MN', 'NH', 'NJ', 'NY', 
                  'PA', 'RI', 'VT', 'VA', 'WI')

Health.fac <- subset(Health.fac, Health.fac$State.Abbreviation %in% state.abbrev)

#Replace state abbreviation by full state name
state.abbreviations <- Health.fac$State.Abbreviation
states.for.df <- c()
for(item in state.abbreviations){
  state <- state.name[which(state.abb == item)]
  states.for.df <- c(states.for.df, state)
}

Health.fac$State.Abbreviation <- states.for.df

#add column with county, state
Health.fac <- Health.fac %>% group_by(County.Name, State.Abbreviation) %>%
  mutate(State.County = paste(County.Name, State.Abbreviation, sep = ", "))

#Export, fix and re-import
write.csv(Health.fac, "Health_facility_counts_thesis.csv")

Health.facility.counts <- read.csv("./Lyme.Data/Health_facility_counts_updated_thesis.csv")

#These will be lost by ther merge

#[1] "Cumberland County, Virginia"     "Greensville County, Virginia"    "King and Queen County, Virginia"
#[4] "Manassas Park County, Virginia"  "Powhatan County, Virginia" 

#Get count of health facilities per county
HF.1 <- cbind(Health.facility.counts[3], Health.facility.counts[11]) #Health facility name, County, State column

health.fac.count <- table(unlist(HF.1$State.County))
health.fac.count <- as.data.frame(health.fac.count)
colnames(health.fac.count) <- c("State.County", "Facility.Count")

###########
#health.fac.names <- health.fac.count$State.County

#Combine

data.try.1 <- merge(data.try.1, health.fac.count, by = "State.County") #507 (expected loss)


##### 3. Urban Rural County Data 
#two types of urban areas: 1. UA: urban area (50,000 or more) 
#2. UC: urban cluster (between 2500 and 50000 people)
#col 4: county name, col 5: population, col 6: county area 
#col 7: urban population, col 8: percent population urban,  
#col 9: urban area, col 10: percent ubran area, col 11: urban population density
#col 12: urban area population, col 13: population percent urban area
#col 14: area of urban area by county, col 15: percent urban area
#col 16: population density urban area, col 17: urban cluster population
#col 18: percent population urban cluster, col 19: urban cluste area
#col 20: percent area urban cluster, col 21: populaiton density urban cluster
#col 22: rural population, col 23: percent population rural, col 24: rural area, 
#col 25: percent area rural, col 26: rural population density 

Urban.rural <- read.csv("./Lyme.Data/PctUrbanRural_County.csv")
Urban.rural <- subset(Urban.rural, Urban.rural$STATENAME %in% states)

concatanated.names <- c()
for(counties in Urban.rural$COUNTYNAME){
  concatanated.names <- c(concatanated.names, paste(counties, "County", sep = " "))
}

Urban.rural$COUNTYNAME <- concatanated.names

#add county, state column

Urban.rural <- Urban.rural %>% group_by(COUNTYNAME, STATENAME) %>%
  mutate(State.County = paste(COUNTYNAME, STATENAME, sep = ", "))

#Export, fix, import

write.csv(Urban.rural, "Urban_rural_data_thesis.csv")
Urban.rural.updated <- read.csv("./Lyme.Data/Urban_rural_data_updated_thesis.csv")


#Combine

data.try.1 <- merge(data.try.1, Urban.rural.updated, by = "State.County") #507 !


######## 4. Population living near parks (column named value is the associated percentage)

park.population <- read_sf("./Lyme.Data/Percent_living_near_parks/Percentage_of_Population_Living_Within_Half_a_Mile_of_a_Park_2010.shp")
park.population <- subset(park.population, park.population$state_name %in% states)

new.county.names <- c()
for(county in park.population$cnty_name){
  new.county.names <- c(new.county.names, paste(county, "County", sep = " "))
}

park.population$cnty_name <- new.county.names

#subset
park.population <- as.data.frame(park.population)
park.population <- cbind(park.population[5], park.population[3], park.population[7])

#add column with County, State
park.population <- park.population %>% group_by(cnty_name, state_name) %>%
  mutate(State.County = paste(cnty_name, state_name, sep = ", "))


#Combine

data.try.1 <- merge(data.try.1, park.population, by = "State.County") #507 !

colnames(data.try.1)[176] <- "Percent.Pop.N.Park"

######## 5. Education by county (2013-2017; percent completed college)
filenames <- list.files("./Lyme.Data/Education.by.county", pattern = "*.csv",
                        full.names = TRUE)
files <- lapply(filenames, read.csv, head = TRUE)

df.list <- list()
#add state column to each file
for(i in 1:length(states)){
  df <- files[[i]]
  State.Name <- rep(states[i], nrow(files[[i]]))
  df <- cbind(df, State.Name)
  df.list[[i]] <- df
}

education.by.county <- rbindlist(df.list)
education.by.county <- as.data.frame(education.by.county)
education.by.county <- cbind(education.by.county[2], education.by.county[10], 
                             education.by.county[9])
colnames(education.by.county) <- c("County.Name", "State.Name", "Education")

#add 'County' to county names
new.names <- c()
for(county in education.by.county$County.Name){
  new.names <- c(new.names, paste(county, "County", sep = " "))
}

education.by.county$County.Name <- new.names

#add County, state column

education.by.county <- education.by.county %>% group_by(County.Name, State.Name) %>%
  mutate(State.County = paste(County.Name, State.Name, sep = ", "))

#Remove white space from df
education.by.county <- as.data.frame(apply(education.by.county,2,function(x)gsub('\\s+', '',x)))

#Remove white space from other data frames and combine

data.try.1 <- as.data.frame(apply(data.try.1, 2, function(x)gsub('\\s+', '',x))) 

#Export, fix, import
write.csv(education.by.county, "Education_by_county_thesis.csv")

education.by.county.updated <- read.csv("./Lyme.Data/Education_by_county_updated.csv")

#remove percent symbol
percent.column <- data.frame(education.by.county.updated$Education)
percent.column <- data.frame(sapply(percent.column, 
                                                 function(x) as.numeric(gsub("%", "", x))))

education.by.county.updated <- cbind(education.by.county[4], 
                                     education.by.county[1:2],
                                     percent.column)

colnames(education.by.county.updated) <- c("State.County", "County.Name", "State.Name", "Education")

#Combine
data.try.1 <- merge(data.try.1, education.by.county.updated, by = "State.County") #507


write.csv(data, "Boosted_regression_Lyme_data_updated.csv")

####### Variable selection & PCA ##################
##### New plan : predict based on location 
library(psych)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(car)
library(mlbench)
library(caret)
library(gbm)

set.seed(1)


data <- read.csv("Northeast_full_mammals_climate_var_th.csv")

#orrelation.mat <- cor(data[,122:140])

#add long late column


data <- data %>% group_by(Longitude.x., Latitude.y.) %>%
  mutate(Coordinate = paste(Longitude.x., Latitude.y., sep = ","))

data <- na.omit(data)
#data <- data[123:142]

var.names <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9",
               "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", 
               "bio17", "bio18", "bio19")

data[var.names] <- lapply(data[var.names], as.numeric)

smp.size <- floor(0.60*nrow(data))

train.idx <- sample(seq_len(nrow(data)), size = smp.size)

train.d.set <- data[train.idx, ]
test.d.set <- data[-train.idx, ]

#boost.data <- gbm(c(Longitude.x., Latitude.y.)~ bio1 + bio2 + bio3 + bio4 + bio5 + 
                    #+                  bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + 
                    #+                  bio14 + bio15 + bio16 + bio17 + bio18 + bio19,
                  #data = train.d.set, distribution = "mgaussian", n.trees = 5000, interaction.depth = 4) 

#control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#model <- train(Y~ bio1 + bio2 + bio3 + bio4 + bio5 + 
                 #bio6 + bio7 + bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + 
                 #bio14 + bio15 + bio16 + bio17 + bio18 + bio19, data = data, method = "lvq",
               #preProcess = "scale", trControl = control)

library(glmnet)
X <- as.matrix(train.d.set[123:141])
scale.x <- scale(X)
Y <- as.matrix(train.d.set[2:3])

val.X <- as.matrix(test.d.set[123:141])

#glmmod <- glmnet(X, Y, family = "mgaussian", alpha = 1)

glmmod.2 <- glmnet(scale.x, Y, family = "mgaussian", alpha = 1, standardize = FALSE)
cvfit.2 <- cv.glmnet(scale.x, Y, family = "mgaussian", standardize = FALSE)

#cvfit <- cv.glmnet(X, Y, family = "mgaussian")

## Look at coefficients: because standardized less important coefficiencts will be smaller

#make data frame of coefficients and sort

coefficients <- coef(cvfit.2$glmnet.fit, s = cvfit.2$lambda.1se)

#long : bio1, bio4, bio10, bio11, bio12, bio14

##### PCA  #####
library(devtools)
library(scales)
library(ggbiplot)
library(factoextra)
library(PerformanceAnalytics)

clim.var <- data[123:141]

climate.pca <- prcomp(clim.var, center = TRUE, scale. = TRUE)
summary(climate.pca)
str(climate.pca)

#Biplots
fviz_pca_var(climate.pca, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#Scree Plot
fviz_eig(climate.pca)

#Access PCA results
# Eigenvale
eig.val <- get_eigenvalue(climate.pca)

#Results for Variables
res.var <- get_pca_var(climate.pca)
res.var$coord
res.var$contrib 

#^ contribution: Dim 1: bio1, bio3, bio4, bio6, bio7, bio11, bio12
#Dim 2: bio1, bio2, bio5, bio8, bio10, bio15, bio17, bio19
#Dim 3: bio13, bio16, bio18
#Dim 4: bio2, bio3

#Comparing to contribution plot and model above: bio1, bio15, bio12,bio7, bio4 seem the best 


res.var$cos2

#Correlation matrix and pairs
chart.Correlation(X, histogram = TRUE)
