rm(list = ls())
#load packages
library("randomForest")
library("caret")
library("pROC")
library("ROCR")
library("dplyr")
library("missForest")
library("tidyverse")
library("gbm")
library("pdp")
library("ggplot2")
library("iml")
library(maps) 
library(raster)
library(rgdal)
library(rminer)
library(kernlab)
library(rJava)
library(spatstat)
library(maptools)
library(mapdata)
library(doParallel)
library(xgboost)
library(sf)
library(terra)
library(Rcpp)

#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                  "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


 # library(cowplot) 
 # library(googleway) 
 # library(ggrepel) 
 # library(ggspatial)
 # library(libwgeom) 
 # library(rnaturalearth)
 # library(rnaturalearthdata)

setwd("C:/Users/gunas015/Desktop/ENM_SA")

source("functions.R") #to calculate 20% from cross validation 
source("functionsCV.R")

#Read OB locations

FMD<-read.csv("FMD_1.csv", head=TRUE,sep=",")
head(FMD)
dim(FMD)
FMD<- unique(FMD)# mapping variations of occurrences, this remove duplicates
dim(FMD)

#plotting outbreak number s

SA<- shapefile("SouthA.shp")

plot(SA, xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.4954055790001),border='black', col='light grey', lwd=0.2)
#xlim longitude, ylim latitude 
points(FMD$lon, FMD$lat, col='blue', pch=20, cex=0.25)


#_______________________For Figures_________________________________________________________

#  library("rnaturalearth")
#  library("rnaturalearthdata")
# # 
#  world<- ne_countries(scale = "medium", returnclass = "sf")
#  class(world)
# # 
#  library("ggspatial")
# # 
#  ggplot(data = world) +
#    geom_sf(fill= "antiquewhite") +
#    coord_sf(xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.4954055790001), expand = FALSE)+
#    geom_point(data=FMD,aes(x=FMD$lon,y=FMD$lat),color="brown",size=0.5)+
#    xlab("Longitude") + ylab("Latitude") +
#    #annotation_scale(location = "bl", width_hint = 0.1)+
#    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1,"in"),
#                           pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering)+ 
#    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))           

#___________________________________________________________________________________________
### Reading Bioclim rasters
#____________________________________________________________________________________________

# minimum temperature at 10 minute resolution for 12 months of 2021
bio1a<-raster("Bioclime/wc2.1_5m_bio_1.tif") ##BIO1 = Annual Mean Temperature
bio2a<-raster("Bioclime/wc2.1_5m_bio_2.tif") ##BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
bio3a<-raster("Bioclime/wc2.1_5m_bio_3.tif") ##BIO3 = Isothermality (BIO2/BIO7) (* 100) colinear with annual temperature range
bio4a<-raster("Bioclime/wc2.1_5m_bio_4.tif")##BIO4 = Temperature Seasonality (standard deviation *100)
bio5a<-raster("Bioclime/wc2.1_5m_bio_5.tif")##BIO5 = Max Temperature of Warmest Month
bio6a<-raster("Bioclime/wc2.1_5m_bio_6.tif")##BIO6 = Min Temperature of Coldest Month 
bio7a<-raster("Bioclime/wc2.1_5m_bio_7.tif")##BIO7 = Temperature Annual Range (BIO5-BIO6)
bio10a<-raster("Bioclime/wc2.1_5m_bio_10.tif") ##BIO10 = Mean Temperature of Warmest Quarter
bio11a<-raster("Bioclime/wc2.1_5m_bio_11.tif") ##BIO11 = Mean Temperature of Coldest Quarter
bio12a<-raster("Bioclime/wc2.1_5m_bio_12.tif") ##BIO12 = Annual Precipitation
bio13a<-raster("Bioclime/wc2.1_5m_bio_13.tif")##BIO13 = Precipitation of Wettest Month
bio14a<-raster("Bioclime/wc2.1_5m_bio_14.tif")##BIO14 = Precipitation of Driest Month
bio15a<-raster("Bioclime/wc2.1_5m_bio_15.tif") ##BIO15 = Precipitation Seasonality (Coefficient of Variation)
bio16a<-raster("Bioclime/wc2.1_5m_bio_16.tif")##BIO16 = Precipitation of Wettest Quarter
bio17a<-raster("Bioclime/wc2.1_5m_bio_17.tif") ##BIO17 = Precipitation of Driest Quarter


#________________________________________________________________________________________#
#      Reading livestock rasters
#________________________________________________________________________________________#
Buffalo<-raster("FAO_STAT/dataverse_files_Buffalo/5_Bf_2015_Da.tif") # This is production system,include animal densities also  
Cattle<-raster("FAO_STAT/dataverse_files_Cat/5_Ct_2015_Da.tif")
Pigs<-raster("FAO_STAT/dataverse_files_pigs/5_Pg_2015_Da.tif")
Goat<-raster("FAO_STAT/dataverse_files_goats/5_Gt_2015_Da.tif")
Sheep<-raster("FAO_STAT/dataverse_files_sheep/5_Sh_2015_Da.tif")


#________________________________________________________________________________________#
#     Roads (Do not run this, already Tif file created)
#________________________________________________________________________________________#

#Read shape file
# Road<-read_sf(dsn="C:/Users/gunas015/Desktop/ENM_SA/South Asia Roads",layer = "RoadsT")
# Road
# #convert shape to Raster
# #You can change the projection
# pgeo <- st_transform(Road, CRS('+proj=longlat +datum=WGS84'))
# 
# ## Set up a raster "template" for a 0.5 degree grid
# ext <-floor(extent(pgeo))
# rr <- raster(ext, res=0.5)
# rr <- rasterize(pgeo, rr, field=1)
# 
# ## Plot raster
# plot(rr)

#Write Raster
#writeRaster(rr, filename = "South Asia Roads/SA_Roads.tif", format = "GTiff")

#road<-raster("South Asia Roads/SA_Roads.tif")
#plot(road)

road<-raster("GRIP4_Region6_vector_fgdb/raster_road/w001001.adf")
plot(road)

#_______________________________________________________________________________#
#           Land cover NASA #Change to NDVI 
#_______________________________________________________________________________#

#to extract rdf4 files
#https://stackoverflow.com/questions/36772341/reading-hdf-files-into-r-and-converting-them-to-geotiff-rasters

# library(gdalUtilities)
# library(gdalUtils)
# library(devtools)
#install.packages("devtools")
#devtools:::install_github("gearslaboratory/gdalUtils")

#install_github("JoshOBrien/gdalUtilities")

# Provides detailed data on hdf4 files, nothing important here  

# gdalinfo("C:/Users/gunas015/Desktop/ENM_SA/NASALandcover2/MCD12C1.A2020001.006.2021362215328.hdf")
# 
# # Tells me what subdatasets are within my hdf4 MODIS files and makes them into a list
# 
# #sds <- get_subdatasets("MCD12C1.A2020001.006.2021362215328.hdf")
# sds <- get_subdatasets("C:/Users/gunas015/Desktop/ENM_SA/NASALandcover2/MCD12C1.A2020001.006.2021362215328.hdf")
# sds
# 
# gdal_setInstallation()
# gdal_setInstallation(ignore.full_scan=FALSE)
# library(rgdal)
# 
# # Extraction of metadata via `GDALinfo`    
# filename <-("C:/Users/gunas015/Desktop/ENM_SA/NASALandcover2/MCD12C1.A2020001.006.2021362215328.hdf")
# hdf<-getHdf("C:/Users/gunas015/Desktop/ENM_SA/NASALandcover2/MCD12C1.A2020001.006.2021362215328.hdf")
# gdalinfo <- GDALinfo(filename,returnScaleOffset = FALSE)
# metadata <- attr(gdalinfo,"subdsmdata")
# metadata
# 
# # Extraction of SDS string for parameter     
# sds <- metadata[grep("Land_Cover_Type1", metadata)[10]] #Column name
# sds
# sds <- sapply(strsplit(sds, "="),"[[", 2) #[[ is used for extracting single item
# sds
# # Raster import via `readGDAL`   
# sds.rg <- readGDAL(sds)
# 
# library(maptools)

#trying to convert each separate layer in to Ratser and then import ot as a Tif file 

#____________________________________________________________________________________#
#         LAND COVER FAO 
#____________________________________________________________________________________#
#land<-raster("Land Cover/luswrld/lus_wrld/vat.adf") #replaced with NDVI 
#plot(land)

#___________________________________________________________________________________#
# NDVI Data
#___________________________________________________________________________________#
land<-raster("Corpurnicus Land Cover/ndvi_merge/w001001x.adf") 
plot(land) #already cropped##
levels(land) #categorical 
#______________________________________________________________________
# LIVESTOCK PRODUCTION SYSTEMS
#________________________________________________________________________#
lps<-raster("GLPS_2011/ps_cmb/W001001.adf")
plot(lps)
levels(lps) #

#Histogram of NDVI Values
# hist(lps,
#      main = "Distribution of NDVI values",
#      xlab = "NDVI",
#      ylab= "Frequency",
#      col = "wheat",
#      xlim = c(2,14),
#      breaks = 30,
#      xaxt = 'n')
# axis(side=1, at = seq(1,14), labels = seq(1,14))


#to obtain land with vegetaion index <0.4 
veg <- reclassify(lps, cbind(-Inf, 0.4, NA))
plot(veg, main = 'Veg cover')


#_________________________________________________________________
#  CROP LAND
#________________________________________________________________________
cropl<-raster("GlcShare_v10_02/glc_shv10_02.tif")
plot(cropl)

#_____________________________________________________________________________________#
#       Cropping Raster
#________________________________________________________________________________

bio1<-crop(bio1a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
#plot(bio1)
bio2<-crop(bio2a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio3<-crop(bio3a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio4<-crop(bio4a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio5<-crop(bio5a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio6<-crop(bio6a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio7<-crop(bio7a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio10<-crop(bio10a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio11<-crop(bio11a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio12<-crop(bio12a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio13<-crop(bio13a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio14<-crop(bio14a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio15<-crop(bio15a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio16<-crop(bio16a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
bio17<-crop(bio17a,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))

#livestock

Buffalo1<-crop(Buffalo,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
#plot(Buffalo1)
Cattle1<-crop(Cattle,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
Pigs1<-crop(Pigs,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
Goat1<-crop(Goat,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
Sheep1<-crop(Sheep,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
land1<-crop(land,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
road1<-crop(road,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))

##crop land and production system
lps1<-crop(lps,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))
cropl1<-crop(cropl,extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001))


#________________________________________________________________________________________#
#       Re sampling to aggregate data 
#________________________________________________________________________________________#

param<-raster(nrow=720,ncol=1200,xmn=68.1434025400001,xmx=97.362253052,ymn=5.92373281500005,ymx=35.4954055790001)
## nrow, col double the resolution, try different resolutions 
#you may need nearly the same cell size, while shifting the cell centers. 
#In those cases, the re sample function can be used. It can do either nearest neighbor assignments
#(for categorical data) or bi linear interpolation (for numerical data).


bio_1a<-aggregate(bio1,fact=4,fun=max)
#fact<-Aggregation factor expressed as number of cells in each direction (horizontally and vertically)
bio_1b<-resample(bio_1a, param, method='bilinear') #bilinear, is for continuous,  
bio_2a<-aggregate(bio2,fact=4,fun=max)
bio_2b<-resample(bio_2a, param, method='bilinear')
bio_3a<-aggregate(bio3,fact=4,fun=max)
bio_3b<-resample(bio_3a, param, method='bilinear')
bio_4a<-aggregate(bio4,fact=4,fun=max)
bio_4b<-resample(bio_4a, param, method='bilinear')
bio_5a<-aggregate(bio5,fact=4,fun=max)
bio_5b<-resample(bio_5a, param, method='bilinear')
bio_6a<-aggregate(bio6,fact=4,fun=max)
bio_6b<-resample(bio_6a, param, method='bilinear')
bio_7a<-aggregate(bio7,fact=4,fun=max)
bio_7b<-resample(bio_7a, param, method='bilinear')
bio_10a<-aggregate(bio10,fact=4,fun=max)
bio_10b<-resample(bio_10a, param, method='bilinear')
bio_11a<-aggregate(bio11,fact=4,fun=max)
bio_11b<-resample(bio_11a, param, method='bilinear')
bio_12a<-aggregate(bio12,fact=4,fun=max)
bio_12b<-resample(bio_12a, param, method='bilinear')
bio_13a<-aggregate(bio13,fact=4,fun=max)
bio_13b<-resample(bio_13a, param, method='bilinear')
bio_14a<-aggregate(bio14,fact=4,fun=max)
bio_14b<-resample(bio_14a, param, method='bilinear')
bio_15a<-aggregate(bio15,fact=4,fun=max)
bio_15b<-resample(bio_15a, param, method='bilinear')
bio_16a<-aggregate(bio16,fact=4,fun=max)
bio_16b<-resample(bio_16a, param, method='bilinear')
bio_17a<-aggregate(bio17,fact=4,fun=max)
bio_17b<-resample(bio_17a, param, method='bilinear')
Buffaloa<-aggregate(Buffalo1, fact=4, fun=max) 
Buffalob<-resample(Buffaloa, param, method='bilinear')
Cattlea<-aggregate(Cattle1, fact=4, fun=max) 
Cattleb<-resample(Cattlea, param, method='bilinear')
Goata<-aggregate(Goat1, fact=4, fun=max) 
Goatb<-resample(Goata, param, method='bilinear')
Sheepa<-aggregate(Sheep1, fact=4, fun=max) 
Sheepb<-resample(Sheepa, param, method='bilinear')
landa<-aggregate(land1, fact=4, fun=max) 
landb<-resample(landa, param, method='ngb') #ngb for categorical, factor 
roada<-aggregate(road1, fact=4, fun=max) 
roadb<-resample(roada, param, method='bilinear')
lps1a<-aggregate(lps1, fact=4, fun=max) 
lps1b<-resample(lps1a, param, method='bilinear')
cropl1<-aggregate(cropl1, fact=4, fun=max) 
cropl1b<-resample(cropl1, param, method='bilinear')


#__________________________________________________________________________________#
#      first layer of the RasterStack
#____________________________________________________________________________________

predictors<-stack(bio_1b,bio_2b,bio_3b,bio_4b,bio_5b,bio_6b,bio_7b,bio_10b,bio_11b,bio_12b,bio_13b,bio_14b,
                  bio_15b,bio_16b,bio_17b,Buffalob,Cattleb,Goatb,Sheepb,landb,roadb,lps1b,cropl1b)
names(predictors)<-c("Annual.Mean.Temp","Mean.Diurnal.Range","Isothermality","Temp.Seasonality","Max.Temp.Warmest.Month",
                     "Min.Temp.Cold.Month","Temp.Annual.Range","Mean.Temp.Warm.Quarter","Mean.Temp.Cold.Quarter","Annual.Preci",
                     "Preci.Wet.Month","Precip.Dry.Month","Preci.Seasonality","Preci.Wet.Quarter","Preci.Dri.Quarter",
                     "Buff.Density","Cattle.Density","Goat.Density","Sheep.Density","Land.Cover","Road.Density",
                     "Production.system","Crop.land")
predictors

#All#############
btvp <-raster::extract(predictors,FMD) #unify predictors with outcome
head(btvp)
dim(btvp)

#i<- which(is.na(btvp[,1])) #to remove nas 
#btvp1<- btvp[-i, ]
#dim(btvp1)

btvp1<-na.omit(btvp)

#null data creating absence data
e <- extent(68.1434025400001,97.362253052,5.92373281500005,35.4954055790001)
plot(e) 
set.seed(42)

bg <- sampleRandom(predictors,700,ext=e) #creating absence data randomly, to match case control ratio of 1:2 
dim(bg)
plot(bg)

#join everything together
d <- rbind(cbind(pa=1, btvp1),cbind(pa=0, bg)) #pa is occurrence data, 0,1s
d <- data.frame(d)

data <- d; str(data) # raster data are continuous predictors, this act as a logistic R model, for actual data
#created by random process
colnames(data)[1] <- "Class"

################# A. Drop NAs#################

#library(dplyr)
#data <-data[complete.cases(data), ]
str(data)
 
#__________________________________________________________________________________
#         Check data correlation
#__________________________________________________________________________________

CorData <-data[-which(names(data) == "Class")] #all predictors to create a correlation matrix
#CorData
#highly correlated variables will be removed 
CorDataNum <- select_if(CorData,is.numeric) # remove variables using random F algorthm 
# # calculate correlation matrix
correlationMatrix <- cor(CorDataNum)

# summarize the correlation matrix and plot
library(ggcorrplot)
a<-ggcorrplot(correlationMatrix, type = "lower")
a

#remove correlated variables########################
hc<- findCorrelation(correlationMatrix,cutoff=0.9,verbose= FALSE,
                      names = TRUE) # put any value as a "cutoff"
hc <-sort(hc); hc
#reduced_Data<-CorDataNum[,-"Annual.Preci"]
reduced_Data<-CorDataNum[,!names(CorDataNum)%in% c("Annual.Mean.Temp",
                                                    "Mean.Temp.Cold.Quarter","Mean.Temp.Warm.Quarter",
                                                    "Min.Temp.Cold.Month","Preci.Dri.Quarter",
                                                    "Preci.Wet.Quarter","Temp.Annual.Range","Temp.Seasonality")]                    
dataR<- cbind(data[1], reduced_Data);str(data)
str(dataR)


#------------------------------------------------------------------
########Select only relevant features#################
#------------------------------------------------------------------
library("Boruta")
#comparing original attributes' importance with importance achievable at random,
#estimated using their permuted copies (shadows).using random forest
library("ranger")

X <- dataR[-which(names(dataR)== "Class")] 
Y <- dataR$Class

set.seed(124)
ImpVar<- Boruta(X, Y, doTrace = 0, maxRuns = 500) #, getImp = getImpRfGini seems to be a bit more severe compared to rfZ
print(ImpVar) # 12 attributes important, nothing unimportant compared to shadow max 

#plot with x axis label vertical
plot(ImpVar, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(ImpVar$ImpHistory),function(i)
  ImpVar$ImpHistory[is.finite(ImpVar$ImpHistory[,i]),i])
names(lz) <- colnames(ImpVar$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(ImpVar$ImpHistory), cex.axis = 0.7)

#plotImpHistory(ImpVar) #show iteration number
#set <- getSelectedAttributes(ImpVar, withTentative = TRUE)#keep tentative
#dataReduced <- data[which(names(data) %in% set)]
#data<- cbind(data[1], dataReduced);str(data)

#____________________________________________________________________________________#
#
#         Create machine learning object
#___________________________________________________________________________________#

#dataR<-dataR[,-3]
# creating a data set as training and testing 
#have to have 1's as 'Positive and 0's as negatives
as.character(dataR$Class)
dataR$Class[dataR$Class== '1']='Positive' 
dataR$Class[dataR$Class== '0']='Negative'
str(dataR)

#for XGRboost we need all numeric, everything is already numeric
#dataR$Land.Cover
#dataR$Land.Cover<-as.numeric(dataR$Land.Cover)

# create two partitions: training and testing data, .8 is 80%
inTrain<-  createDataPartition(y=dataR$Class, p = .8, list = FALSE)

data.train.descr <- dataR[inTrain,-1] #caret package, contain 12 predictors only 
data.train.class <- dataR[inTrain,1] #this is y values only, data set is separated 
data.test.descr <- dataR[-inTrain,-1]
data.test.class <- dataR[-inTrain,1] #for testing you only need y only 


### create folds for Cross validation, k is testing 10 times repeated
#obtain multiple AUCs, then average to one 
set.seed(123)
myFolds <- createMultiFolds(y=data.train.class,k=10,times=10)


# for parallel model training , how many cores are used
#registerDoParallel(cores = 10)

#------------------------------------------------------------------
#################Set up cross validation#################
#------------------------------------------------------------------

#all data - no down sampling
#cross validation methods

myControl<- trainControl(## 10-fold CV) 
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  index = myFolds,
  savePredictions=TRUE,        
  classProbs = TRUE,
  summaryFunction = twoClassSummary,  
  allowParallel=TRUE,
  selectionFunction = "best")



#________________________________________________________________________________
#
#############Logistic Regression####################
#________________________________________________________________________________

# colnames(dataR)
# #dataR$Class <- factor(dataR$Class)
# glm.fit <- glm(Class ~ Mean.Diurnal.Range+Isothermality+Max.Temp.Warmest.Month+Preci.Wet.Month+
#                  Precip.Dry.Month+Preci.Seasonality+Buff.Density+Cattle.Density+Goat.Density+
#                  Sheep.Density+Land.Cover+Road.Density, data = dataR, family = "quasibinomial")
# glm.fit
# plot(glm.fit) #to obtain residual plots 
# 
# ################________________________________________________________#####
# set.seed(125) #again this is a stochastic model so set the seed to reproducibility.
# glm.fit <- train(data.train.descr,data.train.class,
#                  method = "glm",
#                  metric="ROC", # will use accuracy instead
#                  family="binomial",
#                  trControl= myControl)
# glm.fit
# # estimate model performance in terms of a confusion matrix from repeated cross-validation
# oof <- glm.fit$pred
# 
# # consider only the best tuned model
# oof <- oof[which(oof$parameter==glm.fit$bestTune[,'parameter']),]
# 
# repeats<-glm.fit$control$repeats
# 
# glm.performance.cv <- EstimatePerformanceCV(oof=oof,repeats=repeats)
# 
# glm.performance.cv
# 
# # estimate error rates from repeated cross-validation
# glm.error.cv <- EstimateErrorRateCV(oof=oof,repeats=repeats)
# 
# rm(oof,repeats)
#________________________________________________________________________________
#
#################Random Forests #################
#_________________________________________________________________________________


# tune mtry parameters (number of predictors) and perform cross-validation on data
rf.grid <- expand.grid(.mtry=(2:6)) #number of predictors (mtry) to test
rf.grid

#Run model
set.seed(125)
rf.fit <- train(data.train.descr,data.train.class,  
                method = "rf", #chosen model
                metric = "ROC", #measure that select the optimal model
                verbose = FALSE,
                trControl = myControl,
                tuneGrid = rf.grid,# tune the model to select optimal internal node branching
                verboseIter=TRUE
)
rf.fit
plot(rf.fit) #to obtain roc 

#to measure other predictive performance 
predRF <- raster::predict(predictors, rf.fit, type='prob', ext=e)
plot(predRF, xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.495405579000),col=colorRampPalette(c("#edf8b1","#7fcdbb","#2c7fb8"))(255))
predRF


#plot(SL add=TRUE)#####################################################################
# plot(s,xlim=c(78.521315,81.362253052),ylim=c(2.918697,10.835863),maxpixels=2000000,
#      col=colorRampPalette(c("green","orange","white"))(255))
# 
# a1 <- aggregate(predRF,fact=0.1/res(predRF))
# res(predRF)
# res(a1)
# 
# s <- raster(nrow = 10000, ncol = 10000)
# extent(s) <- extent(predRF)
# s <- resample(predRF, s, method = 'bi linear') # resample output
# res(s)
#####_______________________________________________________________________________##

#convertibg ratser to an sf object/data frame  
#https://gis.stackexchange.com/questions/415674/st-as-sf-of-stars-object-reduces-the-object-extent
#
#______________________________________________________________________________________##

library(stars)
predRF_s<- st_as_stars(predRF) %>% st_as_sf(merge = TRUE)
predRF_s
predRF_s1<-as.data.frame(predRF,xy=TRUE)
predRF_s1
## filling the sf object with the converted polygon values


# a<-ggplot(data =predRF_s) +
#   geom_sf() +
#   geom_sf(data=predRF_s,aes(fill = 'layer'))+
#   scale_fill_discrete(low = "white", high = "red")+
#   coord_sf(xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.4954055790001), expand = FALSE)
  # xlab("Longitude") + ylab("Latitude") +
  # annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1,"in"),
  #                        pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering)+ 
  # theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
#   panel.background = element_rect(fill = "aliceblue"))           

#a
  
#https://stackoverflow.com/questions/68037617/fill-in-continuous-colors-in-geom-sf##
 
# ggplot(data =predRF_s)+
#   geom_sf(aes(fill =layer),color= NA)+ 
#   scale_fill_viridis_c(option = "rocket",begin = 0.1)+
#   coord_sf(xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.4954055790001),
#            expand = FALSE) +
#   xlab("Longitude") + ylab("Latitude") +
#  # annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1,"in"),
#   #                       pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering)+
#   theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5))+
#   theme_bw()
          

 
##______________As ratser map_____________________________________________________###
# predRF_s2 <- ggplot(data =predRF_s1) +
#     geom_raster(aes(x = x, y = y, fill = layer)) +
#     scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
#     theme_void() +
#     theme(
#       legend.position = "bottom"
#     )
# predRF_s2+ coord_equal()+
#   xlab("Longitude") + ylab("Latitude") +
#   annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1,"in"),
#                          pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering)+ 
#   theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
#     panel.background = element_rect(fill = "aliceblue"))



#___________________________________________________________________________________##

#estimate model performance in terms of a confusion matrix from repeated cross-validation
oof <- rf.fit$pred #01 fold cross validation -check if worked
head(oof)
tail(oof)#mtry = covariates used
# consider only the best tuned model - this checks performance
oof <- oof[oof$mtry==rf.fit$bestTune[,'mtry'],]
oof
repeats<-rf.fit$control$repeats
rf.performance.cv <- EstimatePerformanceCV(oof=oof,repeats=repeats) #Matthew's correlation coefficient (MCC)
rf.performance.cv 
# estimate error rates from repeated cross-validation
rf.error.cv <- EstimateErrorRateCV(oof=oof,repeats=repeats)
rf.error.cv
rm(oof,repeats)



## #------------------------------------------------------------------
## ################# Gradient Boosting #################
## #-----------------------------------------------------------------
#This didn't work well with this data so it seems - did XGB instead

## # #set up GBM tuning paramters
gbm.grid <-  expand.grid(interaction.depth = c(1,3,5,7,9),
                         shrinkage = 0.1,
                         n.minobsinnode = c(10), n.trees = (0:10)*200) #n.tree tests 0,50...2500 # will stop when is 10 observation in terminal node
nrow(gbm.grid)
set.seed(123)
gbm.fit <- train(data.train.descr, data.train.class,
                 method = "gbm",
                 metric = "ROC",
                 verbose = FALSE,
                 trControl = myControl,
                 ## Now specify the exact models
                 ## to evaludate:
                 tuneGrid = gbm.grid)
gbm.fit
plot(gbm.fit)
predgbm <- raster::predict(predictors, gbm.fit, type='prob', ext=e)
plot(predgbm, xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.495405579000),col=colorRampPalette(c("red","orange","white"))(255))
#you have to check with different numbers to determine what s good 


save(gbm.fit,file="FMDGBM.RData")
load("FMDGBM.RData")

###estimate model performance in terms of a confusion matrix from repeated cross-validation
oof <- gbm.fit$pred
#### consider only the best tuned model
oof <- oof[intersect(which(oof$n.trees==gbm.fit$bestTune[,'n.trees']),which(oof$interaction.depth==gbm.fit$bestTune[,'interaction.depth'])),]
repeats <- gbm.fit$control$repeats
gbm.performance.cv <- EstimatePerformanceCV(oof=oof,repeats=repeats)
gbm.performance.cv
# estimate error rates from repeated cross-validation
gbm.error.cv <- EstimateErrorRateCV(oof=oof,repeats=repeats)
gbm.error.cv

## #------------------------------------------------------------------
## ################# XGBoost #################
## #------------------------------------------------------------------ 


#XGBoost performs better than GBM with larger datasets 
# xgbGrid <-  expand.grid(nrounds = c(10,20),  
#                         max_depth = c(10, 15, 20, 25),
#                         colsample_bytree = seq(0.5, 0.9, length.out = 5),
#                         ## The values below are default values in the sklearn-api. 
#                         eta = 0.1,
#                         gamma=0,
#                         min_child_weight = 1,
#                         subsample = 1)
# nrow(xgbGrid)
# set.seed(123) 
# xgbfit <-  train( data.train.descr, data.train.class, trControl = myControl,
#                   tuneGrid = xgbGrid,
#                   method = "xgbTree")
# 
# xgbfit
# ## #
# plot(xgbfit)
# predxgbm <- raster::predict(predictors, xgbfit, type='prob', ext=e)
# plot(predxgbm,xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.495405579000),col=colorRampPalette(c("red","orange","white"))(255))
# #plot(EU, add=TRUE)
# 
# save(xgbfit,file="FMDXGB.RData")
# load("FMDXGB.RData")
# # estimate model performance in terms of a confusion matrix from repeated cross-validation
# oof <- xgbfit$pred
# # consider only the best tuned model   -need to update oof with all variables in the grid
# oof <- oof[intersect(which(oof$nrounds==xgbfit$bestTune[,'nrounds']),which(oof$max_depth==xgbfit$bestTune[,'max_depth'])),]
# repeats <-xgbfit$control$repeats
# xgb.performance.cv <- EstimatePerformanceCV(oof=oof,repeats=repeats)
# xgb.performance.cv 
# ##estimate error rates from repeated cross-validation
# xgb.error.cv <- EstimateErrorRateCV(oof=oof,repeats=repeats)
# xgb.error.cv

## 
## #------------------------------------------------------------------
## ################# Support vector machine #################
## #------------------------------------------------------------------
## #different algorithm produce different results 

set.seed(123)
svm.fit <- train(Class ~ .,data=cbind(Class=data.train.class,data.train.descr),
                 method = "svmRadial",
                 tuneLength = 9,
                 metric="ROC",
                 trControl = myControl)
svm.fit
plot(svm.fit)
predSVM <- raster::predict(predictors, svm.fit, type='prob', ext=e)
plot(predSVM,xlim=c(68.1434025400001,97.362253052),ylim=c(5.92373281500005,35.495405579000),col=colorRampPalette(c("red","orange","white"))(255))


#### estimate model performance in terms of a confusion matrix from repeated cross-validation
oof <- svm.fit$pred
#### consider only the best tuned model
oof <- oof[intersect(which(oof$sigma==svm.fit$bestTune[,'sigma']),which(oof$C==svm.fit$bestTune[,'C'])),]
repeats <- svm.fit$control$repeats
svm.performance.cv <- EstimatePerformanceCV(oof=oof,repeats=repeats)
svm.performance.cv
###estimate error rates from repeated cross-validation
svm.error.cv <- EstimateErrorRateCV(oof=oof,repeats=repeats)
rm(oof,repeats)

###------------------------------------------------------------------
################### Interpreting the best model #################
###------------------------------------------------------------------

X <-dataR[-which(names(dataR) == "Class")] #load data again for the visualization
Y <- dataR$Class

#create the iml object
#create predictor object. Add your model object name here (GLM, RF, GBM or SVM)
#to check feature importance, only one model at a time 
mod<- Predictor$new(rf.fit,data = X,y = Y) #create predictor object
mod
set.seed(123)
imp<-FeatureImp$new(mod,loss = "ce", compare='ratio', n.repetitions = 1) 
imp.dat<- imp$results
plot(imp)+ theme_bw()#plot results

#--------------------------------------------------------------------------####
##___Obtaining Color plot for feature importance_______________ ##############
#____https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-
#categorical-variables-in-ggplot2-that-have-stable-mappin
#
#---------------------------------------------------------------------------##

# ##To assign colors#########
# summary(imp.dat$feature)
# #imp.dat$feature<-as.factor(imp.dat$feature)
# 
# library(RColorBrewer)
# 
# feature.col<-c("#7FC97F","#BEAED4","#FDC086","orange","#386CB0","#F002F7",
#                       "yellow","#7fcdbb","#41b6c4","#1d91c0","red","purple","black"
#                ,"#dd1c77","#7a0177")
# 
# names(feature.col) <- levels(imp.dat$feature)
# names(feature.col)
# colScale <- scale_colour_manual(name ="Features",values=feature.col)

#________________to add column with new names, only thing that worked____________________________________________________________________________#
#             https://www.r-bloggers.com/2022/06/how-to-add-columns-to-a-data-frame-in-r/
#____________________________________________________________________________________________________________________________________________________#


labela=c(Production.system="Production systems",Isothermality="Isothermality",Mean.Diurnal.Range="Mean diurnal range",Land.Cover="NDVI",
            Cattle.Density="Cattle density",Max.Temp.Warmest.Month="Max.temp warmest month",Buff.Density="Buffalo density",
            Annual.Preci="Annual precipitation",Precip.Dry.Month="Precipitation dry month",Crop.land="Crop land",
            Sheep.Density="Sheep density",Road.Density="Road density",Preci.Seasonality="Preci.seasonality",
            Goat.Density="Goat density",Preci.Wet.Month="Precipitation wet month")


labela

##To obtain the plot###################

imp.dat1<-imp.dat %>%  
  mutate(feature2=labela)
imp.dat1

# p<-ggplot(imp.dat1,aes(x=reorder(feature2,importance),
#                    y=importance))+
#   geom_point(aes(color=feature2))+
#   geom_segment(aes(x=feature2,xend=feature2,y=1,yend=importance,color=feature2)) +
#   theme_light()+
#   scale_color_discrete(name="Variable Name") + 
#   ylab("Feature Importance") +
#   xlab("Variable Name")+
#   coord_flip()

p<-ggplot(imp.dat1,aes(x=reorder(feature2,importance),
                       y=importance))+
  geom_point()+
  geom_segment(aes(x=feature2,xend=feature2,y=1,yend=importance)) +
  theme_light()+
  ylab("Feature Importance") +
  xlab("Variable Name")+
  coord_flip()
p

#p1<-p+colScale+
#theme(legend.title = element_text(color="black", size=10, face="bold"))
#p1


####4.2 Partial dependency (PD) plots and centered individual conditional expectation (cICE) plots.
#' 
#' PD plots and cICE plots provide a valuable way to visualize what effect 
#' each predictor in the model has in shaping exposure risk. 
#' Here we plot the two most important features in our RF model 
#' (but we provide code to plot all of the relevant features).
#'  This code plots the feature effect for both classes (negative and positive).
#'   The y-axis representing the probability of parvovirus exposure. 
#'   The vertical lines on the x-axis (or 'rug plot') shows the distribution of observations across the feature. 
#'   The highlighted line in the PD plot (i.e. model average) with the other 
#'   lines (the ICE lines) reflect the predictions for one observation when we vary that feature.
#' 
#' The age exposed cICE plots demonstrate that, whilst exposure risk of parvovirus is higher in cubs ~2 years of age, some observations have a much more subtle peak. This could be evidence for an interaction effect (i.e., risk is lower for some cubs). CICE curves are diffiult to interpret for categorical predictors so we use standard ICE plots. The resultant box plot shows parvovirus exposure risk was higher in the 1992 epidemic compared to the others.
#' 
#' 
## ---- warning=F----------------------------------------------------------

#plot pd plots for the top predictors (any number appropriate for the data).
#Categorical features don't plot properly

top<- imp.dat$feature[1:5]#n = number of predictors you want to display by partial dependent plots 

# xLabs<-list(Production.system="Production systems",Isothermality="Isothermality(%)",
#          Mean.Diurnal.Range="Mean diurnal range",Cattle.Density="Cattle density",Land.cover="NDVI")

##____https://ggplot2.tidyverse.org/reference/geom_rug.html_________##

ice_curves <- lapply(top, FUN = function(x) {
  cice <- partial(rf.fit, pred.var = x, center = TRUE, ice = TRUE, which.class="Positive",
                  prob = T) #note that we center values in the plot so these are centered ICE plots (cICE)
  autoplot(cice, rug(unit(0.05, "npc")), train = dataR, alpha = 0.1) +
    ylab("c-ICE")+
    theme_bw() 
})

ice_curves

library(ggfortify)

# for (i in 1:5){ NOT WORKING
#    print(autoplot(ice_curves[i]
#                   ,labs(x=xLabs[i],y="c")))
#   }
  
#_______ https://stackoverflow.com/questions/44973795/changing-axis-titles-for-autoplot_________#


g <- autoplot(ice_curves,label.size = 2)         

xLabs <- c("Production systems","Isothermality(%)","Mean diurnal range",
           "Cattle density(no./9km\u00B2)","NDVI")

# loop over all plots and modify each individually
for (i in 1:5){
  g[i] <- g[i] + xlab(xLabs[i])
}

# display the new plot
print(g)

#put them together
# library(gridExtra) #Working, not needed#
# a<-grid.arrange(grobs = c(ice_curves), ncol = 2)
# a 



####4.3 Interactions using Friedman's H index#-----------------------------
#' 
#' Calculating Friedman's H index provides a robust way to assess
#'  the importance of interactions in shaping risk across models.
#'   The interactions identified can then be visualized using PD plots.
#' 
## ------------------------------------------------------------------------
set.seed(345)
mod <- Predictor$new(rf.fit, data = X, y=Y, type='prob', class='Positive') 
#we just want  positive class results now
###type should be change to response for GLM## 

interact <- Interaction$new(mod)
#interact
plot(interact)

b1<-plot(interact)+
  scale_y_discrete(labels=c("Max.temp warmest month","Preci.seasonality","Goat density",
                            "Cattle density","Sheep density","NDVI","Road density",
                            "Annual precipitation","Precipitation dry month","Buffalo density",
                            "Precipitation wet month","Crop land","Isothermality",
                            "Mean diurnal range","Production systems"
                            ))+
  ylab("Features")+
  theme_bw()

b1

##plot most important interactions. We could explore others though.
interact1 <- Interaction$new(mod, feature = "Production.system")

b2<-plot(interact1)+
  scale_y_discrete(labels=c("Sheep density","Goat density","Road density",
                            "Max.temp warmest month","Crop land","Annual precipitation",
                            "Buffalo density","Precipitation wet month",
                            "NDVI","Preci.seasonality","Cattle density",
                             "Precipitation dry month", "Mean diurnal range",
                            "Isothermality"))+
  ylab("Features")+
  theme_bw()+
  ggtitle("Production systems") +
  theme(plot.title =element_text(hjust = 1, vjust =-80)) ##right align hjust,vjust move down
 
b2

ggarrange(b1,b2+ font("x.text", size = 1),
          labels = c("A","B"),
          ncol = 2, nrow = 1)  
#plot(interact1) 

####Explain single predictions using Shapely values#################
#To better understand model predictions, the last step is to use a game theory and 
#specifically Shapely values to understand how the model is applied to 
#individual observations (see main text for more details).
# 
###------------------------------------------------------------------------

# shapley <- Shapley$new(mod, x.interest = X[1000,])
# shapley$plot()+ theme_bw()
# results <- shapley$results #for each instance you can view these results as a table
# head(results, n = 7L)
# sum(results$phi)# <0 indicate model prediction was negative, > 0 model prediction was postive.
# 
# Y[1000] #see if observation was negative positive

###
#join everything together
#d <- rbind(cbind(pa=1, btvp), cbind(pa=0, bg))
#d <- data.frame(d)
#d[,'Land.Cover'] <- as.factor(d[,'Land.Cover'])
#dim(d)
#data <- d; str(data)
#colnames(data)[1] <- "Class"



## ------------------------------------------------------------------------
#shapley2 <- Shapley$new(mod, x.interest = X[131,]) #LUPINE (parvovirus positive)
#shapley2$plot()+ theme_bw()

##plot most important interactions. We could explore others though.
interact1 <- Interaction$new(mod, feature = "Production.system")
plot(interact1)+theme_bw()

#plot dominant interaction
pdp.obj <- FeatureEffect$new(mod, feature = c("Production.system","Land.Cover"), method='pdp')
#pdp.obj

a2<-plot(pdp.obj)+ 
  scale_fill_gradient(low = "white", high = "brown")+
  xlab("Production systems")+
  ylab("Dry month precip")+
  theme_bw()+
  guides(fill=guide_colourbar(title="P"))+ 
  theme(legend.title = element_text(face = "bold"))


a2

library(ggpubr) 

ggarrange(a,a1,a2,a3,a4 + font("x.text", size = 1),
          labels = c("A","B","C","D","E"),
          ncol = 2, nrow = 3)


#__________________________________________________________________###
#
#             Data table 
#___________________________________________________________________##


tab<-read.csv("data_figure_1.csv", head=TRUE,sep=",")

library(tidyr)
library(ggplot2)

tab$Year<-as.factor(tab$Year)
tab$Country<-as.factor(tab$Country)

tab %>%  
  ggplot(aes(x = Year, y = Number,fill=Country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Blue","Red", "Green")) +
  facet_wrap(~Country,labeller=labeller(Country = 
                        c("BD" = "Bangladesh",
                          "IN" = "India",
                          "SL" = "Sri Lanka")))+
  theme_bw() +
  theme(text = element_text(size = 9),
    axis.text.x = element_text(angle =90,vjust = 0.5, hjust=1))+
  theme(legend.position = "none")


