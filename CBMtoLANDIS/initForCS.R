################################################################################
################################################################################
### Preparation Forest Carbon Succession inputs
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/CBM/CBMtoLANDIS", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
require(raster)
require(sp)
require(rgdal)
require(ggplot2)
require(broom)
require(dplyr)
require(maptools)
require(RCurl)

################################################################################
#### Sourcing scripts


################################################################################
### input paths (CBM)
inputPathGIS <- paste(home, "Sync/Travail/ECCC/GIS", sep = "/")
inputPathAIDB <- paste(home, "Sync/Travail/ECCC/CBM/AIDB", sep = "/")
### input path (LANDIS)
inputPathLandis <- paste(home, "Sync/Travail/ECCC/Landis-II/firewood_landis/inputs", sep = "/")
### script path
scriptPath <- paste(home, "Sync/Travail/ECCC/CBM/CBMtoLANDIS/scripts", sep = "/")

### fetch CBM spatial unit raster
spuR <- raster(paste(inputPathGIS, "NIR/spuR.tif", sep = "/"))
spuR_AT <- read.csv(paste(inputPathGIS, "NIR/spuR_AT.csv", sep = "/"))

### fetching vegCodes (species cross-model lookup table)
inputURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(inputURL, "vegCodes.csv", sep="/")))

### sourcing scripts
source(paste(scriptPath, "CBMtoLANDIS_fnc.R", sep = "/"))




################################################################################
landisInputs <- list.files(inputPathLandis)
### experiment specifics
area <- "MRCCentre"
scenario <- NULL

# might want to create loops here, or a function


### fetchj species.txt
species <- landisInputs[grep("species", landisInputs)]
species <- species[grep(area, species)]
species <- read.table(paste(inputPathLandis, species, sep = "/"),
                      header = F, skip = 1, comment.char = ">")

spp <- species[,1]

foo <- sppCodeConvert(spp, inputCode = "LANDIS")





species <- read.csv()
