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
inputPathLandis <- paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis", sep = "/")
### script path
scriptPath <- paste(home, "Sync/Travail/ECCC/CBM/CBMtoLANDIS/scripts", sep = "/")

# ### fetch CBM spatial unit raster
# spuR <- raster(paste(inputPathGIS, "NIR/spuR.tif", sep = "/"))
# spuR_AT <- read.csv(paste(inputPathGIS, "NIR/spuR_AT.csv", sep = "/"))
# 
# ### fetching vegCodes (species cross-model lookup table)
# inputURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
# vegCodes <- read.csv(text = getURL(paste(inputURL, "vegCodes.csv", sep="/")))






################################################################################
landisInputs <- list.files(inputPathLandis)
### experiment specifics
area <- "ForMont"
scenario <- NULL

# might want to create loops here, or a function


### fetchj species.txt
species <- landisInputs[grep("species", landisInputs)]
species <- species[grep(area, species)]
species <- read.table(paste(inputPathLandis, species, sep = "/"),
                      skip = 1, comment.char = ">")


# ### sourcing scripts
source(paste(scriptPath, "CBMtoLANDIS_fnc.R", sep = "/"))
# sppLANDIS <- species[,1]
# ### testing the function
# sppCBM <- sppConvert(spp = sppLANDIS, inputCode = "LANDIS")
# sppLANDIS <- sppConvert(spp = sppCBM, inputCode = "CBM")


#### landtypes
landtypes <- landisInputs[grep("landtypes", landisInputs)]
landtypes <- landtypes[grep(area, landtypes)]
landtypes_AT <- landtypes[grep("txt", landtypes)]
landtypes_AT <- read.table(paste(inputPathLandis, landtypes_AT, sep = "/"),
                                 skip = 1, comment.char = ">")
landtypes <- landtypes[grep("tif", landtypes)]
landtypes <- raster(paste(inputPathLandis, landtypes, sep = "/"))

landtypeNames <- landtypes_AT[which(landtypes_AT$V1 == "yes"), "V3"]



spu <- fetchSPU_fnc(landtypes, landtypes_AT)
plot(landtypes)
#species <- read.csv()
#spp <- species$V1


# lightEstablismentTable_fnc <- function(update = F, template, ## placeholder at this point... 
#                                        LightEstablishmentTable = NULL) {
#     
#     
# } 

template <- "C:/Users/cyrdo/Sync/Travail/ECCC/CBM/CBMtoLANDIS/templates/CFORC-succession.txt"
tableNames <- c("SpeciesParameters", "DOMPools")




x <- "biomass-succession-main-inputs_ForMont_baseline"





bsMainInput <- "C:/Users/cyrdo/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/biomass-succession-main-inputs_ForMont_baseline.txt"
bsDynInput <-  "C:/Users/cyrdo/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/biomass-succession-dynamic-inputs_ForMont_baseline_BiasCorrected.txt"
forCSInput <- "C:/Users/cyrdo/Sync/Travail/ECCC/CBM/CBMtoLANDIS/templates/CFORC-succession.txt"


landisInputFetch(bsMainInput, type = "BiomassSuccession")
landisInputFetch(forCSInput, type = "ForCS")
























