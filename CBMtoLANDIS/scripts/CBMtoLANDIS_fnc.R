########## this function takes LANDIS species codes (as defined in "vegCodes.csv"), and returns CBM specie (as defined in AIDB)
sppConvert <- function(spp, inputCode, ### where "inputCode" is either "CBM" or "LANDIS"
                           exceptions = NULL, ## currently a placeholder for when it might be preferable to assign another species
                           landisURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master",
                           aidbURL = "https://raw.githubusercontent.com/dcyr/CBM_genUseFiles/master/AIDB") {  
    
    ### convert to character if that's not already the case
    spp <- as.character(spp)
    
    ### fetching species lookup tables
    vegCodesURL <- paste(landisURL, "vegCodes.csv", sep="/")
    vegCodes <- read.csv(text = getURL(vegCodesURL))  
    tblSpeciesTypeDefault <- read.csv(text = getURL(paste(aidbURL,
                                                  "tblSpeciesTypeDefault.txt", sep="/"))) 
    ### species names
    sppAIDB <- as.character(tblSpeciesTypeDefault$SpeciesTypeName)
    codeAIDB <-  as.character(tblSpeciesTypeDefault$CanFI_Code)
    sppCommonName <- as.character(vegCodes$commonName)
    sppLANDIS <- as.character(vegCodes$LandisCode)
    
    if(inputCode == "LANDIS") {
        index <- match(spp, sppLANDIS)
        if(sum(is.na(index))>0) {
            stop(paste("Some input code(s) couldn be found in reference list provided in",
                       vegCodesURL))
        }
        #
        sppCode <- as.character(vegCodes[index, "Code"])
        sppOut <- sppAIDB[match(sppCode, codeAIDB)]
        
        if(!is.null(exceptions)) {# enforce exceptions
            exceptionIndex <- which(spp %in% names(exceptions))
            for (i in exceptionIndex) {
                sppOut[i] <- exceptions[[spp[i]]]
            }
        }
        
        # checking for remaining NA's
        indexNA <- which(is.na(sppOut))
        if(length(indexNA)>0) {
            stop(paste("A match for the following species code couldn't be found:",
                   spp[indexNA]))
        } else {
            print("A perfect match was found for all input species code")
            names(sppOut) <- spp
            return(sppOut)
        }
    }

    if(inputCode == "CBM") {
        index <- match(spp, sppAIDB)
        #
        sppCode <- tblSpeciesTypeDefault[index, "CanFI_Code"]#"Code"]
        sppOut <- as.character(vegCodes[match(sppCode, vegCodes$Code), "LandisCode"])
        
        
        if(!is.null(exceptions)) {# enforce exceptions
            exceptionIndex <- which(spp %in% names(exceptions))
            for (i in exceptionIndex) {
                sppOut[i] <- exceptions[[spp[i]]]
            }
        }
        
        # checking for remaining NA's
        indexNA <- which(is.na(sppOut))
        if(length(indexNA)>0) {
            stop(paste("A match for the following species code couldn't be found:",
                       spp[indexNA]))
        } else {
            print("A perfect match was found for all input species code")
            names(sppOut) <- spp
            return(sppOut)
        }
    }
}



#### This function takes LANDIS landtypes (ecoregions) and returns CBM
#### spatial units ID (SPUs)
fetchSPU_fnc <- function(landtypes, landtypes_AT,
                         onlyActive = T,  ### returns values only for active landtypes
                         rule = "majority",
                         aidbURL = "https://raw.githubusercontent.com/dcyr/CBM_genUseFiles/master/AIDB",
                         spuURL = "https://raw.githubusercontent.com/dcyr/CBM_genUseFiles/master/spatial") { ### just a placeholder at the moment
    
    
    print("Matching LANDIS landtypes with CBM SPUID")
    ### LANDIS inputs
    isActive <- landtypes_AT$V1 %in% c("yes", "y", "Yes", "Y", T)
    ltNames <- as.character(landtypes_AT$V3)
    ltID <- landtypes_AT$V2
    
    if(onlyActive) {
        ltNames <- ltNames[isActive]
        ltID <- ltID[isActive]
    }
    
    ### CBM spu table
    tblSPUDefault <- read.csv(text = getURL(paste(aidbURL,
                                                  "tblSPUDefault.txt", sep="/")))
    
    ### spatial unit raster
    tmpFile <- paste0(tempfile(), ".tif")
    url <- paste(spuURL, "spuR.tif", sep="/")
    download.file(url, tmpFile, method="curl")
    spuR <- raster(tmpFile)
    ### spatial unit attribute table
    url <- paste(spuURL, "spuR_AT.csv", sep="/")
    spuR_AT <- read.csv(text = getURL(url))  
    
    ### matching landtypes with CBM spatial units
    # reproject and crop
    spuR <- projectRaster(spuR, landtypes,  method="ngb")
    # matching cells with table lines
    index <- match(values(spuR), spuR_AT$ID)
    outputVals <- c("SPUID", "EcoBoundar", "ProvinceID") ## might have to modify some code if it's anything else
    
    spuVals <- spuR_AT[index, c("EcoBoundar", "ProvinceID")]

    spuVals <- merge(tblSPUDefault, spuVals,
                 by.x = c("AdminBoundaryID", "EcoBoundaryID"),
                 by.y = c("ProvinceID", "EcoBoundar"))
    
    freqTable <- table(spuVals$SPUID, values(landtypes))
    if(rule == "majority") {
        SPUID <- apply(freqTable, 2, function(x) as.numeric(names(x)[which.max(x)]))
    }
    return(SPUID)
}
       
 
      

