################################################################################
################################################################################
#### This function takes LANDIS species codes (as defined in "vegCodes.csv"),
#### and returns CBM specie (as defined in AIDB)

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


################################################################################
################################################################################
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
                 by.y = c("ProvinceID", "EcoBoundar"),
                 all.y = T)
    
    freqTable <- table(spuVals$SPUID, values(landtypes))
    if(rule == "majority") {
        SPUID <- apply(freqTable, 2, function(x) as.numeric(names(x)[which.max(x)]))
    }
    return(SPUID)
}


################################################################################
################################################################################
#### ## create a list of tables for all Biomass Succession main inputs

landisInputFetch <- function(input, type) { ## 'type' is one of 'BiomassSuccession' or 'ForCS'
    require(stringr)
    if(type == "BSMain") {
        valuesSingleAll <- c("Timestep", "SeedingAlgorithm",
                             "InitialCommunities",
                             "InitialCommunitiesMap",
                             "CalibrateMode",
                             "SpinupMortalityFraction",
                             "DynamicInputFile",
                             "AgeOnlyDisturbances:BiomassParameters")
        tablesAll <- c("MinRelativeBiomass", "SufficientLight",
                       "SpeciesParameters",
                       "EcoregionParameters")
    }
    
    if(type == "BSDynamics") {
        x <- read.table(input, skip = 1, 
                        comment.char = ">")
        colnames(x) <- c("year", "landtype", "species",
                         "probEst", "maxANPP", "maxB")
        return(x)
    }
    
    
    if(type == "ForCS") {
        valuesSingleAll <- c("Timestep", "SeedingAlgorithm", "ForCSClimateFile",
                             "InitialCommunities", "InitialCommunitiesMap")
        tablesAll <- c("ForCSOutput", "SoilSpinUp", "AvailableLightBiomass",
                       "LightEstablishmentTable", "SpeciesParameters",
                       "DOMPools", "EcoSppDOMParameters", "ForCSProportions",
                       "DisturbFireTransferDOM", "DisturbOtherTransferDOM",
                       "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass",
                       "ANPPTimeSeries", "MaxBiomassTimeSeries",
                       "EstablishProbabilities", "RootDynamics",
                       "SnagData")
    }
    
    
    
    x <- readLines(input)
    valueHeaderFlags <- grep(paste(valuesSingleAll, collapse = "|"), x)
    tableHeaderFlags <- grep(paste(tablesAll, collapse = "|"), x)
    flagsAll <- c(valueHeaderFlags, tableHeaderFlags)
    flagsAll <- flagsAll[order(flagsAll)]
    
    out <- list()
    for (i in seq_along(valuesSingleAll)) {
        v <- valuesSingleAll[i]
        index <- valueHeaderFlags[i]
        
        tmp <-  x[valueHeaderFlags[i]]
        tmp <- gsub(v, "", tmp)
        # remove anything after comment char
        tmp <- strsplit(tmp, ">")[[1]]
        # remove spaces
        tmp <- gsub(" ", "", tmp)
        # replace backslash with forward slash
        tmp <- gsub("([\\])","/", tmp)
        # remove quotes
        tmp <- gsub('"',"", tmp)
        # remove tabs
        tmp <- gsub("([\t])","", tmp)
        # convert to numeric, if possible
        tmp2 <- suppressWarnings(as.numeric(tmp))
        if(!sum(is.na(tmp2)) > 0) {
            tmp <- tmp2
        }
        out[[v]] <- tmp
    }
    
    
    for (i in seq_along(tablesAll)) {
        v <- tablesAll[i]
        index <- tableHeaderFlags[i]
        if(index < max(flagsAll)) {
            index <- index : (flagsAll[which(flagsAll>index)][1]-1)
        } else {
            index <- index : length(x)
        }
        content <- x[index]
    
        ### putting all comments just after table name
        index <- which(substr(content, 1,1) == ">")
        
        header <- content[c(1,index)]
        tableContent <-  content[-c(1,index)]
        ## replacing tabs by spaces
        tableContent <- gsub("([\t])"," ", tableContent)
        
        # Dealing with strings with quotes
        tmp <- gsub('"',"QUOTE", tableContent)
        tmp_df <- matrix(ncol = 3, nrow = length(tmp))
        if(!identical(tableContent, tmp)) { ## only if necessary
            for (j in seq_along(tableContent)) {
                y <-  str_match(tmp[j], 'QUOTE(.*)QUOTE')
                tmp_df[j,1:2] <- y
                tmp_df[j,3] <- repl <- paste0("str", j)
                tmp[j] <- gsub(y[,1], repl, tmp[j])
            }
            tableContent <- tmp
        } else {
            rm(list = c("tmp", "tmp_df"))  
        }
    
    
    
    ### parsing table
        tableContent <- strsplit(tableContent, " ")
        tableContent <- lapply(tableContent, function(x) x[which(nchar(x) > 0)])
        if(exists("tmp")) {
            for (j in seq_along(tmp)) {
            tableContent[[j]] <- gsub(tmp_df[j,3], tmp_df[j,2], tableContent[[j]]) 
            
            }
        }
    
            ## removing empty lines
        tableContent <- tableContent[which(lapply(tableContent,
                                                  function(x) length(x))>0)]
        
        # ### adding row names for some tables in Biomass Succession main inputs
        # if(type == "BiomassSuccession") {
        #   names(tableContent) <- as.character(lapply(tableContent, function(x) x[1]))
        #   tableContent <- lapply(tableContent, function(x) x[-1])
        # }
    
        nElement <- as.numeric(lapply(tableContent, function(x) length(x)))
        if(length(tableContent) > 1) {
            if(nElement[1] < nElement[2]) {
                colN <- as.character(tableContent[[1]])
                tableContent <- tableContent[-1]
                names(tableContent) <- as.character(lapply(tableContent,
                                                           function(x) x[1]))
                if(v %in% c("AvailableLightBiomass", "MinRelativeBiomass")) {
                    tableContent <- lapply(tableContent,
                                           function(x) x[-1])
                }
                
            } else {
                colN <- paste0("V", 1:nElement[1])  
            }
        } else {
            colN <- paste0("V", 1:nElement[1])  
        }
        rowN <- names(tableContent)
    
    
    
    ### converting to data.frame
    tableContent <-  do.call("rbind", tableContent)
    
    ### converting to numerical if possible
    tmp <- apply(tableContent, 2, function(x) suppressWarnings(as.numeric(x)))
    
    if(class(tmp)=="matrix") {
        numericIndex <- which(apply(tmp, 2, function(x) sum(is.na(x))==0))  
        for (j in 1:ncol(tmp)) {
            if(is.na(sum(tmp[,j]))) {
                y <- tableContent[,j]
            
            } else {
                y <- tmp[,j]
            }
            if(j == 1) {
                df <- data.frame(y, stringsAsFactors = F)
            } else {
                df <- data.frame(df, y)
            }
        }
        colnames(df) <- colN 
        if(!is.null(rowN)) {
            rownames(df) <- rowN  
        }
        tableContent <- df
    } else { ## for vectors
        numericIndex <- which(!is.na(tmp)) 
        for (j in seq_along(tmp)) {
            if(is.na(tmp[j])) {
                y <- tableContent[j]
            
            } else {
                y <- tmp[j]
            }
            if(j == 1) {
                df <- data.frame(y, stringsAsFactors = F)
            } else {
                df <- data.frame(df, y)
            }
        }
        colnames(df) <- colN 
        if(!is.null(rowN)) {
            rownames(df) <- rowN  
        }
        tableContent <- df
    }
    
    
    out[[v]] <- list()
    out[[v]][["header"]] <- header
    out[[v]][["table"]] <- tableContent
    rm(list = c("tmp", "colN"))
    }
    return(out)
}


