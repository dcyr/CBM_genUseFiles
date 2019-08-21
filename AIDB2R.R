################################################################################
################################################################################
### Fetch some ForCS inputs from CBM Archive Index Database (AIDB)
### Dominic Cyr
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/CBM", sep ="/"))
################################################################################
################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#############
require(dplyr)

### fetching tables
x <- list.files("../AIDB/")
x <- x[grep("tbl", x)]

## load all tables (might want to prune a bit...)
for (i in seq_along(x)) {
    tmp <- x[i]
    df <- read.csv(paste0("../AIDB/", tmp))
    tmp <- gsub(".txt", "", tmp)
    assign(tmp, df)
    print(paste0("table '", tmp, "' loaded"))
}



## loading shapefiles

require(raster)
#require(ggmap)
require(rgdal)
#require(ggplot2)
#require(OpenStreetMap)
#require(mapproj)


foo <- readOGR(dsn = "C:/Users/cyrdo/Sync/Travail/ECCC/GIS/Ecoregions", layer = "ecoregions")
studyAreaLL <- spTransform(studyArea, CRS("+init=epsg:4326"))
studyAreaF <- fortify(studyArea)


spu_params <- tblSPUDefault %>%
    #rename(spuID = ) %>%
    merge(tblAdminBoundaryDefault) %>%
    merge(tblEcoBoundaryDefault, by.x = "eco_boundary_id", by.y = "id") %>%
    rename(ecozoneName = name) %>%
    filter(provinceName %in% "Quebec",
           ecozoneName %in% "Boreal Shield East")




#spatial unit-specific parameters 
spu_params <- spatial_unit %>%
    rename(spuID = id) %>%
    merge(admin_boundary, by.x = "admin_boundary_id", by.y = "id") %>%
    rename(provinceName = name) %>%
    merge(eco_boundary, by.x = "eco_boundary_id", by.y = "id") %>%
    rename(ecozoneName = name) %>%
    filter(provinceName %in% "Quebec",
           ecozoneName %in% "Boreal Shield East")

