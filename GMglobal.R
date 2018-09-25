
library(SpaDES)
library(raster)
#setwd("...../GMRiskMapSpaDES/") # <---- change ..... to your directory
#setwd("/Users/kaitlynschyrmann/Desktop/SpaDES GM Module 2018/GMRiskMapSpaDES/")
# setwd("C:/Brian/Projects/Spades/GMRiskMapSpaDES/")
#created an R Project so that the module so the working directory is the location of the project directory, regardless of user.  
## set simulation and module parameters 
setPaths(cachePath = file.path("cache"),
         inputPath = file.path("inputs"),
         modulePath = file.path("modules"),
         outputPath = file.path("outputs"))
paths <- getPaths()
times <- list(start=1.0, end=10.5, timeunit="year")


modules <- list("loadCanopyCov","loadGMTraps", "cropReprojectData","loadPortLocations", "combineRisk", 
                "loadLcc2015", "calculateRisk", "lccToTreeCover", "leafletRiskMap") 
#,"treeCoverClassify",  "trapsReportPDF"
#selectROI isn't much different from inputObjects so added it to cropReprojectData. Module now redundant.
parameters <- list(loadLcc2015 = list(.plotInitialTime = 11),
                   loadGMTraps = list(.plotInitialTime = 11),
                   loadPortLocations = list(.plotInitialTime = 11),
                   cropReprojectData = list(crs = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                                            res = 30,
                                            .plotInitialTime = 11, 
                                            usePlot = FALSE), 
                   lccToTreeCover = list(.plotInitialTime = 11),
                   calculateRisk = list(species = "GypsyMoth",
                                        .plotInitialTime = 11),
                   combineRisk = list(.plotInitialTime = 11,
                                      hiRisk1 = 0.5,
                                      hiRisk2 = 3,
                                      mapHiRisk = TRUE),#
                   leafletRiskMap = list(basemap = "satellite",
                                         mapRisk = TRUE,
                                         mapHiRisk = TRUE,
                                         dataLayers = list("traps"),
                                         riskLayers = list("trapsRisk"),
                                         .plotInitialTime = 11,
                                         fileName = c("VancouverIsland_Leaflet","LowerMainland_Leaflet"))#
                   # trapsReportPDF = list(dataName = "traps",
                   #                       fileName = c("VancouverIsland_trapsReport", "LowerMainland_trapsReport"), #
                   #                       saveDir=getwd(),
                   #                       popDistType = "linear",
                   #                       popMaxDist = 2000,
                   #                       popMinDist = 750,
                   #                       popMaxCatch = 8,
                   #                       basemap = "roadmap",
                   #                       mapRisk = TRUE,
                   #                       mapHiRisk = TRUE,
                   #                       .pdfInitialTime = 100)
)


# Simulation setup
mySim <- simInit(params = parameters, 
                 modules = modules, 
                 paths =  paths,
                 times = times)


graphics.off() 
windows(xpos=1940,ypos=10,width=21,height=11,xpinch = 114, ypinch = 114)
#quartz(w=6,h=6)
dev(width = 6, height = 6)
clearPlot()

mySim1 <- spades(mySim, debug=TRUE) 


# Testing portion
# Test where ROI = dataframe
# ROIsub <- data.frame(Region = c("Vancouver_Island", "Lower_Mainland"),#
#                                              xmn = c(-2040398, -1942000), #
#                                              xmx = c(-1948121, -1873500),#
#                                              ymn = c(1312569, 1359500),#
#                                              ymx = c(1434588, 1420300))#
# objects <- list(ROI = ROIsub) # predefined ROI using dataframe
#
#Test where ROI = list of spatialPolygonsDataFrames
myCrs <- "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#For some reason the SPDF couldn't be piped. 
LowerMainland = data.frame(x_coord = c(-1942000, -1942000, -1873500, -1873500), 
                           y_coord = c(1359500, 1420300, 1420300, 1359500))
LowerMainland <- Polygon(LowerMainland)
LowerMainland <- Polygons(list(LowerMainland), 1)
LowerMainland <- SpatialPolygons(list(LowerMainland))
LowerMainland <- SpatialPolygonsDataFrame(LowerMainland, data = data.frame("ID" = "Lower Mainland"))

VancouverIsland = data.frame(x_coord = c(-1948121, -1948121, -2040398, -2040398), 
                             y_coord = c(1434588, 1312569, 1312569 , 1434588)) 
VancouverIsland <-  Polygon(VancouverIsland)
VancouverIsland <-   Polygons(list(VancouverIsland), 1)
VancouverIsland <-  SpatialPolygons(list(VancouverIsland))
VancouverIsland <- SpatialPolygonsDataFrame(VancouverIsland, data = data.frame("ID" = "Vancouver Island"))

crs(VancouverIsland) <- myCrs 
crs(LowerMainland) <- myCrs

myROI = list("Vancouver Island" = VancouverIsland, "Lower Mainland" = LowerMainland)

#test
objects = list(ROI = myROI)
mySim <- simInit(params = parameters, 
                 modules = modules, 
                 paths =  paths,
                 times = times,
                 objects = objects)

mySim1 <- spades(mySim, debug = TRUE)




testROI <- mySim1$ROI

### View leafletMap in browser from outside spades
 ops1 <- options() # save orignial options
 options(viewer=NULL) 
 mySim1$leafletMap 
 options(viewer=ops1$viewer) # restore original viewer options


rm(list=ls())
testROI <- mySim1$ROI


# LowerMainland system.time: not cached
#   user     system  elapsed 
#   537.313  88.319  634.423 
# LowerMainland system.time: from cache
#   user     system  elapsed 
#   32.233   5.644   39.615 

################################################################################################################
library(raster)

# testROI
x <- c(-1935000, -1934000, -1909000, -1911000, -1917000, -1922000, -1938000, -1935000)
y <- c(1410000, 1407000, 1405000, 1414000, 1414500, 1413800, 1414200, 1410000)
testROI <- cbind(x, y) %>%
  Polygon() %>%
  list() %>%
  Polygons("s1") %>%
  list() %>%
  SpatialPolygons(1L)
crs(testROI) <- crs("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")



# ROI partially outside LCC dataset
x <- c(-1950000,-1930000, -1880000, 1900000)
y <- c(1238570, 1250000, 1228570, 1199990)
outofboundsROI <- cbind(x, y) %>%
  Polygon() %>%
  list() %>%
  Polygons("s1") %>%
  list() %>%
  SpatialPolygons(1L)
crs(outofboundsROI) <- crs("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")




          
          
