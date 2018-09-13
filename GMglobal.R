
library(SpaDES)


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

ROIsub <- data.frame(Region = c("VancouverIsland", "LowerMainland"),
                                             xmn = c(-2040398, -1942000),
                                             xmx = c(-1948121, -1873500),
                                             ymn = c(1312569, 1359500),
                                             ymx = c(1434588, 1420300))
objects <- list(ROI = ROIsub) # predefined roi character string
#objects <- list(roi = testROI) # SpatialPolygon roi - script to create testROI object below
#objects <- list() # no roi - select on map

modules <- list("loadCanopyCov","loadGMTraps", "cropReprojectData","loadPortLocations",  
                #"selectROI","combineRisk","leafletRiskMap", "trapsReportPDF",  
                "loadLcc2015", "lccToTreeCover", "calculateRisk")
#selectROI isn't much different from inputObjects so added it to cropReprojectData. Module now redundant.
parameters <- list(loadLcc2015 = list(.plotInitialTime = 1),
                   loadGMTraps = list(.plotInitialTime = 1),
                   loadPortLocations = list(.plotInitialTime = 1),
                   cropReprojectData = list(crs = "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0",
                                            res = 30,
                                            .plotInitialTime = 1, 
                                            usePlot = FALSE), 
                   lccToTreeCover = list(.plotInitialTime = 1),
                   calculateRisk = list(species = "GypsyMoth",
                                        .plotInitialTime = 1)
                   # combineRisk = list(.plotInitialTime = 1, 
                   #                    hiRisk1 = 0.5,
                   #                    hiRisk2 = 10, 
                   #                    mapHiRisk = TRUE),
                   # leafletRiskMap = list(basemap = "satellite",
                   #                       mapRisk = TRUE,
                   #                       mapHiRisk = TRUE,
                   #                       dataLayers = list("traps"),
                   #                       riskLayers = list("trapsRisk"), #IE commented out
                   #                       .plotInitialTime = 1),
                   # trapsReportPDF = list(dataName = "traps",
                   #                       fileName = "trapsReport",
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
                 times = times,
                 objects = objects)


graphics.off() 
windows(xpos=1940,ypos=10,width=21,height=11,xpinch = 114, ypinch = 114)
#quartz(w=6,h=6)
dev(width = 6, height = 6)
clearPlot()

mySim1 <- spades(mySim, debug=TRUE) 



# Testing portion

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




          
          
