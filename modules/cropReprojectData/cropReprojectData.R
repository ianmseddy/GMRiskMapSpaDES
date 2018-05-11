
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "cropReprojectData",
  description = "Module that crops, reprojects, masks data", 
  keywords = c("crop", "reproject", "mask"), 
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", cropReprojectData = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "cropReprojectData.Rmd"),
  reqdPkgs = list("raster", "sp", "reproducible", "quickPlot", "rgeos"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("crs", "character", "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0", NA, NA, "PROJ.4 character string defining desired coordinate reference system"),
    defineParameter("res", "numeric", 30, 5, 500, "Desired raster resolution, in meters. Single number or vector of two numbers"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "dataListInit", objectClass = "List", desc = "List of data rasters. If NULL, recreates an empty list in .inputsObjects", sourceURL = NA),
    expectsInput(objectName = "roi", objectClass = c("SpatialPolygons"), desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataList", objectClass = "List", desc = "List of cropped & reprojected data"),
    createsOutput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS"),
    createsOutput(objectName = "crs", objectClass = "character", desc = "PROJ.4 character string defining desired coordinate reference system"),
    createsOutput(objectName = "res", objectClass = "numeric", desc = "Desired raster resolution, in meters. Single number or vector of two numbers")
  )
))


## event types
#   - type `init` is required for initialiazation

doEvent.cropReprojectData = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- cropReprojectDataInit(sim)
      
      # schedule future event(s) 
      sim <- scheduleEvent(sim, start(sim), "cropReprojectData", "gis")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "cropReprojectData", "save")
    },
    plot = {
      # do stuff for this event
      cropReprojectDataPlot(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "cropReprojectData", "plot")
    },
    save = {
      # do stuff for this event
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "cropReprojectData", "save")
    },
    gis = {
      # do stuff for this event
      
      sim <- cropReprojectDataGIS(sim)
      
      # schedule future event(s)
      if(is.na(P(sim)$.plotInitialTime)) {
        # if .plotInitialTime=NA, don't schedule plot event
      } else if( "plot" %in% subset(completed(sim), completed(sim)$moduleName=="cropReprojectData")$eventType == FALSE) {
        if( time(sim) >= P(sim)$.plotInitialTime) {
          sim <- scheduleEvent(sim, time(sim), "cropReprojectData", "plot")
        } else { 
          sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "cropReprojectData", "plot")
        }
      }
      # sim <- scheduleEvent(sim, time(sim) + increment, "cropReprojectData", "gis")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
cropReprojectDataInit <- function(sim) {
  
  sim$crs <- P(sim)$crs
  sim$res <- P(sim)$res
  
  return(invisible(sim))
}

### plot event:
# cropReprojectDataPlot <- function(sim) {
#   
#   Plot(sim$dataList)
#   
#   return(invisible(sim))
# }

cropReprojectDataPlot <- function(sim) {
  
  if( length(sim$dataList)==1 ) { #if only one element in dataList
    Plot(sim$dataList[[1]], title=names(sim$dataList)[1])
  } else {
    
    tempSim <- sim$dataList
    for( i in names(sim$dataList) ) {
      
      #if only one point in data, corner points added for plotting (Plot() fails with only 1 point)
      if( is(sim$dataList[[i]], "SpatialPoints") && length(sim$dataList[[i]])==1 ) {
        message("cropReprojectData: '", i, "' in dataList only has 1 point. ROI corner points added to map for reference.")
        corners <- data.frame(coords.x1 = c(xmin(sim$ROI), xmin(sim$ROI), xmax(sim$ROI), xmax(sim$ROI)),
                              coords.x2 = c(ymin(sim$ROI), ymax(sim$ROI), ymin(sim$ROI), ymax(sim$ROI)),
                              ID = c("SW", "NW", "SE", "NE"))
        coordinates(corners) <- ~coords.x1+coords.x2
        crs(corners) <- crs(mySim$crs)
        
        tempdf <- plyr::rbind.fill(data.frame(sim$dataList[[i]]), data.frame(corners))
        coordinates(tempdf) <- ~coords.x1+coords.x2
        crs(tempdf) <- crs(mySim$crs)
        tempdf$col <- ifelse(is.na(tempdf[[i]]), "white", "black")
        tempSim[[i]] <- tempdf
      }
    }
    Plot(tempSim, title=TRUE)
  }
  
  return(invisible(sim))
}


### gis event: crop and reproject dataListInit rasters
cropReprojectDataGIS <- function(sim) {
  
  sim$dataList <- list()
  
  if(sp::proj4string(sim$roi) != sim$crs) {
    sim$ROI <- Cache(sp::spTransform, sim$roi, sp::CRS(sim$crs))
  } else { sim$ROI <- sim$roi }
  
  templateRas <- raster::raster(ext=raster::extent(sim$ROI), 
                                resolution=sim$res, 
                                crs=sim$crs)
  
  for(i in 1:length(sim$dataListInit)) {
    
    if( is(sim$dataListInit[[i]], "Raster")) {
      
      dataPoly <- as(extent(sim$dataListInit[[i]]), "SpatialPolygons")
      crs(dataPoly) <- crs(sim$dataListInit[[i]])
      tempROI <- sp::spTransform(sim$ROI, crs(dataPoly))
      
      if (rgeos::gCovers(dataPoly, tempROI)) { #fully within ROI: normal gis
        sim$dataList[[i]] <- Cache(raster::projectRaster, from=sim$dataListInit[[i]], to=templateRas, method="ngb")
        sim$dataList[[i]] <- Cache(raster::mask, x = sim$dataList[[i]], mask = sim$ROI)
        names(sim$dataList)[[i]] <- names(sim$dataListInit)[[i]]
        
      } else if (rgeos::gIntersects(tempROI, dataPoly) ) { #intersects ROI: extend raster match extent
        message("cropReprojectData: '", names(sim$dataListInit)[i], "' extent intersects ROI (not fully within). Dataset extended to match ROI. Extended values = NA.")
        temp <- Cache(crop, sim$dataListInit[[i]], tempROI)
        temp <- Cache(extend, temp, tempROI, value=NA)  # new extended layer
        
        sim$dataList[[i]] <- Cache(raster::projectRaster, from=temp, to=templateRas, method="ngb")
        sim$dataList[[i]] <- Cache(raster::mask, x = sim$dataList[[i]], mask = sim$ROI)
        names(sim$dataList)[[i]] <- names(sim$dataListInit)[[i]]
        
      } else { #fully outside ROI: cannot use layer
        message("cropReprojectData: '", names(sim$dataListInit)[i], "' extent outside ROI. Data not usable with selected ROI.")
      }
      
    } else if( is(sim$dataListInit[[i]], "Spatial") ) {

      temp <- sp::spTransform(sim$dataListInit[[i]], sp::CRS(sim$crs))
      temp <- raster::crop(temp, sim$ROI)
      
      # checking if cropped points fall within ROI
      if( length(temp) == 0 ) { #fully outside ROI: cannot use layer
        message("cropReprojectData: '", names(sim$dataListInit)[i], "' extent outside ROI. Data not usable with selected ROI.")
      } else { #points within ROI: normal gis
        sim$dataList[[i]] <- temp
        names(sim$dataList)[[i]] <- names(sim$dataListInit)[[i]]
      }
    }
  }
  return(invisible(sim))
}


# .inputObjects <- function(sim) {
#   
#   # creating ROI object if not provided
#   if(is.null(sim$ROI)) {
#     x <- c(-1935000, -1934000, -1909000, -1911000, -1917000, -1922000, -1938000, -1935000)
#     y <- c(1410000, 1407000, 1405000, 1414000, 1414500, 1413800, 1414200, 1410000)
#     ROI <- cbind(x, y) %>%
#       Polygon() %>%
#       list() %>%
#       Polygons("s1") %>%
#       list() %>%
#       SpatialPolygons(1L)
#     crs(ROI) <- crs("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
#     sim$ROI <- ROI
#   }
#  
#   
#   ##### Create data if sim$dataListInit not provided
#   #if(is.null(sim$dataListInit)){
#   #  sim$dataListInit <- list()
#   #  
#   #  exampleRas <- raster::raster(ext=raster::extent(P(sim)$ROI), 
#   #                               resolution=P(sim)$res, 
#   #                               crs=raster::crs(P(sim)$crs))
#   #  values(exampleRas) <- runif(length(exampleRas), min=1, max=100)
#   #  names(exampleRas) <- "treeCover"
#   #  
#   #  sim$dataListInit[["treeCover"]] <- exampleRas
#   #}
#   
#   return(invisible(sim))
# }
### add additional events as needed by copy/pasting from above
