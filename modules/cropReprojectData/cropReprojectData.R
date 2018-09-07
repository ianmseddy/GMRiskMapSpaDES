
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
    createsOutput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS")
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
        crs(corners) <- P(sim)$crs
        
        tempdf <- plyr::rbind.fill(data.frame(sim$dataList[[i]]), data.frame(corners))
        coordinates(tempdf) <- ~coords.x1+coords.x2
        crs(tempdf) <- P(sim)$crs
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
  browser()
  
  if(sp::proj4string(sim$ROI) != P(sim)$crs) {
    sim$ROI <- Cache(sp::spTransform, sim$roi, sp::CRS(sim$crs))
  }
  
  templateRas <- raster::raster(ext=raster::extent(sim$ROI), 
                                resolution = P(sim)$res, 
                                crs = P(sim)$crs)
  
  #Get unique regions (we may have to make actual Region name a )
  Regions <- unique(studyArea$Region)
  
  #Make new SpatialPolygonsDataFrame for each region
  allRegions <- lapply(sim$ROI, FUN = function(x, Region = Regions) {
    a <- x[x$Region == Region,]
    return(a)
  })
  
  #Extend any rasters in dataList so that they fully overlap ROI
  sim$dataList <- lapply(Regions, FUN = function(region, files = sim$dataListInit, dir = outputPath(sim)){
    
    subList <- lappy(files, FUN = function(file, roi = region, dir = dir){
      output <- Cache(postProcess, filename1 = file, studyArea = roi, 
                      filename2 = paste(dir,"/", file, "_", roi, sep = ""))
      return(output)
    } )
    #run postProcess sim$dataListInit by region (postProcess clips, masks, and reprojects)
    
    return(subList)
    })
  names(sim$dataList) <- names(allRegions)
  return(invisible(sim))
}
