
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
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"), 
    defineParameter("usePlot", "logical", FALSE, NA, NA, "Should the module plot GIS files after cropping to ROI?")
    ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "dataListInit", objectClass = "List", desc = "List of data rasters. If NULL, recreates an empty list in .inputsObjects", sourceURL = NA),
    expectsInput(objectName = "ROI", objectClass = "data.frame", 
                 desc = "data.frame defining regions of interest and their extents. Headers should include name, ymin, ymax, xmin, and xmax", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataList", objectClass = "List", desc = "List of cropped & reprojected data")
    # createsOutput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS")
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
    },
    plot = {
      # do stuff for this event
      if (P(sim)$usePlot == TRUE) {
        cropReprojectDataPlot(sim)
        # schedule future event(s)
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "cropReprojectData", "plot")
      }
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
  if (class(sim$ROI) == "data.frame") { #if ROI is a dataframe with extents, rasterize it
    temp <- vector(mode = "list", length = nrow(sim$ROI))
    vars <- c("xmn", "xmx", "ymn", "ymx")
      for (i in 1:nrow(sim$ROI)) {
      
      #This subsets each row and turns it into a raster
      subr <- sim$ROI[i, vars]
      ROI <- raster(xmn = subr$xmn, xmx = subr$xmx, ymn = subr$ymn, ymx = subr$ymx)
      crs(ROI) <-  CRS(P(sim)$crs)
      res(ROI) <- P(sim)$res
      ROI <- setValues(ROI, 1) #This is necessary if ROI is to be reprojected later
      # names(ROI) <- sim$ROI[i, 1]
      temp[[i]] <- ROI
      }
    
    names(temp) <- sim$ROI$Region
    sim$ROI <- temp
  } else if (class(sim$ROI) == "list"){ #if ROI is a list of shapefiles, rasterize them
    if (class(sim$ROI[[1]]) == "SpatialPolygonsDataFrame") { 
      outList <- lapply(sim$ROI, FUN = function(x, simRes = P(sim)$res) {
        outRas <- raster(x)
        outRas <- setValues(outRas, 1) #prevents downstream problem with reprojecting
        res(outRas) <- simRes
        return(outRas)
      })
      names(outList) <- naems(sim$ROI)
      sim$ROI <- outList
    }
  } # ROI is already a list of rasters
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
  if (P(sim)$usePlot == TRUE) {
    #Make sure that dataList is formatted correctly. Then, decide how to do this...
    for (i in 1: length(sim$dataList)) {
      tempSim <- sim$dataList[[i]]
      print(paste("plotting ", names(sim$dataList[i]), sep = ""))
      for( ii in 1:length(names(tempSim))) {
        
        #if only one point in data, corner points added for plotting (Plot() fails with only 1 point)
        if( is(tempSim[[ii]], "SpatialPointsDataFrame") && length(tempSim[[ii]])==1 ) {
          message("cropReprojectData: '", i, "' in dataList only has 1 point. ROI corner points added to map for reference.")
          corners <- data.frame(coords.x1 = c(xmin(sim$ROI[i]), xmin(sim$ROI[i]), xmax(sim$ROI[i]), xmax(sim$ROI[i])),
                                coords.x2 = c(ymin(sim$ROI[i]), ymax(sim$ROI[i]), ymin(sim$ROI[i]), ymax(sim$ROI[i])),
                                ID = c("SW", "NW", "SE", "NE"))
          coordinates(corners) <- ~coords.x1+coords.x2
          crs(corners) <- crs(mySim$crs)
          
          tempdf <- plyr::rbind.fill(data.frame(tempSim[[ii]]), data.frame(corners))
          coordinates(tempdf) <- ~coords.x1+coords.x2
          crs(tempdf) <- P(sim)$crs
          tempdf$col <- ifelse(is.na(tempdf[[ii]]), "white", "black")
          tempSim[[ii]] <- tempdf
        }
      }
      
      clearPlot()
      Plot(tempSim)
      Sys.sleep(3)#this will write ROI to every plot
    }
  }
  return(invisible(sim))
}


### gis event: crop and reproject dataListInit rasters
cropReprojectDataGIS <- function(sim) {
  #Extend any rasters in dataList so that they fully overlap ROI
  sim$dataList <- lapply(1:length(sim$ROI), FUN = function(i, files = sim$dataListInit, ROI= sim$ROI, 
                                                            dir = outputPath(sim)){
    region <- ROI[i]
    
    
    subList <- lapply(1:length(files), FUN = function(ii, subROI = region, files = dataListInit){
     
      subFile <- files[ii] # single brackets necessary to preserve names for file output
      
      if (class(subFile[[1]]) != "SpatialPointsDataFrame") {
      
        output <- Cache(postProcess, x = subFile[[1]], rasterToMatch = subROI[[1]], 
                      filename2 = paste(dir,"/", names(subFile), "_", names(subROI), sep = ""))
      }else{
        #Crop and reproject the old fashioned way
        output <- projectInputs(x = subFile[[1]], targetCRS = crs(subROI[[1]]))
        output <- cropInputs(x = output, subROI[[1]], filename = paste(dir, "/", names(subFile), "_", names(subROI), sep = ""))
      }
        # output@bbox <- raster::as.matrix(extent(subROI[[1]]))
      if (is.null(output)) {
          cat(crayon::bgGreen$bold("no ", names(subFile), " in ", names(subROI), sep = ""))#class is null. It will be removed later  
      }
      return(output)
    })
    
    #run postProcess sim$dataListInit by region (postProcess clips, masks, and reprojects)
    names(subList) <- names(sim$dataListInit)
    subList <- Filter(Negate(is.null), subList)
    
    return(subList)
    })
  
  names(sim$dataList) <- names(sim$ROI)
  
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  
  if(!suppliedElsewhere("ROI", sim)) {
  sim$ROI <- data.frame(Region = c("SouthwestBC", "VancouverIsland", "LowerMainland"),
                        xmn = c(-2118298, -2040398, -1942000),
                        xmx = c(-1770148, -1948121, -1873500),
                        ymn = c(1255505, 1312569, 1359500),
                        ymx = c(1546415, 1434588, 1420300))
  
  }
  
  return(invisible(sim))
}