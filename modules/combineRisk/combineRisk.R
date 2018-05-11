
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "combineRisk",
  description = "Module combines risk rasters in riskList",
  keywords = c("risk"), 
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", combineRisk = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "combineRisk.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "riskList", objectClass = "List", desc = "List containing 'risk' rasters", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "totalRisk", objectClass = "RasterLayer", desc = "Raster of totalRisk created by combining layers in riskList")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.combineRisk = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- combineRiskInit(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "combineRisk", "checkinputs")
    },
    plot = {
      # do stuff for this event
      combineRiskPlot(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "combineRisk", "plot")
    },
    checkinputs = {
      
      # schedule future event(s)
      if( is.null(sim$riskList) ) { 
        sim <- scheduleEvent(sim, time(sim) + 0.1, "combineRisk", "checkinputs", .last())
      } else { 
        sim <- scheduleEvent(sim, time(sim), "combineRisk", "combine") 
      }
      
    },
    combine = {
      # do stuff for this event
      sim <- combineRiskCombine(sim)
      
      # schedule future event(s)
         # schedule first 'plot' event
      if(is.na(P(sim)$.plotInitialTime)) {
        # if .plotInitialTime=NA, don't schedule plot event
      } else if( "plot" %in% subset(completed(sim), completed(sim)$moduleName=="combineRisk")$eventType == FALSE) {
        if( time(sim) >= P(sim)$.plotInitialTime) {
          sim <- scheduleEvent(sim, time(sim), "combineRisk", "plot")
        } else { 
          sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "combineRisk", "plot")
        }
      }
      # sim <- scheduleEvent(sim, time(sim) + increment, "combineRisk", "combine")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
combineRiskInit <- function(sim) {
  
  return(invisible(sim))
}


### plot event:
combineRiskPlot <- function(sim) {
  
  Plot(sim$totalRisk, title = names(sim$totalRisk), cols=rev(heat.colors(16))) #legendRange = 0:1
  if("water" %in% names(sim$dataList)) { #if water is a layer in dataList, add to risk maps
    Plot(sim$dataList[["water"]], addTo="sim$totalRisk")
  }
  
  return(invisible(sim))
}

### combine event: combining riskList rasters
combineRiskCombine <- function(sim) {
  
  
  if(length(sim$riskList) == 1) { #if only one risk layer, then it is totalRisk
    totalRisk <- sim$riskList[[1]]
  } else {  # if more than one layer, add risk layers together
    riskStack <- raster::stack(sim$riskList)
    totalRisk <- sum(riskStack, na.rm=TRUE) } 
  totalRisk[totalRisk<0] <- 0 # if risk values below 0, set to 0
  #totalRisk <- totalRisk/max(raster::values(totalRisk), na.rm=TRUE) # scale values to 1 using max risk value
  names(totalRisk) <- c("totalRisk")
  sim$totalRisk <- totalRisk
  
  return(invisible(sim))
}


#.inputObjects <- function(sim) {

####### Create data if sim$riskList not provided
#  
#  if(is.null(sim$riskList)) {
#    sim$riskList <- list()
#    # example risk 1
#    example1Risk <- raster::raster(ext=raster::extent(-1936248,-1910000,1406079,1415135), 
#                                   resolution=30, 
#                                   crs=raster::crs("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
#    points <- SpaDES.tools::initiateAgents(example1Risk, 5) 
#    example1Risk <- raster::distanceFromPoints(example1Risk, points) 
#    values(example1Risk) <- abs((values(example1Risk) - max(values(example1Risk))))/max(values(example1Risk))
#    names(example1Risk) <- "example1Risk"
#    sim$riskList[["example1Risk"]] <- example1Risk
#    
#    #example risk 2
#    points <- SpaDES.tools::initiateAgents(example1Risk, 10) 
#    example2Risk <- raster::distanceFromPoints(example1Risk, points) 
#    values(example2Risk) <- abs((values(example2Risk) - max(values(example2Risk))))/max(values(example2Risk))
#    names(example2Risk) <- "example2Risk"
#    sim$riskList[["example2Risk"]] <- example2Risk
#   
#  }

#  return(invisible(sim))
#}


