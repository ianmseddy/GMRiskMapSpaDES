
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "loadPortLocations",
  description = "Module that imports port locations and data and applies basic formatting", 
  keywords = c("port"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", loadPortLocations = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "loadPortLocations.Rmd"),
  reqdPkgs = list("raster","rgdal"),
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
    expectsInput(objectName = "ports", objectClass = "SpatialPointsDataFrame", desc = "Raw port location data"),
    expectsInput(objectName = "dataListInit", objectClass = "List", desc = "List of data rasters. If NULL, recreates an empty list in .inputsObjects", sourceURL = NA)
    ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataListInit", objectClass = "List", desc = "List of data containing 'ports' dataset")
  )
))

doEvent.loadPortLocations = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- loadPortLocationsInit(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "loadPortLocations", "format")
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "loadPortLocations", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "loadPortLocations", "save")
    },
    plot = {
      # do stuff for this event
      loadPortLocationsPlot(sim) 
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "loadPortLocations", "plot")
      
    },
    save = {
      # do stuff for this event
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "loadPortLocations", "save")
      
    },
    format = {
      # do stuff for this event
      sim <- loadPortLocationsFormat(sim)
      
      # schedule future event(s)
      #sim <- scheduleEvent(sim, time(sim) + increment, "loadPortLocations", "templateEvent")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
loadPortLocationsInit <- function(sim) {
  
  return(invisible(sim))
}


### plot event:
loadPortLocationsPlot <- function(sim) {
  
  Plot(sim$dataListInit$ports, title = "Port Locations")

  return(invisible(sim))
}

### format event: formatting ports dataset
loadPortLocationsFormat <- function(sim) {
  
  temp <- sim$dataListInit[["ports"]]
  temp@coords <- temp@coords[,c(1:2)]
  temp$ports <- 1 
  temp$ID <- 1:length(temp)
  temp <- sp::spTransform(temp, "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  sim$dataListInit[["ports"]] <- temp
  
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  
  # create sim$dataListInit if it doesn't exist
  if(is.null(sim$dataListInit)){
    sim$dataListInit <- list()
  }
  
  # import port locations data
  if("lowerMainPorts.kml" %in% list.files(file.path(modulePath(sim),"loadPortLocations","data"))) {
    #ogrListLayers(file.path(modulePath(sim),"loadPortLocations","data","lowerMainPorts.kml")) # to list layers within .kml file
    sim$dataListInit[["ports"]] <- rgdal::readOGR(dsn=file.path(modulePath(sim),"loadPortLocations","data","lowerMainPorts.kml"),layer = "lowerMainPorts")
  } else {
    stop("loadPortLocations: There is no port location dataset provided") }
  
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
