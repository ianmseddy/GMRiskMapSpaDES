
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "selectROI",
  description = "Module for setting region of interest (roi).", 
  keywords = c("extent", "SpatialPolygons"), 
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", selectROI = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "selectROI.Rmd"),
  reqdPkgs = list("raster", "sp", "sf", "mapedit", "leaflet.extras", "dismo", "ggmap", "curl"),
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
    expectsInput(objectName = "ROI", objectClass = "data.frame", 
                 desc = "data.frame defining regions of interest and their extents. Headers should include name, ymin, ymax, xmin, and xmam", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (roi)")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.selectROI = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- selectROIInit(sim)

    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


## Event Functions ##

### init event:
selectROIInit <- function(sim) {
  #This will make input Object into polygons

  temp <- vector(mode = "list", length = nrow(sim$ROI))
  for (i in 1:nrow(sim$ROI)) {
    temp[i] <- as(sim$ROI[i], "SpatialPolygons")
    sp::Spatial(bbox = sim$roi[i, 2:4]) #Why is this 2:4 again?
  }
  
  
  return(invisible(sim))
}

### map event:

