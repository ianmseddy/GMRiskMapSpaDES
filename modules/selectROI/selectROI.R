
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
    expectsInput(objectName = "roi", objectClass = c("SpatialPolygons","character", NULL), 
                 desc = "Polygon defining region of interest (roi). Or character string matching a predefined roi. Or NULL to interactively select roi on map.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "roi", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (roi)")
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
      
      # schedule future event(s)
      if( is(sim$roi, "Spatial") ) {   # if roi is a SpatialPolygons, do nothing
      } else if( is.character(sim$roi) && sim$roi %in% c("Canada", "BC", "SouthwestBC", "VancouverIsland", "LowerMainland") ) { # if roi is a predefined ROI, schedule "preset" event
        sim <- scheduleEvent(sim, start(sim), "selectROI", "preset")
      } else if(is.character(sim$roi)) { # if roi is an undefined character string - user option of using "preset" or "map"
        message(paste0("'",sim$roi,"' not an accepted predefined roi."))
        message("Predefined  ROIs include: 'Canada', BC', 'SouthwestBC', 'VancouverIsland', 'LowerMainland'.")
        ans <- question("Type desired predefined roi in console OR type 'map' to interactively select roi from map." ) 
        if (ans %in% c("Canada", "BC", "SouthwestBC", "VancouverIsland", "LowerMainland")) {
          sim$roi <- ans
          sim <- scheduleEvent(sim, start(sim), "selectROI", "preset")
        } else if(ans == "map") {
          sim <- scheduleEvent(sim, start(sim), "selectROI", "map")
        } else { stop("selectROI: No valid roi option selected.") }  # if none of the above, no roi and simulation fails.
      } else if(is.null(sim$roi)) {   # if roi is empty, schedule "map" event
        sim <- scheduleEvent(sim, start(sim), "selectROI", "map")
      } 
      
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "selectROI", "plot")
    },
    plot = {
      # do stuff for this event
      selectROIPlot(sim)
      
      # schedule future event(s)
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "selectROI", "plot")
    },
    map = {
      # do stuff for this event
      sim <- selectROIMap(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "selectROI", "map")
    },
    preset = {
      # do stuff for this event
      sim <- selectROIPreset(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "selectROI", "preset")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


## Event Functions ##

### init event:
selectROIInit <- function(sim) {
  
  return(invisible(sim))
}


### plot event:
selectROIPlot <- function(sim) {
  
  if(curl::has_internet() == FALSE) {
    message("selectROI: No internet connection. Cannot plot selected roi.")
  } else {
    
    # plotting roi using Google Map image - crs may differ than sim$crs
    roiGoogle <- sp::spTransform(sim$roi, raster::crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
    zoomLevel <- ggmap::calc_zoom(c(xmin(roiGoogle), xmax(roiGoogle)), c(ymin(roiGoogle), ymax(roiGoogle)))
    roiGoogleMap <- dismo::gmap(x = extent(roiGoogle), type = "satellite", lonlat=TRUE, zoom=zoomLevel-1)
    Plot(roiGoogleMap, legend = FALSE, title="Region of Interest")
    Plot(roiGoogle, addTo="roiGoogleMap", cols="white")
    
  }
  
  return(invisible(sim))
}

### preset event:
selectROIPreset <- function(sim) {
  
  ROIDefined <- data.frame(roi    =        c( "xmn",     "xmx",    "ymn",     "ymx"),
                           Canada =        c(-2317000,  3092154,  298781.1,  4811413),
                           BC     =        c(-2350000, -1276830,  1135380,   3002980),
                           SouthwestBC =   c(-2118298, -1770148,  1255505,   1546415),
                           VancouverIsland=c(-2040398, -1948121,  1312569,   1434588),
                           LowerMainland = c(-1942000, -1873500,  1359500,   1420300))
  
  if (sim$roi %in% names(ROIDefined)) { 
    sim$roi <- raster::extent(ROIDefined[,match(sim$roi, names(ROIDefined))])
    sim$roi <- as(sim$roi, "SpatialPolygons")
    crs(sim$roi) <- "+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
  } else {
    message(paste0("Message: '", sim$roi, "' not an accepted predefined roi."))
  }
  
  return(invisible(sim))
}

### map event:
selectROIMap <- function(sim) {
  
  message("Select roi in map viewer. Press 'DONE' when complete.")
  
  tempROI <- editMap(leaflet() %>% setView(-120, 50.5, zoom = 6)  %>% addTiles())
  roi <- cbind(st_coordinates(tempROI$drawn$geometry)[,1], st_coordinates(tempROI$drawn$geometry)[,2]) %>%
    Polygon() %>%
    list() %>%
    Polygons("s1") %>%
    list() %>%
    SpatialPolygons(1L)
  crs(roi) <- st_crs(tempROI$drawn$geometry)[[2]]
  sim$roi <- as(roi, "SpatialPolygons")
  
  return(invisible(sim))
}



## extra functions
question <- function(x) {
  writeLines(x)
  out <- scan(what="character", nmax=1, quiet=T)
  out
}
