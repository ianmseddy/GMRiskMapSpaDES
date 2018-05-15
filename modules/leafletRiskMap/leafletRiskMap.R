
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "leafletRiskMap",
  description = "Module creates a leaflet risk map", #"insert module description here",
  keywords = c("leaflet", "risk map"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", leafletRiskMap = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "leafletRiskMap.Rmd"),
  reqdPkgs = list("leaflet", "raster", "sp", "curl", "htmlwidgets", "pander"), #curl package to check internet connection
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("fileName", "character", "leafletMap", NA, NA, "File name the html map will be saved as in 'outputs' directory"),
    defineParameter("basemap", "character", "satellite", NA, NA, "Basemap of leaflet risk map. Options: satellite, roadmap, hybrid, terrain"),
    defineParameter("mapRisk", "logical", FALSE, NA, NA, "Logical of whether to map totalRisk on leaflet map"),
    defineParameter("mapHiRisk", "logical", TRUE, NA, NA, "Logical of whether to map hiRisk on leaflet map"),
    defineParameter("dataLayers", "List", NA, NA, NA, "List of names, naming layers in dataList to be added to map"),
    defineParameter("riskLayers", "List", NA, NA, NA, "List of names, naming layers in riskList to be added to map"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS"),
    expectsInput(objectName = "totalRisk", objectClass = "RasterLayer", desc = "Raster of totalRisk"),
    expectsInput(objectName = "dataList", objectClass = "List", desc = "List of data rasters", sourceURL = NA),
    expectsInput(objectName = "riskList", objectClass = "List", desc = "List of risk rasters", sourceURL = NA), 
    expectsInput(objectName = "highRisk", objectClass = "RasterLayer", desc = "Raster of high risk areas.")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "leafletMap", objectClass = "leaflet", desc = "Leaflet map of region of interest")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.leafletRiskMap = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- leafletRiskMapInit(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "leafletRiskMap", "checkinputs", .last())
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "leafletRiskMap", "save")
    },
    checkinputs = {
      # schedule future event(s)
      ## checking if inputs exist yet - all possible combinations of dataLayers, riskLayers & totalRisk/HiRisk
      if(!is.na(P(sim)$dataLayers) && !is.null(sim$dataList) &&  # all 3
         !is.na(P(sim)$riskLayers) && !is.null(sim$riskList) && 
         (P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE) && !is.null(sim$totalRisk)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if(!is.na(P(sim)$dataLayers) && !is.null(sim$dataList) &&  # 1 - dataLayers
                is.na(P(sim)$riskLayers) && (P(sim)$mapRisk == FALSE && P(sim)$mapHiRisk == FALSE)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if( !is.na(P(sim)$riskLayers) && !is.null(sim$riskList) &&  # 1 - riskLayers
                 is.na(P(sim)$dataLayers) && (P(sim)$mapRisk == FALSE && P(sim)$mapHiRisk == FALSE)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if((P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE) &&  # 1 - totalRisk/HiRisk
                !is.null(sim$totalRisk) && is.na(P(sim)$dataLayers) && is.na(P(sim)$riskLayers)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if(!is.na(P(sim)$dataLayers) && !is.null(sim$dataList) &&  # 2 - dataLayers & riskLayers
                !is.na(P(sim)$riskLayers) && !is.null(sim$riskList) && 
                (P(sim)$mapRisk == FALSE && P(sim)$mapHiRisk == FALSE)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if(!is.na(P(sim)$dataLayers) && !is.null(sim$dataList) # 2 - dataLayers & totalRisk/HiRisk
                && is.na(P(sim)$riskLayers) && 
                (P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE) && !is.null(sim$totalRisk)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if(!is.na(P(sim)$riskLayers) && !is.null(sim$riskList) # 2 - riskLayers & totalRisk/HiRisk
                && is.na(P(sim)$dataLayers) 
                && (P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE) && !is.null(sim$totalRisk)) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else { sim <- scheduleEvent(sim, time(sim)+0.1, "leafletRiskMap", "checkinputs", .last()) } # not all inputs available yet
      
      
    },
    basemap = {
      # do stuff for this event
      sim <- leafletRiskMapBasemap(sim)
      
      # schedule future event(s)
      if(P(sim)$mapRisk == TRUE) sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "maptotalrisk")
      if(P(sim)$mapHiRisk == TRUE) sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "maphirisk")
      if(!is.na(P(sim)$dataLayers)) sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "mapdata")
      if(!is.na(P(sim)$riskLayers)) sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "maprisk")  
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "leafletRiskMap", "plot", .last())
      # sim <- scheduleEvent(sim, time(sim) + increment, "leafletRiskMap", "basemap")
    },
    maptotalrisk = {
      # do stuff for this event
      sim <- leafletRiskMapLeafTotalRisk(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "leafletRiskMap", "maptotalrisk")
    },
    maphirisk = {
      # do stuff for this event
      sim <- leafletRiskMapLeafHiRisk(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "leafletRiskMap", "maphirisk")
    },
    mapdata = {
      # do stuff for this event
      sim <- leafletRiskMapLeafData(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "leafletRiskMap", "mapdata")
    },
    maprisk = {
      # do stuff for this event
      sim <- leafletRiskMapLeafRisk(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "leafletRiskMap", "maprisk")
    },
    plot = {
      # do stuff for this event
      sim <- leafletRiskMapPlot(sim)

      # schedule future event(s)
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "leafletRiskMap", "plot")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
leafletRiskMapInit <- function(sim) {
  
  sim$leafletMap <- leaflet::leaflet() # create leaflet map template
  
  return(invisible(sim))
}

### plot event:
leafletRiskMapPlot <- function(sim) {
  
    # save leafletMap as html file to outputs folder
  saveWidget(sim$leafletMap, file=file.path(outputPath(sim),paste0(P(sim)$fileName, ".html")), selfcontained=FALSE)
    # open saved leafletMap in browser window
  pander::openFileInOS(file.path(outputPath(sim),paste0(P(sim)$fileName, ".html")))

  return(invisible(sim))
}


### basemap event:
leafletRiskMapBasemap <- function(sim) {
  
  ext <- raster::extent(raster::projectExtent(sim$ROI, crs="+proj=longlat +datum=WGS84"))
  sim$leafletMap <- leaflet::fitBounds(sim$leafletMap, lng1=ext[1], lat1=ext[3], lng2=ext[2], lat2=ext[4])
  
  #if(curl::has_internet() == FALSE) {
  #  message("leafletRiskMap: No internet connection. Basemap not loaded to leafletMap.")
  #}
  
  switch(P(sim)$basemap,
         satellite = { sim$leafletMap <- leaflet::addProviderTiles(sim$leafletMap, "Esri.WorldImagery") },
         roadmap = {  sim$leafletMap <- leaflet::addProviderTiles(sim$leafletMap, "Esri.WorldStreetMap") },
         hybrid = { sim$leafletMap <- leaflet::addProviderTiles(sim$leafletMap, "Esri.WorldImagery")
         sim$leafletMap <- leaflet::addProviderTiles(sim$leafletMap, "Esri.WorldTopoMap", options = providerTileOptions(opacity = 0.75)) },
         terrain = { sim$leafletMap <- leaflet::addProviderTiles(sim$leafletMap, "Stamen.Terrain") })
  
  return(invisible(sim))
}

### maptotalrisk event:
leafletRiskMapLeafTotalRisk <- function(sim) {
  
  totalRiskLeaflet <- leaflet::projectRasterForLeaflet(sim$totalRisk, method = "bilinear")
  totalRiskLeaflet[totalRiskLeaflet<=0] <- NA
  if("water" %in% names(sim$dataList)) {
    waterLeaflet <- leaflet::projectRasterForLeaflet(sim$dataList$water, method = "bilinear")
    totalRiskLeaflet <- raster::mask(totalRiskLeaflet, waterLeaflet, inverse=TRUE)
  }
  
  sim$leafletMap <- leaflet::addRasterImage(sim$leafletMap, totalRiskLeaflet, colors=rev(heat.colors(16)), opacity=0.35, project=FALSE)
  
  return(invisible(sim))
}

### maphirisk event:
leafletRiskMapLeafHiRisk <- function(sim) {
 
  highRiskLeaflet <- leaflet::projectRasterForLeaflet(sim$highRisk, method = "bilinear")
  highRiskLeaflet[highRiskLeaflet<=0] <- NA
  if("water" %in% names(sim$dataList)) {
    waterLeaflet <- leaflet::projectRasterForLeaflet(sim$dataList$water, method = "bilinear")
    highRiskLeaflet <- raster::mask(highRiskLeaflet, waterLeaflet, inverse=TRUE)
  }
  sim$leafletMap <- leaflet::addRasterImage(sim$leafletMap, highRiskLeaflet,
                                            colors = "Spectral", opacity = 0.3,project=FALSE)
  
  return(invisible(sim))
}


### maprisk event:
leafletRiskMapLeafRisk  <- function(sim) {

  for(i in 1:length(P(sim)$riskLayers)) {
    temp <- leaflet::projectRasterForLeaflet(sim$riskList[[P(sim)$riskLayers[[i]]]], method = "bilinear")
    if("water" %in% names(sim$dataList)) {
      waterLeaflet <- leaflet::projectRasterForLeaflet(sim$dataList$water, method = "bilinear")
      temp <- raster::mask(temp, waterLeaflet, inverse=TRUE)
    }
    sim$leafletMap <- leaflet::addRasterImage(sim$leafletMap, temp, 
                                              colors=rev(heat.colors(16)), opacity=0.35, project=FALSE)
    
  }  
  return(invisible(sim))
}

install.packages("fastshp")
### mapdata event:
leafletRiskMapLeafData <- function(sim) {
  
  for(i in 1:length(P(sim)$dataLayers)) {
    
    if( is(sim$dataList[[P(sim)$dataLayers[[i]]]], "Spatial") ) {
      
      temp <- sp::spTransform(sim$dataList[[P(sim)$dataLayers[[i]]]], CRSobj=sp::CRS("+proj=longlat +datum=WGS84"))
      if( P(sim)$dataLayers[[i]] %in% names(temp) ) { #if name of data layer matches column in dataframe
        temp <- temp[,c(which(names(temp)==P(sim)$dataLayers[[i]]),which(names(temp)!=P(sim)$dataLayers[[i]]))]
        popup <- c()
        for(k in 1:length(temp)) {
          pop <- c()
          for(j in 1:length(names(temp))) {
            x <- paste0(names(temp)[[j]], ": ",temp[[j]][[k]])
            pop <- paste(pop,x, sep="<BR>")
          }
          popup <- c(popup, pop)
        }
        temp$popup <- popup  
        temp$col <- as.factor(temp[[P(sim)$dataLayers[[i]]]])
        temp$colour <- temp$col
      } else {  #if name doesn't match, use first column in dataframe
        popup <- c()
        for(k in 1:length(temp)) {
          pop <- paste0(P(sim)$dataLayers[[i]])
          for(j in 1:length(names(temp))) {
            x <- paste0(names(temp)[[j]], ": ",temp[[j]][[k]])
            pop <- paste(pop,x, sep="<BR>")
          }
          popup <- c(popup, pop)
        }
        temp$popup <- popup  
        temp$col <- as.factor(temp[[1]])
        temp$colour <- temp$col
      }
      levels(temp$colour) <- c(levels(temp$col), rainbow(n = length(levels(temp$col)), start = 0, end = 3/6))
      for( j in 1:length(levels(temp$col)) ) {
        temp$colour[temp$colour == levels(temp$col)[j]] <- rev(rainbow(n = length(levels(temp$col)), start = 0, end = 3/6))[j]
      }
      temp$col <- NULL
      
      dataClass <- class(sim$dataList[[P(sim)$dataLayers[[i]]]])
      switch(
        dataClass,
        SpatialPointsDataFrame = { sim <- leafletSpatialPoints(sim, temp) },
        SpatialPolygonsDataFrame = { sim <- leafletSpatialPolygons(sim, temp) },
        SpatialLinesDataFrame = { sim <- leafletSpatialLines(sim, temp) },
        warning(paste0("leafletRiskMap: '", P(sim)$dataLayers[[i]], "' in dataLayers not added to leafletMap. Spatial class '", 
                       class(sim$dataList[[P(sim)$dataLayers[[i]]]]), "' not defined in mapdata event."))
      )
      
    } else if( is(sim$dataList[[P(sim)$dataLayers[[i]]]], "Raster") ) {
      
      temp <- leaflet::projectRasterForLeaflet(sim$dataList[[P(sim)$dataLayers[[i]]]], method = "bilinear")
      sim <- leafletRaster(sim, temp)
      
    } else {
      warning(paste0("leafletRiskMap: '", P(sim)$dataLayers[[i]], "' in dataLayers not added to leafletMap. Class '", 
                     class(sim$dataList[[P(sim)$dataLayers[[i]]]]), "' not defined in mapdata event."))
    }
  }
  return(invisible(sim))
}

## additional functions: used in leafletRiskMapLeafData function

# adding SpatialPoints data to leaflet map
leafletSpatialPoints <- function(sim, temp) {
  
  sim$leafletMap <- leaflet::addCircles(sim$leafletMap, lng = sp::coordinates(temp)[,1], lat = sp::coordinates(temp)[,2], 
                                        opacity = 1, popup = temp$popup, color = temp$colour)
  return(sim)
}

# adding SpatialPolygons data to leaflet map
leafletSpatialPolygons <- function(sim, temp) {
  
  for(k in 1:length(temp@polygons)) {
    sim$leafletMap <- leaflet::addPolygons(sim$leafletMap, weight=2,
                                           lng = c(temp@polygons[[k]]@Polygons[[1]]@coords[,1]),
                                           lat = c(temp@polygons[[k]]@Polygons[[1]]@coords[,2]),
                                           opacity = 1, popup = temp$popup[[k]], color = temp$colour[[k]])
  } 
  return(sim)
}

# adding SpatialLines data to leaflet map
leafletSpatialLines <- function(sim, temp) {
  
  for(k in 1:length(temp@lines)) {
    sim$leafletMap <- leaflet::addPolylines(sim$leafletMap, weight=2,
                                            lng = c(temp@lines[[k]]@Lines[[1]]@coords[,1]),
                                            lat = c(temp@lines[[k]]@Lines[[1]]@coords[,2]),
                                            opacity = 1, popup = temp$popup[[k]], color = temp$colour[[k]])
  } 
  return(sim)
}

# adding raster data to leaflet map
leafletRaster <- function(sim, temp) {
  if("water" %in% names(sim$dataList)) {
    waterLeaflet <- leaflet::projectRasterForLeaflet(sim$dataList$water, method = "bilinear")
    temp <- raster::mask(temp, waterLeaflet, inverse=TRUE)
  }
  sim$leafletMap <- leaflet::addRasterImage(sim$leafletMap, temp, colors=rev(topo.colors(16)), opacity=0.35, project=FALSE)
  return(sim)
}


