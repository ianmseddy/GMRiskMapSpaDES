
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
  reqdPkgs = list("leaflet", "raster", "sp", "curl", "htmlwidgets", "pander", "fastshp"), #curl package to check internet connection
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("fileName", "character", "leafletMap", NA, NA, "File name(s) for output html map(s). Will overwrite. 
                    Length should equal length of ROIs"),
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
    expectsInput(objectName = "ROI", objectClass = "list", desc = "list of rasters defining region(s) of interest (ROI)"),
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
      hasData <- !is.null(sim$dataList)
      hasRisk <- !is.null(sim$riskList) || !is.null(sim$totalRisk)
      needsData <- !is.na(P(sim)$dataLayers)
      needsRisk <- P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE
      #sim will always have data and risk layers if it has totalRisk
      
      if (needsData && hasData && needsRisk && hasRisk) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else if (!needsData && !needsRisk) {
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap")
      } else if (needsData && hasData && !needsRisk) { #doesn't need risk
        sim <- scheduleEvent(sim, time(sim), "leafletRiskMap", "basemap") 
      } else { sim <- scheduleEvent(sim, time(sim)+0.1, "leafletRiskMap", "checkinputs", .last()) } # inputs not all available
    
      
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
  
  output <- lapply(1:length(sim$ROI), FUN = function(i) {
    x <- leaflet::leaflet() # create leaflet map template
    return(x)
  })
  names(output) <- names(sim$ROI)
  sim$leafletMap <- output
  return(invisible(sim))
}

### plot event:
leafletRiskMapPlot <- function(sim) {
  if (length(sim$leafletMap != length(P(sim)$fileName))) {
    stop("Number of leaflet file names do not match number of ROIs")
  }

  for (i in 1:length(sim$leafletMap)) {
    # save leafletMap as html file to outputs folder
    
    saveWidget(sim$leafletMap[[i]], file = file.path(outputPath(sim),paste0(P(sim)$fileName[i], ".html")), selfcontained=FALSE)
    
  # open saved leafletMap in browser window
  pander::openFileInOS(file.path(outputPath(sim),paste0(P(sim)$fileName[i], ".html")))
  }
  return(invisible(sim))
}


### basemap event:
leafletRiskMapBasemap <- function(sim) {

  output <- Cache(lapply, 1:length(sim$ROI), function(i, 
                                         leafletMap = sim$leafletMap, 
                                         ROI = sim$ROI,
                                         basemap = P(sim)$basemap){
    #subset files by ROI
    ROI = ROI[[i]]
    leafletMap <- leafletMap[[i]]
    
    ext <- raster::extent(raster::projectExtent(ROI, crs="+proj=longlat +datum=WGS84"))
    leafletMap <- leaflet::fitBounds(leafletMap, lng1=ext[1], lat1=ext[3], lng2=ext[2], lat2=ext[4])
    
    #if(curl::has_internet() == FALSE) {
    #  message("leafletRiskMap: No internet connection. Basemap not loaded to leafletMap.")
    #}
    
    switch(basemap,
           satellite = { leafletMap <- leaflet::addProviderTiles(leafletMap, "Esri.WorldImagery") },
           roadmap = {  leafletMap <- leaflet::addProviderTiles(leafletMap, "Esri.WorldStreetMap") },
           hybrid = { leafletMap <- leaflet::addProviderTiles(leafletMap, "Esri.WorldImagery")
           leafletMap <- leaflet::addProviderTiles(leafletMap, "Esri.WorldTopoMap", 
                                                       options = providerTileOptions(opacity = 0.75)) },
           terrain = { leafletMap <- leaflet::addProviderTiles(leafletMap, "Stamen.Terrain") })
    return(leafletMap)
  })
  
  names(output) <- paste(names(sim$ROI), "_leafletMap")
  sim$leafletMap <- output
  
  return(invisible(sim))
}

### maptotalrisk event:
leafletRiskMapLeafTotalRisk <- function(sim) {
  
  
  output <- Cache(lapply, 1:length(sim$ROI), function(i, 
                                                     totalRisk = sim$totalRisk,
                                                     dataList = sim$dataList,
                                                     leafletMap = sim$leafletMap){
    #subset all Input files by ROI
    totalRisk <- totalRisk[[i]]
    dataList <- dataList[[i]]
    leafletMap <- leafletMap[[i]]
    #map total Risk
    totalRiskLeaflet <- leaflet::projectRasterForLeaflet(totalRisk, method = "bilinear")
    totalRiskLeaflet[totalRiskLeaflet<=0] <- NA
    if("water" %in% names(dataList)) {
      waterLeaflet <- leaflet::projectRasterForLeaflet(dataList$water, method = "bilinear")
      totalRiskLeaflet <- raster::mask(totalRiskLeaflet, waterLeaflet, inverse=TRUE)
    }
    
    leafletMap <- leaflet::addRasterImage(leafletMap, totalRiskLeaflet, 
                                              colors=rev(heat.colors(16)), opacity=0.35, project=FALSE)
    return(leafletMap)  
  })
  
  names(output) <- paste(names(sim$ROI), "_leafletMap")
  sim$leafletMap <- output
  
  return(invisible(sim))
}

### maphirisk event:
leafletRiskMapLeafHiRisk <- function(sim) {
  
  output <- Cache(lapply, 1:length(sim$ROI), function(i,
                                                     leafletMap = sim$leafletMap, 
                                                     dataList = sim$dataList,
                                                     highRisk = sim$highRisk) {
    #Subset all input files by ROI
    leafletMap = leafletMap[[i]]
    dataList = dataList[[i]]
    highRisk = highRisk[[i]]
    #Map high Risk
    highRiskLeaflet <- leaflet::projectRasterForLeaflet(highRisk, method = "bilinear")
    highRiskLeaflet[highRiskLeaflet<=0] <- NA
    if("water" %in% names(dataList)) {
      waterLeaflet <- leaflet::projectRasterForLeaflet(dataList$water, method = "bilinear")
      highRiskLeaflet <- raster::mask(highRiskLeaflet, waterLeaflet, inverse=TRUE)
    }
    leafletMap <- leaflet::addRasterImage(leafletMap, highRiskLeaflet,
                                              colors = "Spectral", opacity = 0.3,project=FALSE)
    return(leafletMap)
  })
  
  names(output) <- paste(names(sim$ROI), "_leafletMap")
  sim$leafletMap <- output
  
  return(invisible(sim))
}


### maprisk event:
leafletRiskMapLeafRisk  <- function(sim) {
  
  output <- lapply(1:length(sim$ROI), FUN = function(i, 
                                                     riskLayers = P(sim)$riskLayers,
                                                     dataList = sim$dataList,
                                                     leafletMap = sim$leafletMap,
                                                     riskList = sim$riskList) {
    
    #subset all input files
    riskLayers = riskLayers
    dataList = dataList[[i]]
    leafletMap = leafletMap[[i]]
    riskList = riskList[[i]]
    #Map risk for each ROI
    for(ii in 1:length(riskLayers)) {
      temp <- leaflet::projectRasterForLeaflet(riskList[[riskLayers[[ii]]]], method = "bilinear")
      if("water" %in% names(dataList)) {
        waterLeaflet <- leaflet::projectRasterForLeaflet(dataList$water, method = "bilinear")
        temp <- raster::mask(temp, waterLeaflet, inverse=TRUE)
      }
      leafletMap <- leaflet::addRasterImage(leafletMap, temp, 
                                                colors=rev(heat.colors(16)), opacity=0.35, project=FALSE)
    }
  return(leafletMap)
  })
  
  names(output) <- paste(names(sim$ROI), "_leafletMap")
  sim$leafletMap <- output
  
  return(invisible(sim))
}


### mapdata event:
leafletRiskMapLeafData <- function(sim) {
  #################
  for (i in 1:length(sim$dataList)) {  
    dataList <- sim$dataList[[i]]
    
    for(ii in 1:length(P(sim)$dataLayers)) {
      
      if( is(dataList[[P(sim)$dataLayers[[ii]]]], "Spatial") ) {
        
        temp <- sp::spTransform(dataList[[P(sim)$dataLayers[[ii]]]], CRSobj=sp::CRS("+proj=longlat +datum=WGS84"))
        
        if( P(sim)$dataLayers[[ii]] %in% names(temp) ) { #if name of data layer matches column in dataframe
          temp <- temp[,c(which(names(temp)==P(sim)$dataLayers[[ii]]),which(names(temp)!=P(sim)$dataLayers[[ii]]))]
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
          
          temp$col <- as.factor(temp[[P(sim)$dataLayers[[ii]]]])
          temp$colour <- temp$col
        }
       
        levels(temp$colour) <- c(levels(temp$col), rainbow(n = length(levels(temp$col)), start = 0, end = 3/6))
        for( j in 1:length(levels(temp$col)) ) {
          temp$colour[temp$colour == levels(temp$col)[j]] <- rev(rainbow(n = length(levels(temp$col)), start = 0, end = 3/6))[j]
        }
        temp$col <- NULL
        
        dataClass <- class(dataList[[P(sim)$dataLayers[[ii]]]])
        switch(
          dataClass,
          SpatialPointsDataFrame = { sim$leafletMap[[i]] <- leafletSpatialPoints(inList = sim$leafletMap[[i]], temp) },
          SpatialPolygonsDataFrame = { sim$leafletMap[[i]] <- leafletSpatialPolygons(inList = sim$leafletMap[[i]], temp) },
          SpatialLinesDataFrame = { sim$leafletMap[[i]] <- leafletSpatialLines(inList = sim$leafletMap[[i]], temp) },
          warning(paste0("leafletRiskMap: '", 
                         P(sim)$dataLayers[[ii]], 
                         "' in dataLayers not added to leafletMap. Spatial class '", 
                         class(dataList[[P(sim)$dataLayers[[ii]]]]), 
                         "' not defined in mapdata event."))
        )
        
      } else if( is(dataList[[P(sim)$dataLayers[[ii]]]], "Raster") ) {
        
        temp <- leaflet::projectRasterForLeaflet(dataList[[P(sim)$dataLayers[[ii]]]], method = "bilinear")
        sim$leafletMap[[i]] <- leafletRaster(inList = sim$leafletMap[[i]], temp, dataList = sim$dataList[[i]])
        
      } else {
        warning(paste0("leafletRiskMap: '", P(sim)$dataLayers[[ii]], 
                       "' in dataLayers not added to leafletMap. Class '", 
                       class(dataList[[P(sim)$dataLayers[[ii]]]]),
                       "' not defined in mapdata event."))
      }
    }
  }
  return(invisible(sim))
}

## additional functions: used in leafletRiskMapLeafData function

# adding SpatialPoints data to leaflet map
leafletSpatialPoints <- function(inList, temp) {
  
  inList <- leaflet::addCircles(inList, lng = sp::coordinates(temp)[,1], lat = sp::coordinates(temp)[,2], 
                                        opacity = 1, popup = temp$popup, color = temp$colour)
  return(inList)
}

# adding SpatialPolygons data to leaflet map
leafletSpatialPolygons <- function(inList, temp) {
  
  for(k in 1:length(temp@polygons)) {
    inList <- leaflet::addPolygons(inList, weight=2,
                                           lng = c(temp@polygons[[k]]@Polygons[[1]]@coords[,1]),
                                           lat = c(temp@polygons[[k]]@Polygons[[1]]@coords[,2]),
                                           opacity = 1, popup = temp$popup[[k]], color = temp$colour[[k]])
  } 
  return(inList)
}

# adding SpatialLines data to leaflet map
leafletSpatialLines <- function(inList, temp) {
  
  for(k in 1:length(temp@lines)) {
    inList <- leaflet::addPolylines(inList, weight=2,
                                            lng = c(temp@lines[[k]]@Lines[[1]]@coords[,1]),
                                            lat = c(temp@lines[[k]]@Lines[[1]]@coords[,2]),
                                            opacity = 1, popup = temp$popup[[k]], color = temp$colour[[k]])
  } 
  return(inList)
}

# adding raster data to leaflet map
leafletRaster <- function(inList, temp, dataList) {
  if("water" %in% names(dataList)) {
    waterLeaflet <- leaflet::projectRasterForLeaflet(sim$dataList$water, method = "bilinear")
    temp <- raster::mask(temp, waterLeaflet, inverse=TRUE)
  }
  inList <- leaflet::addRasterImage(inList, temp, colors=rev(topo.colors(16)), opacity=0.35, project=FALSE)
  return(inList)
}


