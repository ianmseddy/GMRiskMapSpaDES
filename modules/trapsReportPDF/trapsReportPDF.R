
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName

### For trapReportPDF module to work, traps dataset must have column named "ID" with unique identifier for each trap

defineModule(sim, list(
  name = "trapsReportPDF",
  description = "Module creates a PDF document reporting positive trap catch locations and populations ", 
  keywords = c("PDF", "trapping"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", trapsReportPDF = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "trapsReportPDF.Rmd"),
  reqdPkgs = list("maptools", "dismo", "raster", "sp", "gridExtra", "grid", "ggmap", "curl"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("dataName", "character", "traps", NA, NA, "Name of the trapping dataset in dataList to map"),
    defineParameter("fileName", "character", "trapsReport", NA, NA, "File name the PDF report will be saved as"),
    defineParameter("saveDir", "character", outputPath(sim), NA, NA, "Directory path where PDF report will be saved. Default is outputPath(sim)"),
    defineParameter("popDistType", "character", "straight", NA, NA, "Shape of the population distance curve: 'straight', 'square', 'linear', 'logistic', 'gaussian'"),
    defineParameter("popMaxDist", "numeric", 2000, 0, 20000, "Distance defining the maximum search radius for traps of the same population. Must be defined."),
    defineParameter("popMinDist", "numeric", 1000, 0, 20000, "Distance defining the minimum search radius for traps of the same population"),
    defineParameter("popMaxCatch", "integer", 10, 1, 100, "Maximum catch number where the search radius is at maxDist"),
    defineParameter("basemap", "character", "roadmap", NA, NA, "Basemap type of map. Options: satellite, roadmap, hybrid, terrain"),
    defineParameter("mapRisk", "logical", FALSE, NA, NA, "Logical of whether to map totalRisk to PDF map"),
    defineParameter("mapHiRisk", "logical", FALSE, NA, NA, "Logical of whether to map hiRisk to PDF map"),
    defineParameter(".pdfInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first pdf event should occur"),
    defineParameter(".pdfInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between pdf events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "dataList", objectClass = "List", desc = "List of data rasters, including object named P(sim)$dataName", sourceURL = NA),
    expectsInput(objectName = "totalRisk", objectClass = "RasterLayer", desc = "Raster of totalRisk"), 
    expectsInput(objectName = "highRisk", objectClass = "RasterLayer", desc = "Raster of high risk. Will be null if mapHiRisk = FALSE.")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.trapsReportPDF = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
     
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "trapsReportPDF", "checkinputs", .last())
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "trapsReportPDF", "save")
    },
    checkinputs = {
      
      # schedule future event(s)
      if(is.na(P(sim)$.pdfInitialTime)) {
        # if .pdfInitialTime=NA, don't schedule pdf event
      } else if( "pdfopen" %in% subset(completed(sim), completed(sim)$moduleName=="trapsReportPDF")$eventType == FALSE) {
        if( time(sim) >= P(sim)$.pdfInitialTime) { # if pdfInitialTime has passed, open pdf now
          
          if( (P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE) && !is.null(sim$dataList) && !is.null(sim$totalRisk)) {
            if(curl::has_internet() == FALSE) {
              message("trapsReportPDF: No internet connection. Cannot generate PDF.")
            } else sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfopen")
            
          } else if( (P(sim)$mapRisk == FALSE && P(sim)$mapHiRisk == FALSE) && !is.null(sim$dataList) ) {
            if(curl::has_internet() == FALSE) {
              message("trapsReportPDF: No internet connection. Cannot generate PDF.")
            } else sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfopen")
            
          } else {
            sim <- scheduleEvent(sim, time(sim)+0.1, "trapsReportPDF", "checkinputs", .last())
          }
          
        } else {  # if pdfInitialTime has not passed, schedule to open pdf at pdfInitialTime
          
          if( (P(sim)$mapRisk == TRUE || P(sim)$mapHiRisk == TRUE) && !is.null(sim$dataList) && !is.null(sim$totalRisk)) {
            if(curl::has_internet() == FALSE) {
              message("trapsReportPDF: No internet connection. Cannot generate PDF.")
            } else sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfopen")
            
          } else if( (P(sim)$mapRisk == FALSE && P(sim)$mapHiRisk == FALSE) && !is.null(sim$dataList) ) {
            if(curl::has_internet() == FALSE) {
              message("trapsReportPDF: No internet connection. Cannot generate PDF.")
            } else sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfopen")
            
          } else {
            sim <- scheduleEvent(sim, time(sim)+0.1, "trapsReportPDF", "checkinputs", .last())
          }
          
        }
      }
    },
    pdfopen = {
      # do stuff for this event
      sim <- trapsReportPDFopen(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfmap")
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.pdfInterval, "trapsReportPDF", "pdfopen")
    },
    pdfmap = {
      # do stuff for this event
      sim <- trapsReportPDFoverall(sim)
      sim <- trapsReportPDFtrapextent(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdftraps")
      # sim <- scheduleEvent(sim, time(sim) + increment, "trapsReportPDF", "pdfmap")
    },
    pdftraps = {
      # do stuff for this event
      sim <- trapsReportPDFtraps(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfdata")
      # sim <- scheduleEvent(sim, time(sim) + increment, "trapsReportPDF", "pdftraps")
    },
    pdfdata = {
      # do stuff for this event
      sim <- trapsReportPDFdata(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "trapsReportPDF", "pdfclose")
      # sim <- scheduleEvent(sim, time(sim) + increment, "trapsReportPDF", "pdftraps")
    },
    pdfclose = {
      # do stuff for this event
      sim <- trapsReportPDFclose(sim)
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "trapsReportPDF", "pdfclose")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:


### pdfopen event:
trapsReportPDFopen <- function(sim) {
  
  # checks if "dataName" matches a layer in the dataList
  if( P(sim)$dataName %in% names(sim$dataList) == FALSE ) {
    stop( paste0("dataName = '", P(sim)$dataName, "' not in sim$dataList. Cannot create PDF report for '", P(sim)$dataName, "'." ))
  }
  
  # checks if any positive trap catches in ROI
  if( P(sim)$dataName %in% names(sim$dataList[[P(sim)$dataName]]) ) {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[charmatch(P(sim)$dataName, names(sim$dataList[[P(sim)$dataName]]))]] > 0)
    if(length(posTraps)==0) { stop("trapsReportPDF: No positive trap catches in selected ROI for dataset '", P(sim)$dataName, "'") }
  } else {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[1]] > 0)
    if(length(posTraps)==0) { stop("trapsReportPDF: No positive trap catches in selected ROI for dataset '", P(sim)$dataName, "'") }
  }
  
  # open PDF device
  if("pdf" %in% unlist(strsplit(P(sim)$fileName, "[.]")) == FALSE) {
    fileName <- paste0(P(sim)$fileName,".pdf")
    pdf(file.path(P(sim)$saveDir,fileName), width=7, height=9, onefile=TRUE, paper = "letter")
  } else pdf(file.path(P(sim)$saveDir, P(sim)$fileName), width=7, height=9, onefile=TRUE, paper = "letter")
  
  return(invisible(sim))
}


### pdfmap event: first page of PDF - map of entire ROI
trapsReportPDFoverall <- function(sim) {
  
  #### get google basemap
  roiGoogle <- sp::spTransform(sim$ROI, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  zoomLevel <- ggmap::calc_zoom(c(xmin(roiGoogle), xmax(roiGoogle)), c(ymin(roiGoogle), ymax(roiGoogle)))
  mapGoogle <- dismo::gmap(x = roiGoogle, type = P(sim)$basemap, lonlat=TRUE, zoom=zoomLevel-1)
  box <- as(raster::extent(mapGoogle), 'SpatialPolygons')
  plot_gmap(mapGoogle) #plot
  
  # if mapRisk=TRUE: add totalRisk to map
  if(P(sim)$mapRisk == TRUE) {
    riskGoogle <- raster::projectRaster(sim$totalRisk, mapGoogle)
    riskGoogle[riskGoogle<=0] <- NA
    if("water" %in% names(sim$dataList)) {
      waterMask <- raster::projectRaster(sim$dataList$water, mapGoogle, method="ngb")
      riskGoogle <- raster::mask(riskGoogle, waterMask, inverse=TRUE)
    }
    plot(riskGoogle, add=T, col=rev(heat.colors(16)), legend=FALSE, alpha=0.3)
  }
  # if mapHiRisk=TRUE: add hiRisk to map
  if(P(sim)$mapHiRisk == TRUE) {

    highRiskGoogle <- raster::projectRaster(sim$highRisk, mapGoogle)
    highRiskGoogle[highRiskGoogle<=0] <- NA
    if("water" %in% names(sim$dataList)) {
      waterMask <- raster::projectRaster(sim$dataList$water, highRiskGoogle, method="ngb")
      highRiskGoogle <- raster::mask(highRiskGoogle, waterMask, inverse=TRUE)
    }
    plot(highRiskGoogle, add=T, legend=FALSE, alpha=0.4)
  }
  
  # subset positive traps 
  if( P(sim)$dataName %in% names(sim$dataList[[P(sim)$dataName]]) ) {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[charmatch(P(sim)$dataName, names(sim$dataList[[P(sim)$dataName]]))]] > 0)
  } else {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[1]] > 0)
  }
  # plot traps to map
  posTrapsGoogle <- sp::spTransform(posTraps, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  points(posTrapsGoogle, pch=16, cex=1)
  
  # add legend, scalebar, box
  leg <- legend("bottomleft", legend=paste("Positive traps    "), 
                pch=16, cex=0.8, pt.cex=1, bg="white", plot=FALSE)
  legbox <- as(raster::extent(c(xmin(box), xmax(box), ymin(box)-leg$rect$h, ymin(box))), "SpatialPolygons")
  legend(x = xmin(legbox), y = ymax(legbox), legend=paste("Positive traps"),
         pch=16, cex=0.8, pt.cex=1, bg="white", bty = "n")
  temp <- projectRaster(mapGoogle, crs=crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
  xDist <- round((sqrt((xmax(temp)-xmin(temp))^2 + (ymax(temp)-ymin(temp))^2)/1000)/10)
  if(xDist < 1) xDist <- 1
  distOpts <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
  scaleDist <- distOpts[length(xDist>=distOpts[(xDist >= distOpts) == TRUE])]
  scaleDivs <- ifelse(scaleDist <= 5, 2, 4)
  
  raster::scalebar(d=scaleDist, xy=c(leg$rect$left + leg$rect$w, ymin(legbox) + (leg$rect$h/3)),
                   type="bar", divs=scaleDivs, lonlat=TRUE, below="km", cex=0.7)
  text(x = (xmax(box)+xmin(box))/2, y = ymax(box)+(leg$rect$h/2),
       label="Project Region of Interest", font=2, cex=1.5)
  box2 <- as(extent(c(xmin(box), xmax(box), ymin(legbox), ymax(box))), "SpatialPolygons")
  plot(box2, add=T)
  
  return(invisible(sim))
}


### pdfmap event: second page of PDF - map of all traps (could be smaller than entire ROI)
trapsReportPDFtrapextent <- function(sim) {
 
  # subset positive traps 
  if( P(sim)$dataName %in% names(sim$dataList[[P(sim)$dataName]]) ) {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[charmatch(P(sim)$dataName, names(sim$dataList[[P(sim)$dataName]]))]] > 0)
  } else {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[1]] > 0)
  }
  
  #### get google basemap
  posTrapsGoogle <- sp::spTransform(posTraps, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  
  # extent of posTraps plus added buffer
  zoomLevel <- ggmap::calc_zoom(c(xmin(posTrapsGoogle)-(xmax(posTrapsGoogle)-xmin(posTrapsGoogle))/10,  
                                  xmax(posTrapsGoogle)+(xmax(posTrapsGoogle)-xmin(posTrapsGoogle))/10), 
                                c(ymin(posTrapsGoogle)-(ymax(posTrapsGoogle)-ymin(posTrapsGoogle))/10, 
                                  ymax(posTrapsGoogle)+(ymax(posTrapsGoogle)-ymin(posTrapsGoogle))/10))  
  mapGoogle <- dismo::gmap(x = posTrapsGoogle, type = P(sim)$basemap, lonlat=TRUE, zoom=zoomLevel-1)
  # temp_ras <- projectRaster(sim$totalRisk, mapGoogle)
  # mapGoogle <- dismo::gmap(x = temp_ras, type = P(sim)$basemap, lonlat = TRUE, zoom = zoomLevel-1)
  
  box <- as(raster::extent(mapGoogle), 'SpatialPolygons')
  plot_gmap(mapGoogle) #plot
  
  # plot traps to map
  posTrapsGoogle <- sp::spTransform(posTraps, mapGoogle@crs)
  plot(posTrapsGoogle, pch = 16, cex = 0.8, add=T)
  
  # if mapRisk=TRUE: add totalRisk to map
    if(P(sim)$mapRisk == TRUE) {
    riskGoogle <- raster::projectRaster(sim$totalRisk, mapGoogle)
    riskGoogle[riskGoogle<=0] <- NA
    if("water" %in% names(sim$dataList)) {
      waterMask <- raster::projectRaster(sim$dataList$water, mapGoogle, method="ngb")
      riskGoogle <- raster::mask(riskGoogle, waterMask, inverse=TRUE)
    }
    plot(riskGoogle, add=T, col=rev(heat.colors(16)), legend=FALSE, alpha=0.3)#rev(heat.colors(16))
  }
  # if mapHiRisk=TRUE: add hiRisk to map
  if(P(sim)$mapHiRisk == TRUE) {

    highRiskGoogle <- raster::projectRaster(sim$highRisk, mapGoogle)
    highRiskGoogle[highRiskGoogle<=0] <- NA
    if("water" %in% names(sim$dataList)) {
      waterMask <- raster::projectRaster(sim$dataList$water, highRiskGoogle, method="ngb")
      highRiskGoogle <- raster::mask(highRiskGoogle, waterMask, inverse=TRUE)
    }
    plot(highRiskGoogle, add=T, col="#FF0000", legend=FALSE, alpha=0.4)
  }


  # add legend, scalebar, box
  leg <- legend("bottomleft", legend=paste("Positive traps    "), 
                pch=16, cex=0.8, pt.cex=1, bg="white", plot=FALSE)
  legbox <- as(raster::extent(c(xmin(box), xmax(box), ymin(box)-leg$rect$h, ymin(box))), "SpatialPolygons")
  legend(x = xmin(legbox), y = ymax(legbox), legend=paste("Positive traps"),
         pch=16, cex=0.8, pt.cex=1, bg="white", bty = "n")
  
  #temp <- sp::spTransform(posTrapsGoogle, CRSobj = sp::CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
    temp <- projectRaster(mapGoogle, crs=crs("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
  xDist <- round((sqrt((xmax(temp)-xmin(temp))^2 + (ymax(temp)-ymin(temp))^2)/1000)/10)
  if(xDist < 1) xDist <- 1
  distOpts <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
  scaleDist <- distOpts[length(xDist>=distOpts[(xDist >= distOpts) == TRUE])]
  scaleDivs <- ifelse(scaleDist <= 5, 2, 4)
  
  raster::scalebar(d=scaleDist, xy=c(leg$rect$left + leg$rect$w, ymin(legbox) + (leg$rect$h/3)),
                   type="bar", divs=scaleDivs, lonlat=TRUE, below="km", cex=0.7)
  text(x = (xmax(box)+xmin(box))/2, y = ymax(box)+(leg$rect$h/2),
       label="Positive Trap Locations", font=2, cex=1.5)
  box2 <- as(extent(c(xmin(box), xmax(box), ymin(legbox), ymax(box))), "SpatialPolygons")
  plot(box2, add=T)
  dev.off()
  return(invisible(sim))
}


### pdftraps event: middle pages of PDF - maps of traps/populations
trapsReportPDFtraps <- function(sim) {
  
  # subset positive traps
  if( P(sim)$dataName %in% names(sim$dataList[[P(sim)$dataName]]) ) {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[charmatch(P(sim)$dataName, names(sim$dataList[[P(sim)$dataName]]))]] > 0)
  } else {
    posTraps <- subset(sim$dataList[[P(sim)$dataName]], sim$dataList[[P(sim)$dataName]][[1]] > 0)
  }
  # project to google crs
  
  posTrapsGoogle <- sp::spTransform(posTraps, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))

  posTraps <- posTraps[with(posTraps, order(posTraps$traps, decreasing=TRUE)), ] # reorder posTraps by trap catch
  popList <- list() # to track separate populations
  trapLabels <- c() # to keep track of which traps have been plotted 
  for(i in 1:length(posTraps)) { 
    if(posTraps$ID[i] %in% trapLabels){
    } else{ 
      
      
      trapROI <- sp::polygons(dismo::circles(data.frame(x = sp::coordinates(posTraps)[i,][1], 
                                                        y = sp::coordinates(posTraps)[i,][2]),
                                             lonlat=FALSE, n=500, dissolve=FALSE,
                                             d=populationSearchDistance(x = posTraps[i,]$traps, 
                                                                        type=P(sim)$popDistType, 
                                                                        maxCatch=P(sim)$popMaxCatch, 
                                                                        minDist=P(sim)$popMinDist, 
                                                                        maxDist=P(sim)$popMaxDist)))
      crs(trapROI) <- crs(posTraps)
      trapCrop <- raster::crop(posTraps, trapROI)
      
      for( j in 1:length(trapCrop) ) {
        tempROI <- sp::polygons(dismo::circles(data.frame(x = sp::coordinates(trapCrop)[j,][1], 
                                                          y = sp::coordinates(trapCrop)[j,][2]),
                                               lonlat=FALSE, n=500, dissolve=FALSE,
                                               d=populationSearchDistance(x = trapCrop[j,]$traps, 
                                                                          type=P(sim)$popDistType, 
                                                                          maxCatch=P(sim)$popMaxCatch, 
                                                                          minDist=P(sim)$popMinDist, 
                                                                          maxDist=P(sim)$popMaxDist)))
        
        crs(tempROI) <- crs(posTraps)
        tempTraps <- raster::crop(posTraps, tempROI)
        
        if( FALSE %in% (tempTraps$ID %in% trapCrop$ID) == FALSE ) {
        } else { trapCrop <- spRbind(trapCrop,  tempTraps[match(FALSE, tempTraps$ID %in% trapCrop$ID ), ]) }
      }
      trapLabels <- c(trapLabels, as.character(trapCrop$ID))
      tempList <- list(trapCrop$ID)
      popList <- c(popList, tempList)
      
      # reset trap extent for mapping
      if(length(trapCrop) > 1) { 
        trapROI <- raster::extent(trapCrop)
        #trapROI <- raster::extent(raster::extent(trapCrop)[1] - 500, raster::extent(trapCrop)[2] + 500,
        #                          raster::extent(trapCrop)[3] - 500, raster::extent(trapCrop)[4] + 500)
      }
      
      # subsetting google traps to match trapCrop
      trapCropGoogle <- posTrapsGoogle[match(trapCrop$ID, posTrapsGoogle$ID),]
      
      # get google basemap
      tempGoogleROI <- raster::extent(raster::projectExtent(raster::raster(trapROI, crs=raster::crs(posTraps)), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
      tempGoogleMap <- dismo::gmap(x = tempGoogleROI, type = P(sim)$basemap, lonlat=TRUE, zoom=13)
      
      # checking all traps are on google map - if not, zoom out 1 zoom level
      trapsPoly <- as(extent(trapCropGoogle),"SpatialPolygons")
      crs(trapsPoly) <- crs(tempGoogleMap)
      mapPoly <- as(extent(tempGoogleMap),"SpatialPolygons")
      crs(mapPoly) <- crs(tempGoogleMap)
      if ( rgeos::gCovers(mapPoly, trapsPoly) == FALSE ) tempGoogleMap <- dismo::gmap(x = extent(trapsPoly), type = "roadmap", lonlat=TRUE, zoom=12)
      
      # box and plotting
      box2 <- as(raster::extent(tempGoogleMap), 'SpatialPolygons')
      plot_gmap(tempGoogleMap) #plot
      
      # if mapRisk=TRUE: add totalRisk to map
      if(P(sim)$mapRisk == TRUE) {
        tempGoogleRisk <- raster::projectRaster(sim$totalRisk, tempGoogleMap)
        tempGoogleRisk[tempGoogleRisk<=0] <- NA
        if("water" %in% names(sim$dataList)) {
          waterMask <- raster::projectRaster(sim$dataList$water, tempGoogleMap, method="ngb")
          tempGoogleRisk <- raster::mask(tempGoogleRisk, waterMask, inverse=TRUE)
        }
        plot(tempGoogleRisk, add=T, col=rev(heat.colors(16)), legend=FALSE, alpha=0.3)
      }
      # if mapHiRisk=TRUE: add hiRisk to map
      if(P(sim)$mapHiRisk == TRUE) {
        highRiskGoogleTemp <- raster::projectRaster(sim$highRisk, tempGoogleMap)
        highRiskGoogleTemp[highRiskGoogleTemp<=0] <- NA
        if("water" %in% names(sim$dataList)) {
          waterMask <- raster::projectRaster(sim$dataList$water, highRiskGoogleTemp, method="ngb")
          highRiskGoogleTemp <- raster::mask(highRiskGoogleTemp, waterMask, inverse=TRUE)
        }
        plot(highRiskGoogleTemp, add=T, col="#FF0000", legend=FALSE, alpha=0.4)
      }
      
      # setting point colours
      temp <- as.factor(trapCropGoogle$traps)
      trapCropGoogle$colour <- temp
      levels(trapCropGoogle$colour) <- c(levels(temp), rainbow(n = length(levels(temp))))
      for( k in 1:length(levels(temp)) ) {
        trapCropGoogle$colour[trapCropGoogle$colour == levels(temp)[k]] <- rainbow(n = length(levels(temp)))[k]
      }
      trapCropGoogle <- trapCropGoogle[with(trapCropGoogle, order(trapCropGoogle$traps)), ]
      
      # plot traps to map
      points(trapCropGoogle, pch=16, cex=1, col=trapCropGoogle$colour)
      maptools::pointLabel(x=sp::coordinates(trapCropGoogle)[,1], y=sp::coordinates(trapCropGoogle)[,2], labels=as.character(trapCropGoogle$ID), cex=0.8, font=1)
      
      # add legend, scalebar, box
      leg <- legend("bottomleft", legend=paste0("Catch: ", unique(trapCropGoogle$traps),"    "), 
                    pch=16, cex=0.8, pt.cex=1, bg="white", plot=FALSE)
      legbox <- as(raster::extent(c(xmin(box2), xmax(box2), ymin(box2)-leg$rect$h, ymin(box2))), "SpatialPolygons")
      legend(x = xmin(legbox), y = ymax(legbox), legend=paste0("Catch: ", unique(trapCropGoogle$traps)),
             pch=16, cex=0.8, pt.cex=1, bg="white", bty = "n", col=unique(trapCropGoogle$colour))
      raster::scalebar(d=1, xy=c(leg$rect$left + leg$rect$w, ymin(legbox) + 0.001688268),
                       type="bar", divs=2, lonlat=TRUE, below="km", cex=0.7)
      text(x = (xmax(box2)+xmin(box2))/2, y = ymax(box2)+(leg$rect$h/2),
           label=paste0("Population ", length(popList)), font=2, cex=1.5)
      box2 <- as(extent(c(xmin(box2), xmax(box2), ymin(legbox), ymax(box2))), "SpatialPolygons")
      plot(box2, add=T)
      
    }
  }
  
  sim$dataList[[P(sim)$dataName]]$populationID <- NA
  for(m in 1:length(popList)) {
    sim$dataList[[P(sim)$dataName]]$populationID[match(popList[[m]], sim$dataList[[P(sim)$dataName]]$ID)] <- m
  }
  
  return(invisible(sim))
}


### pdfdata event: last page of PDF - dataframe of trap information
trapsReportPDFdata <- function(sim) {
  
  posTraps <- subset(sim$dataList[[P(sim)$dataName]], !is.na(sim$dataList[[P(sim)$dataName]]$populationID))
  
  # project to google crs
  posTrapsGoogle <- sp::spTransform(posTraps, CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "))
  
  # dataframe for traps in ROI
  trapsDF <- data.frame(posTrapsGoogle)
  trapsDF$optional <- NULL
  colnames(trapsDF) <- c(names(posTrapsGoogle), "Longitude", "Latitude")
  trapsDF <- trapsDF[with(trapsDF, order(trapsDF$populationID)), ]
  
  # new page in PDF
  grid::grid.newpage()
  # plot data.frame to PDF
  gridExtra::grid.table(trapsDF, rows=NULL, theme = gridExtra::ttheme_default(base_size=10))
  
  sim$positiveTraps <- NULL
  
  return(invisible(sim))
}


### pdfclose event:
trapsReportPDFclose <- function(sim) {
  
  # close PDF device
  dev.off()
  
  return(invisible(sim))
}


## extra functions
plot_gmap <- function(ras,...){
  cols <- ras@legend@colortable
  z <- raster::unique(ras)
  par(mar=c(0,0,0,0))
  plot(ras,col=cols[z+1],legend=FALSE,box=FALSE,axes=FALSE,legend.mar=0,...)
}
