
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
# 
# **If an object in sim$dataList is class(Spatial*DataFrame), data to be translated to risk
#   must be in first data column or column name must match object name
# 
#  *Needs testing for SpatialLines/Polygons/MultiPoints/Pixels/Grid

defineModule(sim, list(
  name = "calculateRisk",
  description = "Module translate data rasters into risk rasters", 
  keywords = c("risk", "translate"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", calculateRisk = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "calculateRisk.Rmd"),
  reqdPkgs = list("raster", "reproducible", "quickPlot"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("species", "character", NA, NA, NA, "Defines species of interest"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "riskParams", objectClass = "data.frame", desc = "Dataframe of species and data specific parameters for 'translateRisk' and combining risk functions", sourceURL = NA),
    expectsInput(objectName = "dataList", objectClass = "List", desc = "List of data layers", sourceURL = NA),
    expectsInput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS", sourceURL = NA),
    expectsInput(objectName = "crs", objectClass = "data.frame", desc = "PROJ.4 character string defining desired coordinate reference system", sourceURL = NA),
    expectsInput(objectName = "res", objectClass = "data.frame", desc = "Desired raster resolution, in meters. Single number or vector of two numbers", sourceURL = NA)
    ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "riskParams", objectClass = "data.frame", desc = "Dataframe of species and data specific parameters for 'translateRisk' and combining risk functions"),
    createsOutput(objectName = "riskList", objectClass = "List", desc = "List containing a 'risk' raster for each layer in the dataList")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.calculateRisk = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- calculateRiskInit(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim)+0.2, "calculateRisk", "risk")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "calculateRisk", "save")
    },
    plot = {
      # do stuff for this event
      calculateRiskPlot(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "calculateRisk", "plot")
    },
    save = {
      # do stuff for this event
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "calculateRisk", "save")
    },
    risk = {
      # do stuff for this event
      sim <- calculateRiskTranslate(sim)
      
      # schedule future event(s)
      if(is.na(P(sim)$.plotInitialTime)) {
        # if .plotInitialTime=NA, don't schedule plot event
      } else if( "plot" %in% subset(completed(sim), completed(sim)$moduleName=="calculateRisk")$eventType == FALSE) {
        if( time(sim) >= P(sim)$.plotInitialTime) {
          sim <- scheduleEvent(sim, time(sim), "calculateRisk", "plot")
        } else { 
          sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "calculateRisk", "plot")
        }
      }
      # sim <- scheduleEvent(sim, time(sim) + increment, "calculateRisk", "templateEvent")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
calculateRiskInit <- function(sim) {

  
  return(invisible(sim))
}

### plot event:
calculateRiskPlot <- function(sim) {
  
  Plot(sim$riskList, title = TRUE, cols=rev(heat.colors(16))) #legendRange = 0:1
  if("water" %in% names(sim$dataList)) { #if water is a layer in dataList, add to risk maps
    for(m in names(sim$riskList)) { 
      Plot(sim$dataList[["water"]], addTo=m)
    }
  }
  
  return(invisible(sim))
}

### risk event: translate dataList rasters to risk rasters
calculateRiskTranslate <- function(sim) {
  
  sim$riskList <- list()
  
  for(i in 1:length(sim$dataList)){
    if( names(sim$dataList[i]) %in% sim$riskParams$layer ) { #if data layer is included in SpeciesRiskParams.csv
      
      tempParams <- subset(sim$riskParams, layer==names(sim$dataList[i])) #subset riskParams
      
      if(class(sim$dataList[[i]]) == "RasterLayer") { #if data layer is a RasterLayer
        
        fw <- focalWeightRisk(sim$dataList[[i]], 
                              type = as.character(tempParams$value[grep("type",tempParams$parameter)]),
                              a = as.numeric(as.character(tempParams$value[grep("a",tempParams$parameter)])), 
                              b = as.numeric(as.character(tempParams$value[grep("b",tempParams$parameter)])), 
                              riskCutoff = as.numeric(as.character(tempParams$value[grep("riskCutoff",tempParams$parameter)])))
        risk <- Cache(raster::focal, sim$dataList[[i]], w=fw, pad=T, padValue=0)
        #risk <- risk/max(raster::values(risk), na.rm=T) #  scale to 1 - can remove and scale elsewhere
        risk <- risk * as.numeric(as.character(tempParams$value[grep("weight",tempParams$parameter)]))
        #risk <- risk * ifelse(temp$params$direction=="increase", 1, -1) * as.numeric(temp$params$weight)
        names(risk) <- paste0(names(sim$dataList)[[i]],"Risk")
        sim$riskList[[names(risk)]] <- risk
        
      } else  if( is(sim$dataList[[i]], "Spatial") ) { #if data layer is Spatial*, needs to be rasterized to translate risk
        if("RasterLayer" %in% lapply(sim$dataList, class)) { #if there is a raster layer in the the dataList, use it as the template raster
          tempRas <- sim$dataList[[match("RasterLayer", lapply(sim$dataList, class))]] 
        } else { #if no data rasters in dataList, use default/set parameters
          tempRas <- raster::raster(ext=raster::extent(sim$ROI), resolution=sim$res, crs=raster::crs(sim$crs))
        }
        
        if(names(sim$dataList[i]) %in% names(sim$dataList[[i]])) { #if name of data layer matches column in dataframe, rasterize that field
          temp <- Cache(raster::rasterize, sim$dataList[[i]], tempRas,
                        field=names(sim$dataList)[[i]],fun=max, 
                        background=0)
        } else { #if name doesn't match, use first column in dataframe
          temp <- Cache(raster::rasterize, sim$dataList[[i]], tempRas,
                        field=names(sim$dataList[[i]][1]),fun=max, 
                        background=0)
        }
        
        fw <- focalWeightRisk(temp, 
                              type = as.character(tempParams$value[grep("type",tempParams$parameter)]),
                              a = as.numeric(as.character(tempParams$value[grep("a",tempParams$parameter)])), 
                              b = as.numeric(as.character(tempParams$value[grep("b",tempParams$parameter)])), 
                              riskCutoff = as.numeric(as.character(tempParams$value[grep("riskCutoff",tempParams$parameter)])))
        risk <- Cache(raster::focal, temp, w=fw, pad=T, padValue=0)
        #risk <- risk/max(raster::values(risk), na.rm=T) #  scale to 1 - can remove and scale elsewhere
        risk <- risk * as.numeric(as.character(tempParams$value[grep("weight",tempParams$parameter)]))
        names(risk) <- paste0(names(sim$dataList)[[i]],"Risk")
        sim$riskList[[names(risk)]] <- risk
        
      }
    } else { #data layer not included in SpeciesRiskParams.csv
      message(paste0('Message: "', names(sim$dataList[i]),'" ', "not matched in ~/modules/calculateRisk/data/SpeciesRiskParams.csv. ",
                     'No risk translation completed for "', names(sim$dataList[i]),'".'))
    }
  }
  
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  
  # read in SpeciesRiskParams.csv
  if(is.null(sim$riskParams)){
    if("SpeciesRiskParams.csv" %in% list.files(file.path(modulePath(sim),"calculateRisk","data"))) {
      
      if( P(sim)$species %in% read.csv(file.path(modulePath(sim),"calculateRisk","data","SpeciesRiskParams.csv"))$species ) {
        sim$riskParams <- subset(read.csv(file.path(modulePath(sim),"calculateRisk","data","SpeciesRiskParams.csv")), species==P(sim)$species)
      } else {
        stop(paste0("calculateRisk: species '", P(sim)$species, "' not found in SpeciesRiskParams.csv. Add species to csv file or select different species." )
        )
      }
    } else {
      stop("calculateRisk: There is no SpeciesRiskParams.csv dataframe provided") }
  }
  
  ##### Create data if sim$dataList not provided
  #if(is.null(sim$dataList)){
  #  sim$dataList <- list()
  #  
  #  exampleRas <- raster::raster(ext=raster::extent(P(sim)$ROI), 
  #                               resolution=P(sim)$res, 
  #                               crs=raster::crs(P(sim)$crs))
  #  values(exampleRas) <- runif(length(exampleRas), min=1, max=100)
  #  names(exampleRas) <- "treeCover"
  #  
  #  sim$dataList[["treeCover"]] <- exampleRas
  #}
  #
  ##### Create ROI if sim$ROI not provided
  #if(is.null(sim$ROI)) {
  #  x <- c(-1935000, -1934000, -1909000, -1911000, -1917000, -1922000, -1938000, -1935000)
  #  y <- c(1410000, 1407000, 1405000, 1414000, 1414500, 1413800, 1414200, 1410000)
  #  ROI <- cbind(x, y) %>%
  #    Polygon() %>%
  #    list() %>%
  #    Polygons("s1") %>%
  #    list() %>%
  #    SpatialPolygons(1L)
  #  crs(ROI) <- P(sim)$crs
  #  sim$ROI <- ROI
  #}
  #
  ##### Create crs if sim$crs not provided
  #if(is.null(sim$crs)) {
  #sim$crs <- crs("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0)
  #}
  #
  ##### Create res if sim$res not provided
  #if(is.null(sim$res)) {
  #sim$res <- 30
  #}
  
  return(invisible(sim))
}

#### Risk functions in "~/modules/calculateRisk/R" folder

#SpatialClasses <- c("SpatialPointsDataFrame", "SpatialLinesDataFrame", "SpatialPolygonsDataFrame", 
#                    "SpatialMultiPointsDataFrame", "SpatialPixelsDataFrame", "SpatialGridDataFrame",
#                    "SpatialPoints", "SpatialLines", "SpatialPolygons", "SpatialMultiPoints", "SpatialPixels", "SpatialGrid")

