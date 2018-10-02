
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
    defineParameter("usePlot", "logical", TRUE, NA, NA, "Should module include plotting"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "riskParams", objectClass = "data.frame", desc = "Dataframe of species and data specific parameters for 'translateRisk' and combining risk functions", sourceURL = NA),
    expectsInput(objectName = "dataList", objectClass = "List", desc = "List of data layers", sourceURL = NA),
    expectsInput(objectName = "ROI", objectClass = "SpatialPolygons", desc = "Polygon defining region of interest (ROI), in desired PROJ.4 CRS", sourceURL = NA)
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
      # do stuff for this event
      sim <- calculateRiskInit(sim)
  
      sim <- scheduleEvent(sim, start(sim) + 2, "calculateRisk", "risk")

    },
    
    risk = {
      # do stuff for this event
      sim <- Cache(calculateRiskTranslate, sim)
      
     if (P(sim)$usePlot){
       sim <- scheduleEvent(sim, time(sim), "calculateRisk", "plot")
     }
    
    },
    
    plot = {
      # do stuff for this event
      calculateRiskPlot(sim)
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
 
  for (i in 1:length(sim$riskList)) {
    clearPlot()
    Plot(sim$riskList[[i]], title = names(sim$riskList[[i]]), cols=rev(heat.colors(16))) #legendRange = 0:1
    if("water" %in% names(sim$dataList[[i]])) { #if water is a layer in dataList, add to risk maps
      for(m in names(sim$riskList[[i]])) { 
        Plot(sim$dataList[[i]][["water"]], addTo= m)
      }
    }
   }  
  return(invisible(sim))
}

### risk event: translate dataList rasters to risk rasters
calculateRiskTranslate <- function(sim) {
  
  i <- 1:length(sim$dataList)
  outList <- lapply(i, FUN = function(i, dataList = sim$dataList, riskParams = sim$riskParams,
                                      ROI = sim$ROI) { 
   
    ROI <- ROI[[i]]
    riskList <- list()
    dataList <- dataList[[i]] #Subset dataList by ROI
    
    for(ii in 1:length(dataList)){
      if( names(dataList[ii]) %in% riskParams$layer ) { #if data layer is included in SpeciesRiskParams.csv
        
        tempParams <- subset(riskParams, layer==names(dataList[ii])) #subset riskParams
        
        if(class(dataList[[ii]]) == "RasterLayer") { #if data layer is a RasterLayer
          
          fw <- focalWeightRisk(dataList[[ii]], 
                                type = as.character(tempParams$value[grep("type",tempParams$parameter)]),
                                a = as.numeric(as.character(tempParams$value[grep("a",tempParams$parameter)])), 
                                b = as.numeric(as.character(tempParams$value[grep("b",tempParams$parameter)])), 
                                riskCutoff = as.numeric(as.character(tempParams$value[grep("riskCutoff",
                                                                                           tempParams$parameter)])))
          risk <- Cache(raster::focal, dataList[[ii]], w=fw, pad=T, padValue=0)
          #risk <- risk/max(raster::values(risk), na.rm=T) #  scale to 1 - can remove and scale elsewhere
          risk <- risk * as.numeric(as.character(tempParams$value[grep("weight",tempParams$parameter)]))
          #risk <- risk * ifelse(temp$params$direction=="increase", 1, -1) * as.numeric(temp$params$weight)
          names(risk) <- paste0(names(dataList)[[ii]],"Risk")
          riskList[[names(risk)]] <- risk
          
        } else  if( is(dataList[[ii]], "Spatial") ) { 
          #if data layer is Spatial*, needs to be rasterized to translate risk
          if("RasterLayer" %in% lapply(dataList, class)) { 
            #if there is a raster layer in the the dataList, use it as the template raster
            tempRas <- dataList[[match("RasterLayer", lapply(dataList, class))]] 
          } else { #if no data rasters in dataList, use default/set parameters
            tempRas <- raster::raster(ext=raster::extent(ROI), resolution = res(ROI), crs = crs(ROI))
          }
          
          if(names(dataList[ii]) %in% names(dataList[[ii]])) { 
            #if name of data layer matches column in dataframe, rasterize that field
            temp <- Cache(raster::rasterize, dataList[[ii]], tempRas,
                          field=names(dataList)[[ii]],fun=max, 
                          background=0)
          } else { #if name doesn't match, use first column in dataframe
            temp <- Cache(raster::rasterize, dataList[[ii]], tempRas,
                          field=names(dataList[[ii]][1]),fun=max, 
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
          names(risk) <- paste0(names(dataList)[[ii]],"Risk")
          riskList[[names(risk)]] <- risk
          
        }
      } else { #data layer not included in SpeciesRiskParams.csv
        
        if (names(dataList[ii]) != "water") {
          message(paste0('Message: "', names(dataList[ii]),'" ', 
                         "not matched in ~/modules/calculateRisk/data/SpeciesRiskParams.csv. ",
                         'No risk translation completed for "', names(dataList[ii]),'".'))
        }
      }
    }
    return(riskList)
  })
  
  names(outList) <- paste(names(sim$dataList), "_risk")
  sim$riskList <- outList
  
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
  
  
  
  return(invisible(sim))
}
