
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "loadGMTraps",
  description = "Module that imports Gypsy Moth trapping data and applies basic formatting", 
  keywords = c("traps", "Gypsy Moth"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", loadGMTraps = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "loadGMTraps.Rmd"),
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
    expectsInput(objectName = "traps", objectClass = "SpatialPointsDataFrame", desc = "Raw trap data containing trap locations and trap catch"),
    expectsInput(objectName = "dataListInit", objectClass = "List", desc = "List of data rasters. If NULL, recreates an empty list in .inputsObjects", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataListInit", objectClass = "List", desc = "List of data containing 'traps' dataset")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.loadGMTraps = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "loadGMTraps", "format")
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "loadGMTraps", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "loadGMTraps", "save")
    },
    plot = {
      # do stuff for this event
      loadGMTrapsPlot(sim) 
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "loadGMTraps", "plot")

    },
    save = {
      # do stuff for this event

      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "loadGMTraps", "save")

          },
    format = {
      # do stuff for this event
      sim <- loadGMTrapsFormat(sim)

      # schedule future event(s)
      #sim <- scheduleEvent(sim, time(sim) + increment, "loadGMTraps", "templateEvent")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##


### plot event:
loadGMTrapsPlot <- function(sim) {

  noCatch <- subset(dataListInit[["traps"]], traps == -1)
  posCatch <- subset(dataListInit[["traps"]], traps > 0)
  Plot(noCatch, title = "Trap Locations")
  Plot(posCatch, addTo = "noCatch", col="red")

  return(invisible(sim))
}

### format event: formatting traps dataset
loadGMTrapsFormat <- function(sim) {

  tempTraps <- sim$dataListInit[["traps"]]
  #tempTraps@data$traps <- suppressWarnings(as.numeric(substr(tempTraps@data$Name, 3, 3)))
  tempTraps@data$traps[is.na(tempTraps@data$traps)] <- 0
  tempTraps@data$traps[tempTraps@data$traps==0] <- -1                            ##################
  tempTraps@coords <- tempTraps@coords[,c(1:2)]
  if(length(subset(tempTraps, traps>0)) > 0) { 
    #unique ID for positive traps
    tempTraps@data$num <- 1:length(tempTraps)
    posPoints <- subset(tempTraps, traps>0)
    posPoints$ID <- 1:length(posPoints)
    tempTraps@data$ID <- merge(data.frame(subset(tempTraps, select=c("num"))),
                                   data.frame(subset(posPoints, select=c("num", "ID"))),
                                   all=T, by="num")$ID
    tempTraps$num <- NULL
  } else {
    tempTraps@data$ID <- 1:length(tempTraps)
  }
  sim$dataListInit[["traps"]] <- tempTraps
  sim$dataListInit$traps@data$traps[sim$dataListInit$traps@data$traps==0]<--1  #####################
  
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$userSuppliedObjNames)) {
  #  defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  # create sim$dataListInit if it doesn't exist
  if(is.null(sim$dataListInit)){
    sim$dataListInit <- list()
  }
  
  #  #import 2015 traps
#   if("2015 prelim.kml" %in% list.files(file.path(modulePath(sim),"loadGMTraps","data"))) {
     #ogrListLayers(file.path(modulePath(sim),"loadGMTraps","data","2015 prelim.kml")) # to list layers within .kml file
#     sim$dataListInit[["traps"]] <- rgdal::readOGR(dsn=file.path(modulePath(sim),"loadGMTraps","data","2015 prelim.kml"),layer = "Waypoints")

#        } else {
#     stop("loadGMTraps: There is no trapping dataset provided") }
  
  # import 2017 traps
  
  if("CurrentYear_TrapData.csv" %in% list.files(file.path(modulePath(sim),"loadGMTraps","data"))) {
    #sim$dataListInit[["traps"]] <- rgdal::readOGR(dsn=file.path(modulePath(sim),"loadGMTraps","data","2017 prelim.kml"),layer = "2018GM Prelim")
    trapdat<-read.csv(file=file.path(modulePath(sim), "loadGMTraps", "data", "CurrentYear_TrapData.csv"))
    trapdat$origID<-1:dim(trapdat)[1]
    traps_spdf<-SpatialPointsDataFrame(coords=trapdat[,c("Longitude", "Latitude")],
                                       proj4string=crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                       data=trapdat[,-which(names(trapdat)=="Latitude" | names(trapdat)=="Longitude")])
    
    d <- gDistance(traps_spdf, byid=T)
    diag(d) <- NA
    nnPts<-unlist(apply(d,1,function(x) which.min(x)))
    minDist<-unlist(apply(d,1,function(x) min(x,na.rm=TRUE)))
    traps_spdf@data$nnPts<-c(nnPts)
    traps_spdf@data$minDist<-minDist
    
    traps_spdf<-traps_spdf[order(traps_spdf@data$minDist),]
    
    for (i in 1:length(traps_spdf)){
      if ( (traps_spdf@data$minDist[i]<0.0001) & (traps_spdf@data$traps[traps_spdf@data$nnPts[i]]==0) ){
        traps_spdf<-traps_spdf[traps_spdf@data$origID!=traps_spdf@data$nnPts[i],]
      }
      if (i>=length(traps_spdf)) {break}
    }
    sim$dataListInit[["traps"]] <- traps_spdf

  } else {
    stop("loadGMTraps: There is no trapping dataset provided")
  }
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
