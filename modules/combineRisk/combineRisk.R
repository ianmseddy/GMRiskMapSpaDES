
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
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"), 
    defineParameter("hiRisk1", "numeric", 0.5, 0, 1, "If mapHiRisk=TRUE, value between 0 and 1 defining minimum risk classified as high risk"),
    defineParameter("hiRisk2", "numeric", 10.0, 0, 1, "If mapHiRisk=TRUE, value between 0 and 1 defining minimum risk classified as high risk"),
    defineParameter("mapHiRisk", "logical", TRUE, NA, NA, "Logical of whether to map hiRisk")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "riskList", objectClass = "List", desc = "List containing 'risk' rasters", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "totalRisk", objectClass = "RasterLayer", desc = "Raster of totalRisk created by combining layers in riskList"),
    createsOutput(objectName = "highRisk", objectClass = "RasterLayer", desc = "Raster made by reclassifying totalRisk. Will be empty if mapHiRisk == FALSE")
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
      sim <- scheduleEvent(sim, time(sim) + 2.5, "combineRisk", "combine") 
      
      # schedule future event(s)
      
    },
    combine = {
      # do stuff for this event
      sim <- Cache(combineRiskCombine, sim, userTags = "combineRiskCombine")
      sim <- combineRiskPlot(sim)
      # schedule future event(s)
        
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "combineRisk", "plot")
     
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:

### plot event:
combineRiskPlot <- function(sim) {
  for (i in 1:length(sim$totalRisk)) {
    clearPlot()
    browser()
    Plot(sim$totalRisk[[i]][["totalRisk"]], title = names(sim$totalRisk[i]), cols=rev(heat.colors(16))) #legendRange = 0:1
    if("water" %in% names(sim$dataList[[i]])) { #if water is a layer in dataList, add to risk maps
      Plot(sim$dataList[[i]][["water"]], addTo= "sim$totalRisk[[i]][['totalRisk']]", title = "")
      }
    }   
  return(invisible(sim))
}

### combine event: combining riskList rasters
combineRiskCombine <- function(sim) {
  sim$highRisk <- NULL #will be null by default
  
  outList <- lapply(1:length(sim$riskList), FUN = function(i, riskList = sim$riskList){
    riskList = riskList[[i]]
    if(length(riskList) == 1) { #if only one risk layer, then it is totalRisk
      totalRisk <- riskList[[1]]
    } else {  # if more than one layer, add risk layers together
      riskStack <- raster::stack(riskList)
      totalRisk <- sum(riskStack, na.rm=TRUE) } 
    totalRisk[totalRisk<0] <- 0 # if risk values below 0, set to 0
    #totalRisk <- totalRisk/max(raster::values(totalRisk), na.rm=TRUE) # scale values to 1 using max risk value
    names(totalRisk) <- c("totalRisk")
    return(totalRisk)
  })
 
  names(outList) <- paste(names(sim$dataList))
  sim$totalRisk <- outList
  
  #Map Hi Risk
  if (P(sim)$mapHiRisk == TRUE) {
    highRisk = vector(mode = "list", length = length(sim$totalRisk))
    for(i in 1:length(sim$totalRisk)) {
      
      tempRisk <- sim$totalRisk[[i]]
      if (!P(sim)$hiRisk2 < max(values(tempRisk))) {
        cat(crayon::cyan("The hi risk cut-off is higher than any values found in ", names(sim$dataList[i])))
        highRisk[[i]] <- Cache(raster::reclassify,tempRisk, 
                                            matrix(c(0, P(sim)$hiRisk1, NA, 
                                                     P(sim)$hiRisk1, P(sim)$hiRisk2, 1 ,
                                                     P(sim)$hiRisk2, P(sim)$hiRisk2, 2),
                                                   ncol=3, byrow=T),
                                            include.lowest=TRUE)
      } else {
        highRisk[[i]] <- Cache(raster::reclassify, tempRisk, 
                                            matrix(c(0, P(sim)$hiRisk1, NA, 
                                                     P(sim)$hiRisk1, P(sim)$hiRisk2, 1 ,
                                                     P(sim)$hiRisk2, max(values(tempRisk)), 2),
                                                   ncol=3, byrow=T),
                                            include.lowest=TRUE)
      }
    }
    
    sim$highRisk <- highRisk
  }
  return(invisible(sim))
}

#.inputObjects <- function(sim) {



