
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "lccToTreeCover",
  description = "Reclassify AAFC 2015 landcover to tree cover ", 
  keywords = c("lcc","tree cover"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", lccToTreeCover = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "lccToTreeCover.Rmd"),
  reqdPkgs = list("raster", "reproducible", "quickPlot"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "lccLegend", objectClass = "data.frame", desc = "AAFC 2015 land cover code and label legend", sourceURL = NA),
    expectsInput(objectName = "dataList", objectClass = "List", desc = "List of lists (each an ROI) of data containing 'lcc' dataset", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataList", objectClass = "List", desc = "List of data containing 'treeCover' and 'water' datasets")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.lccToTreeCover = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      sim <- lccToTreeCoverInit(sim)
      
      sim <- scheduleEvent(sim, start(sim)+ 1, "lccToTreeCover", "reclass")

    },
   
    reclass = {
      # do stuff for this event
      sim <- Cache(lccToTreeCoverReclassify, sim)
      
      # schedule future event(s)
      if(!is.na(P(sim)$.plotInitialTime)) {
        lccToTreeCoverPlot(sim)
        }

      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
lccToTreeCoverInit <- function(sim) {
  
  return(invisible(sim))
}


### plot event:
lccToTreeCoverPlot <- function(sim) {
  for (i in 1:length(sim$dataList)) {
    clearPlot()
    Plot(sim$dataList[[i]][["treeCover"]], title = names(sim$dataList[[i]][["treeCover"]]))
    Plot(sim$dataList[[i]][["water"]], addTo = 'sim$dataList[[i]][["treeCover"]]', title = "")
  }
  return(invisible(sim))
}

### reclass event: reclassifies lcc to treeCover
lccToTreeCoverReclassify <- function(sim) {

  newList <- lapply(1:length(sim$dataList), FUN = lccToTreeCoverlccReclass, dataList = sim$dataList, 
                   legend = sim$lccLegend)

  names(newList) <- names(sim$dataList)
  sim$dataList <- newList
  
  return(invisible(sim))
}

lccToTreeCoverlccReclass <- function(i, dataList, legend){

  if( "lcc" %in% names(dataList[[i]]) ) {
    #you took Init off sim here. investigate
    ## classify water layer
    water <- Cache(raster::setValues, dataList[[i]][["lcc"]], 
                   ifelse(raster::values(dataList[[i]][["lcc"]])!=20, NA, 0))
    names(water) <- c('water') 
    quickPlot::setColors(water) <- "lightblue"
    dataList[[i]][["water"]] <- water
    
    ## classify treeCover layer
    temp <- list()
    for(ii in strsplit("Shrubland,Nursery,Forest (undifferentiated),Coniferous,Broadleaf,Mixedwood",split = ",")){
      temp[ii] <- legend$Code[charmatch(ii, legend$Label)]
    }
    
    temp.df <- data.frame(legend$Code)
    temp.df$Reclass <- ifelse(temp.df[,1] %in% temp, 1, 0)
    
    treeCover <- Cache(raster::subs, x=dataList[[i]][["lcc"]], y=temp.df)
    treeCover <- Cache(mask, treeCover, dataList[[i]][["lcc"]])
    names(treeCover) <- c('treeCover')
    dataList[[i]][["treeCover"]] <- treeCover
    dataList[[i]][["lcc"]] <- NULL
    
  } else {
    browser()
    message("no lcc dataset provided for ", names(dataList[i]), ". Cannot be reclassified.")  
    }
  
  return(dataList[[i]])
  
}
  
.inputObjects <- function(sim) {
  
  ###### import lcc dataset
  #if(is.null(sim$dataList[["lcc"]])) {
  #  if("ACGEO_2015_CI_BC_30m_v1_crop.tif" %in% list.files(file.path(modulePath(sim),"lccToTreeCover","data"))) {
  #    lcc <- raster::raster(file.path(modulePath(sim),"lccToTreeCover","data","ACGEO_2015_CI_BC_30m_v1_crop.tif"))
  #    sim$dataListInit[["lcc"]] <- lcc
  #  } else {
  #    stop("lccToTreeCover: There is no AAFC 2015 landcover dataset provided.")
  #    }
  #}
  
  ###### import land cover class legend
  if(is.null(sim$lccLegend)){
    if("aci_crop_classifications_iac_classifications_des_cultures.csv" %in% list.files(file.path(modulePath(sim),"loadLcc2015","data"))) {
      lccLegend <- read.csv(file.path(modulePath(sim),"loadLcc2015","data","aci_crop_classifications_iac_classifications_des_cultures.csv"))
      colnames(lccLegend) <- c("Code", "Label", "ColourR", "ColourG", "ColourB")
      sim$lccLegend <- lccLegend
    } else {
      stop("loadLcc2015: There is no AAFC 2015 landcover classification legend provided.")
    } 
  }
  
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
