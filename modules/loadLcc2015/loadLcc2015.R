
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "loadLcc2015",
  description = "Module imports the AAFC 2015 landCover dataset",
  keywords = c("lcc", "tree cover"),
  authors = person("Kaitlyn", "Schurmann", email = "kdschurmann@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9001", loadLcc2015 = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "loadLcc2015.Rmd"),
  reqdPkgs = list("raster", "quickPlot"),
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
    expectsInput(objectName = "lcc", objectClass = "RasterLayer", desc = "Raw AAFC 2015 landcover dataset"),
    expectsInput(objectName = "dataListInit", objectClass = "List", desc = "List of data rasters. If NULL, recreates an empty list in .inputsObjects", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataListInit", objectClass = "List", desc = "List of data containing 'lcc' dataset")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.loadLcc2015 = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- loadLcc2015Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "loadLcc2015", "format")
      #sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "loadLcc2015", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "loadLcc2015", "save")
    },
    plot = {
      # do stuff for this event
      
      loadLcc2015Plot(sim) 
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "loadLcc2015", "plot")
    },
    save = {
      # do stuff for this event

      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "loadLcc2015", "save")

    },
    format = {
      # do stuff for this event

      sim <- loadLcc2015Format(sim)

      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + increment, "loadLcc2015", "format")

    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## Event Functions ##

### init event:
loadLcc2015Init <- function(sim) {

  return(invisible(sim))
}

### plot event:
loadLcc2015Plot <- function(sim) {

  Plot(sim$dataListInit[["lcc"]], title = "AAFC Land Cover Classification 2015")

  return(invisible(sim))
}

### format event: formatting lcc dataset
loadLcc2015Format <- function(sim) {
  
  return(invisible(sim))
}


.inputObjects = function(sim) {
  
  # import lcc dataset
  if("ACGEO_2015_CI_BC_30m_v1_crop.tif" %in% list.files(file.path(modulePath(sim),"loadLcc2015","data"))) {
    sim$dataListInit[["lcc"]] <- raster::raster(file.path(modulePath(sim),"loadLcc2015","data","ACGEO_2015_CI_BC_30m_v1_crop.tif"))
  } else {
    stop("loadLcc2015: There is no AAFC 2015 land cover dataset provided") }
  
  # ###### import land cover class legend
  # if(is.null(sim$lccLegend)){
  #   if("aci_crop_classifications_iac_classifications_des_cultures.csv" %in% list.files(file.path(modulePath(sim),"loadLcc2015","data"))) {
  #     lccLegend <- read.csv(file.path(modulePath(sim),"loadLcc2015","data","aci_crop_classifications_iac_classifications_des_cultures.csv"))
  #     names(lccLegend)[2] <- "Label"
  #     sim$lccLegend <- lccLegend
  #   } else {
  #     stop("loadLcc2015: There is no AAFC 2015 landcover classification legend provided.")
  #   } 
  # }
  
  return(invisible(sim))
}
