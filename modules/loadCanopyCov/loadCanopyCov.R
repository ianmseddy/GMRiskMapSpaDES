
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "loadCanopyCov",
  description = NA, #"loads Global Canopy Cover dataset originally from (https://landcover.usgs.gov/glc/TreeCoverDescriptionAndDownloads.php) ",
  keywords = NA, # c("Global Canopy Cover"),
  authors = person("Brian", "Van Hezewijk", email = "brian.vanhezewijk@canada.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1", loadCanopyCov = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "loadCanopyCov.Rmd"),
  reqdPkgs = list("raster", "quickPlot"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("usePlot", "logical", TRUE, NA, NA, "Should module include plotting"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "globCanCov", objectClass = "RasterLayer", desc = "Raw Global Canopy Cov dataset"),
    expectsInput(objectName = "dataListInit", objectClass = "List", desc = "List of data rasters. If NULL, recreates an empty list in .inputsObjects", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.loadCanopyCov = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      # do stuff for this event
      sim <- loadCanopyCovInit(sim)

    },
    plot = {
      # do stuff for this event
      
      if (P(sim)$usePlot) {
        loadCanopyCovPlot(sim) 
      }
      # schedule future event(s)
     
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions

### init event:
loadCanopyCovInit <- function(sim) {
  
  return(invisible(sim))
}

### plot event:
loadCanopyCovPlot <- function(sim) {
  
  Plot(sim$dataListInit[["globCanCov"]], title = "Percent Canopy Cover")
  
  return(invisible(sim))
}

### format event: formatting lcc dataset

.inputObjects <- function(sim) {
  
  # create sim$dataListInit if it doesn't exist
  if(is.null(sim$dataListInit)){
    sim$dataListInit <- list()
  }
  
  # import lcc dataset
  if("GlobalCanopyCover_swBC_crop.tif" %in% list.files(file.path(modulePath(sim),"loadCanopyCov","data"))) {
    sim$dataListInit[["treeCover"]] <- raster::raster(file.path(modulePath(sim),"loadCanopyCov","data","GlobalCanopyCover_swBC_crop.tif"))
  } else {
    stop("loadCanopyCov: There is no Canopy Cover dataset provided") }
  
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
