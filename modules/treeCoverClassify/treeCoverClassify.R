
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "treeCoverClassify",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2.9002", treeCoverClassify = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "treeCoverClassify.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("coniferousClass", "numeric", 210, NA, NA, "LCC class(es) of coniferous forest"),
    defineParameter("deciduousClass", "numeric", 220, NA, NA, "LCC class(es) of deciduous forest")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "lccLegend", objectClass = "data.frame", desc = "AAFC 2015 land cover code and label legend", sourceURL = NA),
    expectsInput(objectName = "dataList", objectClass = "List", desc = "List of lists (each an ROI) of data containing 'lcc' dataset", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "dataList", objectClass = "List", desc = "List of lists containing tree cover rasters distinguished by coniferous, deciduous, mixed, and undetermined"))
))

## event types
#   - type `init` is required for initialiazation

doEvent.treeCoverClassify = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      sim <- scheduleEvent(sim, time(sim) + 1, "treeCoverClassify", "reclassify")
    },

    reclassify = {
      
      sim <- reclassifyForestCover(sim)

      
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
reclassifyForestCover <- function(sim) {
 
  outList <- lapply(sim$dataList, function(dataList, 
                                           decClass = P(sim)$deciduousClass, 
                                           conClass = P(sim)$coniferousClass) {
    #x is now a single dataList containing LCC and Forest Cover data
    browser()
    landCover <- dataList[["lcc"]] #a  cropped tif
    treeCover <- dataList[["treeCover"]] #a cropped tif
    
    treeValues <- Cache(getValues, treeCover)
    landValues <- Cache(getValues, landCover)
    
    if (length(decClass) > 1){
      #Using inverse mask is fastest, otherwise do this method
      decidLand <- landValues[landValues %in% decClass] 
      decidValues <- treeValues
      decidValues[!decidLand] <- NA 
      decidTrees <- setValues(treeCover, treeValues)
    } else {
      decidTrees <- Cache(mask, treeCover, mask = landCover, maskvalue = decClass, inverse = TRUE)
    }  
    
    if (length(conClass) > 1) {
      conifLand <- landValues[landValues %in% conClass] 
      conifValues <- treeValues #make copy of tree cover to edit
     conifValues[!conifLand] <- NA 
      conifTrees <- setValues(treeCover, conifValues)
    } else {
      conifTrees <- Cache(mask, treeCover, mask = landCover, maskvalue = conClass, inverse = TRUE)
    }
    #Check that you don't need a mixed class before you change treeValues, or make a copy of TreeValues
    #Unclassified trees
    treeValues <- Cache(getValues, treeCover)
    treeValues[landValues %in% c(decClass)] <- NA
    treeValues[landValues %in% c(conClass)] <- NA
    otherTrees <- setValues(treeCover, as.vector(treeValues))
    
    #what's left should be tree cover values that were not classified as coniferous, deciduous, mixed, whatever
    
    #add new layers to dataList and remove old ones
    dataList[["conifTreeCover"]] <- conifTrees
    dataList[["decidTreeCover"]] <- decidTrees
    dataList[["otherTreeCover"]] <- otherTrees
    dataList$treeCover <- NULL
    dataList$lcc <- NULL
    #Issues. 1. You have removed areas classified as forest in LCC but NA in forestCover
    #There is a large amount of area that is forest
    
    return(dataList)  
  })
  
  #anythig you change in datalist has to correspond to risk values, so don't overwrite anything.... 
  names(outList) <- names(sim$ROI)
  
  sim$dataList <- outList
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
