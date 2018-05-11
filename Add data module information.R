# Creating new loadData module

  ## Raster object
# - .importObjects to load Raster dataset to sim$dataListInit[[name]]
# - the name (sim$dataListInit[[name]]) assigned to the object will be used by other modules in the simulation
# - object name must be added to 'SpeciesRiskParams.csv' file (located in "~/modules/calculateRisk/data" folder)
#     species: species the values will be used for
#     layer: layer name must match object name
#     func: pointer to the function where the parameter is used (translateRisk or calculateRiskTranslate in calculateRisk module)
#     parameter: defines value (see "~/modules/calculateRisk/R/translateRisk" for parameter definitions)
#     value: value must be entered for each parameter
    

  ## Spatial object
# - .importObjects to load Spatial*DataFrame dataset to sim$dataListInit[[name]]
# - the name (sim$dataListInit[[name]]) assigned to the object will be used by other modules in the simulation
# - format event to add/delete/clean/format data columns
# - the data column that is to be translated to risk must in first column of data.frame or the column name must match the object name
#     Ex1: traps SpatialPointsDataFrame contains columns named: Name, Description, traps, ID...the "traps" column will be translated into risk
#     Ex2: traps SpatialPointsDataFrame contains columns named: Name, Description, ID...the "Name" column will be translated into risk (must be numeric)
# - Traps objects must have a column named "ID" contained unique identifier for each feature (used in trapsReportPDF module)
# - object name must be added to 'SpeciesRiskParams.csv' file (located in "~/modules/calculateRisk/data" folder)
#     species: species the values will be used for
#     layer: layer name must match object name
#     func: pointer to the function where the parameter is used (translateRisk or calculateRiskTranslate in calculateRisk module)
#     parameter: defines value (see "~/modules/calculateRisk/R/translateRisk" for parameter definitions)
#     value: value must be entered for each parameter

