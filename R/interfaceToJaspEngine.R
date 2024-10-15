
interfaceToJaspEngine <- list2env(list(
  .setLog                        = NA,
  .setRError                     = NA,
  .crashPlease                   = NA,
  .setRWarning                   = NA,
  .runSeparateR                  = NA,
  .returnString                  = NA,
  .columnIsScale                 = NA,
  .callbackNative                = NA,
  .dataSetRowCount               = NA,
  .returnDataFrame               = NA,
  .columnIsOrdinal               = NA,
  .columnIsNominal               = NA,
  .encodeColNamesLax             = NA,
  .decodeColNamesLax             = NA,
  .encodeColNamesStrict          = NA,
  .decodeColNamesStrict          = NA,
  .setColumnDataAsScale          = NA,
  .readFullDatasetToEnd          = NA,
  .allColumnNamesDataset         = NA,
  .readDatasetToEndNative        = NA,
  .readFilterDatasetToEnd        = NA,
  .setColumnDataAsOrdinal        = NA,
  .setColumnDataAsNominal        = NA,
  .readDataSetHeaderNative       = NA,
  .createCaptureConnection       = NA,
  .postProcessLibraryModule      = NA,
  .requestTempFileNameNative     = NA,
  .requestTempRootNameNative     = NA,
  .readDataSetRequestedNative    = NA,
  .requestStateFileNameNative    = NA,
  .readFullFilteredDatasetToEnd  = NA,
  .requestSpecificFileNameNative = NA
))

# for jaspTools
setJaspInterface <- function(...) {
  dots <- list(...)
  keys <- names(dots)
  for (key in keys)
    assign(key, dots[[key]], envir = interfaceToJaspEngine)
}

runThroughInterface <- function(key, ...) {
  obj <- mget(key, envir = interfaceToJaspEngine, ifnotfound = NA)

  if (is.na(obj))
    stop("Tried to look up the key ", key, " in interfaceToJaspEngine but it has not been set by jaspEngine or does not exist!")

  if (is.function(obj))
    return(obj(...))

  return(obj)

}
