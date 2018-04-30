library(stringr)
library(readr)
library(ggplot2)

#' Read LAS and split into raw sections
#' Intended to be used interally by the las function
#'
#' @param las_dat
#'
#' @return list. Raw sections of the las file.
#' @export
#'
#' @examples
sectionParser = function(las_dat){

  sections = str_split(las_dat, pattern='~', simplify = TRUE)

  # optional sections
  parameters = sections[grepl(pattern = '^P', x = sections)]
  if (length(parameters) == 0)
    parameters = NULL

  other = sections[grepl(pattern = '^O', x = sections)]
  if (length(other) == 0)
    other = NULL

  list(
    # VERSION INFORMATION (Required)
    las_version = sections[grepl(pattern = '^V', x = sections)],
    # WELL INFORMATION (Required)
    well = sections[grepl(pattern = '^W', x = sections)],
    # CURVE INFORMATION (Required)
    curves = sections[grepl(pattern = '^C', x = sections)],
    # PARAMETER INFORMATION (Optional)
    parameters = parameters,
    # OTHER (Optional)
    other = other,
    # ASCII LOG DATA (Required)
    log_data = sections[grepl(pattern = '^A', x = sections)]
  )
}


#' Read LAS file version information
#'
#' @param version_dat
#'
#' @return
#' @export
#'
#' @examples
versionParser = function(version_dat){
  # split using first dot in line,
  # first space in line,
  # and last colon in line

  version_lines = str_split(version_dat, '\n', simplify = TRUE) # split lines
  version_lines = version_lines[2:length(version_lines)] # remove header line
  version_lines = version_lines[version_lines != ''] # remove empty lines

  version_info = data.frame()

  for (line in version_lines){
    identifier = str_split(line, '\\.', simplify = TRUE, n = 2)[1] # split on first dot
    after_dot_space = str_split(line, '\\. ', simplify = TRUE)[2] # split on first space after dot
    identifier_val = str_split(after_dot_space, ':', n=2, simplify = TRUE)[1]
    val = str_split(after_dot_space, ':', n=2, simplify = TRUE)[2]
  }
}


#' Parse curve data
#'
#' @param curves
#'
#' @return
#' @export
#'
#' @examples
curveParser = function(curves){

  curve_lines = str_split(curves, '\n', simplify = TRUE) # split lines
  curve_lines = curve_lines[2:length(curve_lines)] # remove header line
  curve_lines = curve_lines[curve_lines != ''] # remove empty lines

  mnem = str_split(curve_lines, '[.]', n = 2, simplify = TRUE)[, 1] # split on first . delimiter for mnemonic and retrain mnen
  mnem = str_replace(mnem, '[:space:]+', '')

  units = str_split(curve_lines, '[.]', n = 2, simplify = TRUE)[, 2] # split on delimiter for mneumonic and take units
  units = str_split(units, '[:space:]', simplify = TRUE)[, 1]

  api = str_extract(curve_lines,
                    '[:digit:]{2} [:digit:]{3} [:digit:]{2} [:digit:]{2}') # extract api based on 00 000 00 00 digits
  description = str_split(curve_lines, '[:]', n = 2, simplify = TRUE)[, 2] %>% str_replace('^ ', '') # split on : and keep description

  return(data.frame(mnem, units, api, description))
}


#' Parse log data
#'
#' @param log_data
#' @param curves
#' @param nodata
#'
#' @return
#' @export
#'
#' @examples
logdataParser = function(log_data, curves, nodata){

  log_lines = str_split(log_data, '\n', simplify = TRUE) # split lines
  log_lines = log_lines[2:length(log_lines)] # remove header line
  log_lines = str_replace(log_lines, '^[:space:]+', '') # remove leading spaces

  data_df = data.frame(str_split(log_lines, '[:space:]+', simplify = TRUE)) # split columns based on spaces

  # convert dataframe to numeric values
  data_df[] = lapply(data_df, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })

  data_df = setNames(data_df, curves$mnem)
  #row.names(data_df) = data_df[[1]]

  data_df[data_df == nodata] = NA

  return(data_df)
}


#' Parse well data
#'
#' @param well_info
#'
#' @return
#' @export
#'
#' @examples
wellParser = function(well_info){
  well_lines = str_split(well_info, '\n', simplify = TRUE) # split lines
  well_lines = well_lines[2:length(well_lines)] # remove header line
  well_lines = str_replace(well_lines, '^[:space:]+', '') # remove leading spaces
  well_lines = well_lines[well_lines != ''] # remove trailing lines

  mnemonics = str_split(well_lines, '[:space:]', n = 2, simplify = TRUE)[,1]

  value = str_split(well_lines, '[:]', n = 2, simplify = TRUE)[,1]
  value = str_split(value, '[.]', n = 2, simplify = TRUE)[,2]
  value = str_split(value, '[:space:]', n = 2, simplify = TRUE)[,2] %>% str_trim()

  description = str_split(well_lines, '[:]', n = 2, simplify = TRUE)[,2] %>% str_trim()

  well_data = data.frame(mnemonics, value, description)

  well_info = list(
    STRT.M = as.numeric(as.character(well_data[well_data$mnemonics == 'STRT.M', 'value'])),
    STOP.M = as.numeric(as.character(well_data[well_data$mnemonics == 'STOP.M', 'value'])),
    STEP.M = as.numeric(as.character(well_data[well_data$mnemonics == 'STEP.M', 'value'])),
    nodata = as.numeric(as.character(well_data[well_data$mnemonics == 'NULL.', 'value'])),
    COMP = as.character(well_data[well_data$mnemonics == 'COMP.', 'value']),
    FLD = as.character(well_data[well_data$mnemonics == 'FLD', 'value']),
    DATE = as.character(well_data[well_data$mnemonics == 'DATE.', 'value']),
    WELL = as.character(well_data[well_data$mnemonics == 'WELL.', 'value']),
    LABL = as.character(well_data[well_data$mnemonics == 'LABL', 'value']),
    APIN = as.character(well_data[well_data$mnemonics == 'APIN.', 'value']),
    UWI = as.character(well_data[well_data$mnemonics == 'UWI', 'value']),
    OPER = as.character(well_data[well_data$mnemonics == 'OPER.', 'value'])
  )

  return(well_info)
}


parametersParser = function(param_dat){

}


otherParser = function(other_dat){

}


#' S3 Class for LAS data
#'
#' @param las_file
#'
#' @return
#' @export
#'
#' @examples
las = function(las_file){

  # read las
  las_dat = readr::read_file(lasfile)

  # parse data
  sections = sectionParser(las_dat)
  las_version = versionParser(sections$version)
  well_info = wellParser(sections$well)
  curves = curveParser(sections$curves)
  log_data = logdataParser(
    log_data=sections$log_data, curves=curves, nodata=well_info$nodata)

  structure(
    list(
      raw_sections = sections,
      las_version = las_version,
      well_info = well_info,
      curves = curves,
      log_data = log_data
    ),
    class = 'LAS'
  )
}


#' Add new log curve and curve information
#'
#' @param las
#' @param data
#'
#' @return
#' @export
#'
#' @examples
addCurve = function(las, data){
}


# Generic functions
#' Plot LAS curve data
#'
#' @param las
#'
#' @return
#' @export
#'
#' @examples
plot.LAS = function(las){
  ggplot(data=las$log_data, aes(TVD)) +
    geom_line(aes(y=GR)) +
    coord_flip() +
    scale_x_reverse()
}


write.LAS = function(las){
}
