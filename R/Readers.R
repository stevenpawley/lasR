#' Read LAS and split into raw sections
#' Parses a character vector representing a raw LAS file and splits it into
#' sections based on the header lines. Intended to be used interally by the las
#' function
#'
#' @param las_dat character. Raw LAS file as a character vector
#'
#' @return list. Raw sections of the las file.
#' @export
#'
#' @examples
sectionParser = function(las_dat){

  # split sections based on ~ separator
  sections = str_split(las_dat, pattern='~', simplify = TRUE)

  # check for the presence of optional sections
  parameters = sections[grepl(pattern = '^P', x = sections)]
  if (length(parameters) == 0)
    parameters = NULL

  other = sections[grepl(pattern = '^O', x = sections)]
  if (length(other) == 0)
    other = NULL

  # return sections as a named list
  list(
    # Version Information (Required)
    las_version = sections[grepl(pattern = '^V', x = sections)],
    # Well Information (Required)
    well = sections[grepl(pattern = '^W', x = sections)],
    # Curve Information (Required)
    curves = sections[grepl(pattern = '^C', x = sections)],
    # Parameter Information (Optional)
    parameters = parameters,
    # Other (Optional)
    other = other,
    # ASCII Log Data (Required)
    log_data = sections[grepl(pattern = '^A', x = sections)]
  )
}


#' Splits a raw section into lines for further parsing
#'
#' Intended to be used internally by the las function
#'
#' @param dat character. A raw LAS section
#'
#' @return character with section split into separate lines
#' @export
#'
#' @examples
splitSectionData = function(dat) {

  # Split a raw section into lines for further parsing
  chr_lines = str_replace_all(dat, '\r\n', '\n') # unify line endings
  chr_lines = str_split(chr_lines, '\n', simplify = TRUE) # split lines
  chr_lines = chr_lines[2:length(chr_lines)] # remove header line
  chr_lines = chr_lines[grepl('^#', chr_lines) == FALSE]
  chr_lines = chr_lines[chr_lines != ''] # remove empty lines

  return(chr_lines)
}


#' Read LAS file version information
#'
#' @param version_dat character. Character vector containing the version section
#' of a LAS file.
#'
#' @return
#' @export
#'
#' @examples
versionParser = function(version_dat){
  # Parsing procedure
  # 1. Split character vector into lines
  #   a. Split character vector into lines using newline separator
  #   b. Remove the first line (the header)
  #   c. Remove any empty lines
  #
  #. 2. Parse into a dataframe
  #   a. Each line is split on first dot (identifier),
  #   b. Before last colon (value)
  #   c. After lasdt colon (description)

  # (1) Split the raw section data into lines
  version_lines = splitSectionData(dat = version_dat)

  # (2) Parse into dataframe
    if (length(version_lines) > 0) {

    # a. Split on first dot and take [,1]
    identifier = str_split(version_lines, '\\.', simplify = TRUE)[,1] %>%
      str_trim()

    # b. Split on first space after dot
    after_dot_space = str_split(version_lines, '\\. ', simplify = TRUE)[, 2]

    # c. Then separate into two fields based on colon separator
    value = str_split(after_dot_space, ':', n=2, simplify = TRUE)[, 1] %>%
      str_trim()
    val = str_split(after_dot_space, ':', n=2, simplify = TRUE)[, 2] %>%
      str_trim()

    } else {
      identifier = NULL
      value = NULL
      val = NULL
    }

  return(data.frame(identifier, value, val))
}


#' Parse curve data
#'
#' @param curves_dat
#'
#' @return
#' @export
#'
#' @examples
curveParser = function(curves_dat){
  # Parsing procedure
  # 1. Split character vector into lines
  #   a. Split character vector into lines using newline separator
  #   b. Remove the first line (the header)
  #   c. Remove any empty lines
  #
  #. 2. Parse curve information into a dataframe. For each line:
  #   a. Split on first dot: before the dot = the mnenmonic
  #   b. Split on first dot: after dot but before space = units
  #   c. Extract API values based on pattern matching
  #   d. Split on colon: after the colon = the description

  # (1) Split the raw section data into lines
  curve_lines = splitSectionData(curves_dat)

  # remove any spaces before the first dot
  curve_lines = str_replace(curve_lines, '[..]', '.')
  curve_lines = str_remove(curve_lines, '^[:space:]+')
  curve_lines = str_replace(curve_lines, '[:space:]+[.]', '.')

  # (2) Parse curve information into a dataframe
  if (length(curve_lines) > 0) {

    # a. Split on first dot: before the dot = the mnemonic
    mneumonics = str_split(curve_lines, '[.]', n = 2, simplify = TRUE)[, 1] %>%
      str_trim()

    # b. Split on first dot: after the dot but before first space = units
    units = str_split(curve_lines, '[.]', n = 2, simplify = TRUE)[, 2]
    units = str_split(units, '[:space:]', simplify = TRUE)[, 1]

    # c. Extract api based on 00 000 00 00 digits pattern
    api = str_extract(curve_lines,
                      '[:digit:]{2} [:digit:]{3} [:digit:]{2} [:digit:]{2}')

    # d. Split on colon: after the colon = the description
    description = str_split(curve_lines, '[:]', n = 2, simplify = TRUE)[, 2] %>%
      str_trim()

  } else {
    mneumonics = NULL
    units = NULL
    api = NULL
    description = NULL
  }

  return(data.frame(mneumonics, units, api, description))
}


#' Parse log data
#'
#' @param log_dat character. Raw section containing well log curve data
#' @param curves dataframe. Curve information
#' @param nodata numeric. Nodata value read from the well information section
#'
#' @return dataframe. Well log data.
#' @export
#'
#' @examples
logdataParser = function(log_dat, curves, nodata, wrap){
  # Parsing procedure
  # 1. Split character vector into lines
  #   a. Split character vector into lines using newline separator
  #   b. Remove the first line (the header)
  #   c. Remove any empty lines
  #
  #. 2. Parse curve information into a dataframe. For each line:
  #   a. Split columns based on any number of spaces and create dataframe
  #   b. Convert dataframe into numeric values
  #   c. Extract API values based on pattern matching
  #   d. Set nodata values to NA

  # (1) Split the raw section data into lines
  log_lines = splitSectionData(log_dat)
  log_lines = str_trim(log_lines)

  # (2) Parse well log data into a dataframe
  if (length(log_lines) > 0) {

    # a. Split columns based on spaces
    if (wrap == TRUE) {

      # Reshape log data into column-wise vector
      log_df = log_lines %>% str_split('[:space:]+', simplify = TRUE) %>%
        data.frame(stringsAsFactors = F)
      log_df = as.vector(t(log_df))
      log_df = log_df[log_df != ""]

      # Dataframe to store reshaped log data
      data_df = as.data.frame(
        matrix(ncol = length(log_df) / nrow(curves), nrow = nrow(curves)),
        stringsAsFactors = F)

      # Reshape log data into new dataframe
      data_df[] = log_df
      data_df = t(data_df)

    } else {
      data_df = data.frame(
        str_split(log_lines, '[:space:]+', simplify = TRUE),
        stringsAsFactors = F)
    }

    # b. Convert dataframe to numeric values
    data_df = as.data.frame(apply(data_df, c(1,2), as.numeric))

    # c. Set column names of dataframe based on curve information
    data_df = setNames(data_df, curves$mneumonics)
    rownames(data_df) = data_df[, 1]

    # d. Set nodata values to NA
    data_df[data_df == nodata] = NA

  } else {
    data_df = NULL
  }

  return(data_df)
}


#' Parse well data
#'
#' @param well_dat character. Raw section for well information
#'
#' @return list. List of well section information
#' @export
#'
#' @examples
wellParser = function(well_dat){
  # Parsing procedure
  # 1. Split character vector into lines
  #   a. Split character vector into lines using newline separator
  #   b. Remove the first line (the header)
  #   c. Remove any empty lines
  #
  # 2. Parse well information into a list For each line:
  #   a. Split on the first space: before space = mnemonic and unit
  #   b. Split after the space on the colon: before colon = value
  #   c. Split after the space on the colon: after colon = description
  #   d. Create a named list

  # (1) Split the raw section data into lines
  well_lines = splitSectionData(well_dat)

  # remove any spaces before the first dot
  well_lines = str_replace(well_lines, '[..]', '.')
  well_lines = str_remove(well_lines, '^[:space:]+')
  well_lines = str_replace(well_lines, '[:space:]+[.]', '.')

  # (2) Parse well information into a list
  if (length(well_lines) > 0) {

    # a. Split on first space: before space = mnemonic and unit
    space_divider = str_split(well_lines, '[:space:]', n = 2, simplify = TRUE)
    mnemonics_and_unit = space_divider[, 1] %>% str_trim()
    mnemonics = str_split(mnemonics_and_unit, '[.]', simplify = T)[, 1] %>%
      str_trim()
    mnemonics[mnemonics == 'NULL'] = 'nodata'
    units = str_split(mnemonics_and_unit, '[.]', simplify = T)[, 2] %>% str_trim()

    # b. Split after the space on the colon: before colon = value
    colon_divider = str_split(space_divider[, 2], '[:]', n = 2, simplify = TRUE)
    value = colon_divider[, 1] %>% str_replace('^[.]', '') %>% str_trim()

    # c. Split after the space on the colon: after colon = description
    description = colon_divider[, 2] %>%
      str_trim()

    # d. Create named list
    well_info = list()
    for (i in seq_along(mnemonics)) {
      well_info[[paste0(mnemonics[[i]])]] = list(
        unit=units[[i]], value=value[[i]], description=description[[i]])
    }

    # e. Convert numeric-like values to numerics
    for (i in seq_along(well_info)) {
      col = well_info[[i]]['value']
      if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
        well_info[[i]]['value'] = as.numeric(as.character(col))
      }
    }
  } else {
    well_info = NULL
  }

  return(well_info)
}


#' Parse the parameter section of an LAS ile
#'
#' @param param_dat character. Raw parameter section
#'
#' @return dataframe. Parameter information
#' @export
#'
#' @examples
parametersParser = function(param_dat){
  # Parsing procedure
  # 1. Split character vector into lines
  #   a. Split character vector into lines using newline separator
  #   b. Remove the first line (the header)
  #   c. Remove any empty lines
  #
  # 2. Same parsing procedure as well information section:
  #   a. Split on the first space: before space = mnemonic and unit
  #   b. Split after the space on the colon: before colon = value
  #   c. Split after the space on the colon: after colon = description
  #   d. Create a named list

  # (1) Split the raw section data into lines
  param_lines = splitSectionData(param_dat)

  if (length(param_lines) > 0) {

    # (2) Parse well information into a dataframe
    # a. Split on first space: before space = mnemonic and unit
    space_divider = str_split(param_lines, '[:space:]', n = 2, simplify = TRUE)
    mnemonics_and_unit = space_divider[, 1] %>% str_trim()
    mnemonics = str_split(mnemonics_and_unit, '[.]', simplify = T)[, 1] %>%
      str_trim()
    units = str_split(mnemonics_and_unit, '[.]', simplify = T)[, 2] %>%
      str_trim()

    # b. Split after the space on the colon: before colon = value
    colon_divider = str_split(space_divider[, 2], '[:]', n = 2, simplify = TRUE)
    value = colon_divider[, 1] %>% str_replace('^[.]', '') %>% str_trim()

    # c. Split after the space on the colon: after colon = description
    description = colon_divider[, 2] %>% str_trim()

  } else {
    mnemonics=NULL
    units=NULL
    value=NULL
    description=NULL
  }

  return(data.frame(mnemonics, units, value, description))
}
