#' Write an LAS object to a new LAS file
#'
#' @param las LAS object
#' @param filename character. File path
#'
#' @return
#' @export
#'
#' @examples
writeLAS = function(las, filename) {

  filename = '/Users/steven/GitHub/lasR/testout.las'

  # Version Information
  write_lines(versionWriter(las$las_version), path = filename, append = FALSE)

  # Well Information
  write_lines(wellWriter(las$well), path = filename, append = TRUE)
}


#' Creates a character vector of the LAS Version Information Section
#'
#' Generates a character vector of the LAS Version Information Section, with
#' each new line as a separate element. This can be written to a file
#' using standard IO functions
#'
#' @param las_version data.frame. Containing parsed LAS version information
#'
#' @return character.
#' @export
#'
#' @examples
versionWriter = function(las_version) {

  # Header
  out = paste0('~Version Information')

  # Create mnen.unit data : description block
  for (i in 1:nrow(las_version)) {
    version_line = paste0(
      as.character(las_version[i, 1]), '.',
      as.character(las_version[i, 2]), ' ',
      as.character(las_version[i, 3]), ' : ',
      as.character(las_version[i, 4]))

    out = append(out, version_line)
  }

  out = append(out, '#')

  return (out)
}


wellWriter = function(well) {

  # Header
  out = '~Well Information'
  out = append(out, paste('#MNEM.UNIT', 'DATA', 'DESCRIPTION', sep = '\t'))

  # Create mnen.unit data : description block
  for (i in seq_along(well)) {
    well_line = paste0(
      as.character(names(well)[i]), '.',
      as.character(well[[i]]['unit']), ' ',
      as.character(well[[i]]['data']), ' : ',
      as.character(well[[i]]['description']))

    out = append(out, well_line)
  }

  out = append(out, '#')

  return(out)
}
