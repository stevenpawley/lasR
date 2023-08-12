#' S3 Class for LAS data
#'
#' @param las_file
#'
#' @return
#' @export
#'
#' @examples
las <- function(las_file) {
  # read las
  las_dat <- readr::read_file(las_file)

  # parse section and las version
  sections <- sectionParser(las_dat)

  if (length(sections$las_version) > 0) {
    las_version <- versionParser(version_dat = sections$las_version)
  } else {
    stop(paste("Version Information Section not recognized in file", las_file))
  }

  # check for wrapping
  if (las_version[las_version$mnemonics %in% "WRAP", "data"] == "YES") {
    wrap <- TRUE
  } else {
    wrap <- FALSE
  }

  # parse remaining sections
  if (length(sections$well) > 0) {
    well_info <- wellParser(well_dat = sections$well)
  } else {
    well_info <- NULL
    warning(paste("Well Information Section not recognised in file", las_file))
  }

  if (length(sections$curves) > 0) {
    curves <- curveParser(curves_dat = sections$curves)
  } else {
    curves <- NULL
    warning(paste("Curve Information Section not recognized in file", las_file))
  }

  if (length(sections$log_data) > 0) {
    log_data <- logdataParser(
      log_dat = sections$log_data,
      curves = curves,
      nodata = well_info$nodata$data,
      wrap = wrap
    )
  } else {
    log_data <- NULL
    warning(paste("ASCII Log Data Section not recognised in file", las_file))
  }

  if (length(sections$parameters) > 0) {
    parameters <- parametersParser(param_dat = sections$parameters)
  } else {
    parameters <- NULL
    warning(paste("Parameter Information Section not recognised in file", las_file))
  }

  # return as LAS S3 object
  structure(
    list(
      file = las_file,
      raw_sections = sections,
      las_version = las_version,
      well = well_info,
      curves = curves,
      log_data = log_data,
      wrapped = wrap
    ),
    class = "LAS"
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
addCurve <- function(las, data) {
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
plot.LAS <- function(las, depth.col, curves, depth_lim) {
  # some checks
  if (is.null(las$log_data)) {
    stop("No log data is present")
  }

  # subset curves and depth intervals
  if (!missing(curves)) {
    log_data <- dplyr::select_(las$log_data, depth.col, curves)
  } else {
    log_data <- las$log_data
  }

  if (missing(depth.col)) {
    depth.col <- names(log_data)[1]
  }

  if (!missing(depth_lim)) {
    log_data <- log_data[
      log_data[[depth.col]] > min(depth_lim) &
        log_data[[depth.col]] < max(depth_lim),
    ]
  }

  # plotting
  log_data %>%
    reshape2::melt(id.vars = depth.col) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = depth.col, y = "value")) +
    ggplot2::geom_line() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_reverse() +
    ggplot2::facet_grid(. ~ variable, scales = "free_x")
}


write.LAS <- function(las) {
}
