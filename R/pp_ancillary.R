#' Ancillary Information from OSM Features
#'
#' @param x an object of class \code{sf} that is used to associate OSM features
#'     to. Usually, x may include polygon features representing building units
#' @param volume x volume information (height or number of floors) useful for
#'     float ancillary information
#' @param key OSM feature keys or values available in x
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#'
#' @return an object of class \code{sf} including ancillary information either for
#'     night or day estimates
#' @export
#'
#' @examples
#' \dontrun{
#'     data('trg')
#'
#'     # Download OSM amenities
#'     dt <- pp_vgi(trg, key = amenity)
#'
#'     # create binary ancillary information
#'     dt <- pp_ancillary(dt, 'amenity')
#'
#'     # create ancillary information both binary and float
#'     dt <- pp_ancillary(dt, floors, 'amenity')
#' }
#'
pp_ancillary <- function(x, volume = NULL, key) {
  rlang::check_required(x)
  rlang::check_required(key)

  if (!inherits(x, "sf")) {
    cli::cli_abort('{.arg x} must be an object of class sf, not {.obj_type_friendly {x}.')
  }

  volume <- rlang::quo_name(rlang::enquo(volume))
  key <- paste(key)

  for (i in 1:length(key)) {
    k <- key[i]
    if (!k %in% colnames(x)) {
      cli::cli_abort('{k} cannot be found in the columns of {.arg x}.')
    }
  }


  x$binary <- 1
  x$binary <- x$binary - x[, key, drop = TRUE]
  x$binary[x$binary < 1] <- 0


  if (volume != 'NULL') {
    if (!volume %in% colnames(x)) {
      cli::cli_abort('{volume} cannot be found in the columns of {.arg x}.')
    }
    x$flt_day <- x[, key, drop = TRUE]/x[, volume, drop = TRUE]
    x$float <- round(1 - x$flt_day, 3)
    x$float[x$float <= 0] <- 0
    x$flt_day <- NULL
  }
  nm <- c(colnames(x)[colnames(x) != 'geometry'], 'geometry')
  x <- x[, nm]
  return(x)
}
