#' Areal Interpolation of Population Data
#'
#' @param target An object of class \code{sf} that is used to interpolate data
#'     to. Usually, target may include polygon features representing building units
#' @param source An object of class \code{sf} including data to be interpolated.
#'     Source may be a set of coarse polygon features such as city blocks or
#'     census tracts
#' @param sid Source identification number
#' @param spop Source population values to be interpolated
#' @param method Two methods provided: \code{awi} (areal weighting interpolation)
#'     and \code{vwi} (volume weighting interpolation). \code{awi} proportionately
#'     interpolates the population values based on areal weights calculated
#'     by the area of intersection between the source and target zones. \code{vwi}
#'     proportionately interpolates the population values based on areal weights
#'     calculated by the area of intersection between the source and target zones
#'     multipled by the volume information (height or number of floors).
#' @param volume Target feature volume information (height or number of floors).
#'     Required when \code{method=vwi}
#' @param ancillary ancillary information
#' @param point Whether to return point geometries (FALSE by default)
#'
#' @return An object of class \code{sf} including estimated population
#'     counts for target features using either \code{awi} or \code{vwi}
#'     methods. The estimated population counts are stored in a new column called
#'     pp_est.
#' @export
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_crs
#' @importFrom sf st_make_valid
#'
#' @examples
#' # read lib data
#' data('src')
#' data('trg')
#'
#' # areal weighted interpolation - awi
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi)
#'
#' # areal weighted interpolation - awi using point geometries
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi, point = TRUE)
#'
#' # volume weighted interpolation - vwi
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors)
#'
#' # volume weighted interpolation - vwi using point geometries
#' pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors, point = TRUE)
#'
pp_estimate <- function(target, source, sid, spop, volume = NULL, ancillary = NULL, point = FALSE, method) {
  # check arguments
  rlang::check_required(target)
  rlang::check_required(source)
  rlang::check_required(sid)
  rlang::check_required(spop)
  rlang::check_required(method)

  # enquote args where necessary
  sid <- rlang::quo_name(rlang::enquo(sid))
  spop <- rlang::quo_name(rlang::enquo(spop))
  volume <- rlang::quo_name(rlang::enquo(volume))
  method <- rlang::quo_name(rlang::enquo(method))
  ancillary <- rlang::quo_name(rlang::enquo(ancillary))

  # check whether source and .target are of sf class
  if (!inherits(source, "sf")) {
    cli::cli_abort("{.arg source} must be an object of class sf, not {.obj_type_friendly {source}}")
  }

  if (!inherits(target, "sf")) {
    cli::cli_abort('{.arg target} must be an object of class sf, not {.obj_type_friendly {target}}.')
  }

  # check whether source and target share the same crs
  if (sf::st_crs(target) != sf::st_crs(source)) {
    cli::cli_abort('CRS mismatch.')
  }

  # check whether params exist in the given source object
  if (!sid %in% colnames(source)) {
    cli::cli_abort('{sid} cannot be found')
  }

  if (!spop %in% colnames(source)) {
    cli::cli_abort('{spop} cannot be found')
  }

  # check whether method is valid
  rlang::arg_match0(method, c('awi', 'vwi', 'bdi', 'fdi'))

  # check whether spop is numeric
  if (!is.numeric(source[, spop, drop = TRUE])) {
    cli::cli_abort('{spop} must be numeric')
  }

  # check whether point is logical
  if (!is.logical(point)) {
    cli::cli_abort('point must be logical (T|TRUE, F|FALSE')
  }

  # check whether sid and pop already exists in both target and source features
  if (any(colnames(target) == sid)) {
    colnames(target)[names(target) == sid] <- 'target_id'
  }

  if (any(colnames(target) == spop)) {
    colnames(target)[names(target) == spop] <- 'target_pop'
  }

  # make valid geometries
  target <- sf::st_make_valid(target)
  source <- sf::st_make_valid(source)

  if (method == 'awi') {
    out <- pp_awi(target, source = source, sid = sid, spop = spop,
                  point = point)
  } else if (method == 'vwi') {
    if (is.null(volume)) {
      cli::cli_abort('volume is required for vwi')
    }

    if (!volume %in% colnames(target)) {
      cli::cli_abort('{volume} cannot be found')
    }

    if (!is.numeric(target[, volume, drop = TRUE])) {
      cli::cli_abort('{volume} must be numeric')
    }
    out <- pp_vwi(target, source = source, sid = sid, spop = spop,
                  volume = volume, point = point)
  } else if (is.null(method)) {
    if (ancillary == 'NULL') {
      cli::cli_abort('ancillary is required for bdi')
    }

    if (!ancillary %in% colnames(target)) {
      cli::cli_abort('{ancillary} cannot be found')
    }

    if (!is.numeric(target[, ancillary, drop = TRUE])) {
      cli::cli_abort('{ancillary} must be numeric')
    }
    if (volume == 'NULL') {
      out <- pp_bdi(target, source = source, sid = sid, spop = spop,
                    point = point, ancillary = ancillary)
    } else {

      if (!volume %in% colnames(target)) {
        cli::cli_abort('{volume} cannot be found')
      }

      if (!is.numeric(target[, volume, drop = TRUE])) {
        cli::cli_abort('{volume} must be numeric')
      }
      out <- pp_bdi(target, source = source, sid = sid, spop = spop,
                    volume = volume, point = point, ancillary = ancillary)
    }

  } else if (method == 'fdi') {
    if (is.null(ancillary)) {
      cli::cli_abort('ancillary is required for fdi')
    }

    if (!ancillary %in% colnames(target)) {
      cli::cli_abort('{ancillary} cannot be found')
    }

    if (!is.numeric(target[, ancillary, drop = TRUE])) {
      cli::cli_abort('{ancillary} must be numeric')
    }

    if (!volume %in% colnames(target)) {
      cli::cli_abort('{volume} cannot be found')
    }

    if (!is.numeric(target[, volume, drop = TRUE])) {
      cli::cli_abort('{volume} must be numeric')
    }

    out <- pp_fdi(target, source = source, sid = sid, spop = spop,
                  point = point, ancillary = ancillary, volume = volume)


  }

  nm <- c(colnames(out)[colnames(out) != 'geometry'], 'geometry')
  out <- out[, nm]

  return(out)
}
