#' Download and Count OSM Features Over Target
#'
#' @param x an object of class \code{sf} that is used to interpolate data
#'     to. Usually, x may include polygon features representing building units
#' @param key osm feature key (quoted) see \link[osmdata]{available_features}
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom sf st_bbox
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#' @importFrom sf st_intersects
#' @importFrom osmdata available_features
#' @importFrom osmdata available_tags
#' @importFrom osmdata opq
#' @importFrom osmdata add_osm_features
#' @importFrom osmdata osmdata_sf
#' @importFrom dplyr %>%
#' @importFrom units drop_units
#'
#' @return an object of class \code{sf} including OSM features
#' @export
#'
#' @examples
#' \dontrun{
#'     data('trg')
#'
#'     # example using just a key
#'     pp_vgi(trg, key = 'amenity')
#'
#'     # example using two keys
#'     pp_vgi(trg, key = c('amenity', 'shop')
#' }
#'

pp_vgi <- function(x, key) {
  # check args
  rlang::check_required(x)
  rlang::check_required(key)

  xc <- "sf" %in% class(x)
  if (!inherits(x, "sf")) {
    cli::cli_abort('{.arg x} must be an object of class sf, not {.obj_type_friendly {x}}.')
  }

  # keep track of srid and transform if necessary
  x_crs <- sf::st_crs(x, parameters = TRUE)$srid
  t_crs <- 'EPSG:4326'
  if (x_crs != t_crs) {
    x <- sf::st_transform(x, t_crs)
  }

  # check whether keys exist in osm list
  af <- osmdata::available_features()
  for (i in 1:length(key)) {
    if (!key[i] %in% af) {
      cli::cli_abort('{key[i]} is not a valid OSM feature.')
    }
  }

  # convert key string
  key <- sprintf("\"%s\"", key)

  # get osm_features
  bb <- sf::st_bbox(x)
  dt <- opq(bbox = bb) %>%
    add_osm_features(features = key) %>%
    osmdata_sf()

  # get osm_points
  pt <- dt$osm_points
  pt <- pt[, c('osm_id', 'geometry')]

  # get osm_polygons
  pl <- dt$osm_polygons
  pl <- suppressWarnings(st_centroid(pl))
  pl <- pl[, c('osm_id', 'geometry')]

  # combine both datasets into a new single point
  fdt <- rbind(pt, pl)

  # transform the CRS of the data
  fdt <- sf::st_transform(fdt, x_crs)
  x <- sf::st_transform(x, x_crs)

  # count points over x polygons
  x$pp_vgi <- lengths(st_intersects(x, fdt))

  # re-arrange the names
  nm <- c(colnames(x)[colnames(x) != 'geometry'], 'geometry')
  x <- x[, nm]
  x <- sf::st_zm(x)

  return(x)
}
