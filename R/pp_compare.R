#' Comparison to Other Data
#'
#' @param x An object of class \code{sf} or \code{data.frame} including
#'      estimated and actual values
#' @param estimated Population estimates using \link[populR]{pp_estimate}
#'      function
#' @param actual Actual population values
#' @param title Scatterplot title \code{string}
#'
#' @return A list including rmse, mae, linear model details and correlation coefficient
#' @export
#'
#' @importFrom sf st_as_sf
#' @importFrom graphics abline
#' @importFrom graphics text
#' @importFrom stats cor
#' @importFrom stats lm
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#' @importFrom Metrics rmse
#' @importFrom Metrics mae
#' @importFrom usethis ui_stop
#'
#' @examples
#' # read lib data
#' data('src')
#' data('trg')
#'
#' # areal weighting interpolation - awi
#' awi <- pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi)
#'
#' # volume weighting interpolation - vwi
#' vwi <- pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors)
#'
#' # awi - rmse
#' pp_compare(awi, estimated = pp_est, actual = rf,
#'     title ='awi')
#'
#' # vwi - rmse
#' pp_compare(vwi, estimated = pp_est, actual = rf,
#'     title ='vwi')
#'
pp_compare <- function(x, estimated, actual, title) {
  # check arguments
  if (missing(x)) {
    usethis::ui_stop('x is required')
  }

  if (missing(actual)) {
    usethis::ui_stop('actual is required')
  }

  if (missing(estimated)) {
    usethis::ui_stop('estimated is required')
  }

  if (missing(title)) {
    usethis::ui_stop('title is required')
  }

  actual <- rlang::quo_name(rlang::enquo(actual))
  estimated <- rlang::quo_name(rlang::enquo(estimated))

  # check if exists
  if (!estimated %in% colnames(x)) {
    usethis::ui_stop('{estimated} cannot be found')
  }

  if (!actual %in% colnames(x)) {
    usethis::ui_stop('{actual} cannot be found')
  }

  # check whether args are numeric
  if (!is.numeric(x[, actual, drop = TRUE])) {
    usethis::ui_stop('{actual} must be numeric')
  }

  # check whether spop is numeric
  if (!is.numeric(x[, estimated, drop = TRUE])) {
    usethis::ui_stop('{estimated} must be numeric')
  }

  # calculate rmse, calculate correlation coefficient and create linear regression model
  rmse <- Metrics::rmse(x[, actual, drop = T], x[, estimated, drop = T])
  mae <- Metrics::mae(x[, actual, drop = T], x[, estimated, drop = T])
  linear_model <- lm(x[, actual, drop = T] ~ x[, estimated, drop = T])
  correlation_coef <- round(summary(linear_model)$r.squared, 3)
  myList <- list(rmse = rmse, mae = mae, linear_model = linear_model, correlation_coef = correlation_coef)

  # scatterplot with line and correlation coeficient as text
  plot(x[, actual, drop = T], x[, estimated, drop = T], col="#634B56",
       main = substitute(paste(title, ", R"^2~paste("= ", correlation_coef)), list(title = title,
       correlation_coef = correlation_coef)), cex.main = 1.2, xlab = "Actual", ylab = "Estimated")
  abline(linear_model, col="#FD8D3C")

  return(myList)

}
