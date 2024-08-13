#' Rounding Function
#'
#' @param x An object of class \code{sf} obtained by the
#'     \link[populR]{pp_estimate} function
#' @param tpop Target population estimates obtained by the
#'     \link[populR]{pp_estimate} function
#' @param spop Initial source population values (included after the implementation
#'     of the \link[populR]{pp_estimate} function)
#' @param sid Source identification number
#'
#' @return An object of class \code{sf} including rounded population counts stored
#'     in a new column called pp_int
#' @export
#'
#' @importFrom rlang quo_name
#' @importFrom rlang enquo
#'
#' @examples
#' # read lib data
#' data('src')
#' data('trg')
#'
#' # areal weighted interpolation - awi
#' awi <- pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = awi)
#'
#' # volume weighted interpolation - vwi
#' vwi <- pp_estimate(trg, src, sid = sid, spop = pop,
#'     method = vwi, volume = floors)
#'
#' # awi - round
#' pp_round(awi, tpop = pp_est, spop = pop, sid = sid)
#'
#' # vwi - round
#' pp_round(vwi, tpop = pp_est, spop = pop, sid = sid)
#'
pp_round <- function(x, tpop, spop, sid) {
  #check arguments
  rlang::check_required(x)
  rlang::check_required(tpop)
  rlang::check_required(spop)
  rlang::check_required(sid)

  # check whether colnames exist
  spop <- rlang::quo_name(rlang::enquo(spop))
  sid <- rlang::quo_name(rlang::enquo(sid))
  tpop <- rlang::quo_name(rlang::enquo(tpop))

  # check whether parameters exist in the given target object
  if (!spop %in% colnames(x)) {
    cli::cli_abort('{spop} cannot be found')
  }

  if (!sid %in% colnames(x)) {
    cli::cli_abort('{sid} cannot be found')
  }

  if (!tpop %in% colnames(x)) {
    cli::cli_abort('{tpop} cannot be found')
  }

  # check whether spop and tpop are numeric
  if (!is.numeric(x[, spop, drop = TRUE])) {
    cli::cli_abort('{spop} must be numeric')
  }

  if (!is.numeric(x[, tpop, drop = TRUE])) {
    cli::cli_abort('{tpop} must be numeric')
  }

  x$newid <- 1:nrow(x)
  x$pp_int <- round(x[, tpop, drop = TRUE], 0)
  x$diff <- x[, tpop, drop = TRUE] - x$pp_int

  code <- unique(x[, sid, drop = TRUE])

  df <- data.frame()
  for (i in 1:length(code)) {
    df <- rbind(df, c(code[i], unique(x[, spop, drop = TRUE][x[, sid, drop = TRUE] == code[i]]),
                      sum(x[, tpop, drop = TRUE][x[, sid, drop = TRUE] == code[i]]),
                      sum(x[, 'pp_int', drop = TRUE][x[, sid, drop = TRUE] == code[i]])))
  }
  names(df) <- c('code', 'spop', 'estpop', 'intpop')

  for (i in 1:nrow(df)) {
    diaf <- df$spop[i] - df$intpop[i]

    if (diaf == 0) {
      next
    } else if (diaf > 0) {
      d <- abs(diaf)
      sub <- x[x[, sid, drop = TRUE] == df$code[i], ]
      sub <- sub[order(-sub$diff), ]
      for (j in 1:d) {
        sub$pp_int[j] <- sub$pp_int[j] + 1
      }
    } else if (diaf < 0) {
      d <- abs(diaf)
      sub <- x[x[, sid, drop = TRUE] == df$code[i], ]
      sub <- sub[order(sub$diff), ]
      for (j in 1:d) {
        sub$pp_int[j] <- sub$pp_int[j] - 1
      }
    }
    for (j in 1:nrow(sub)) {
      x$pp_int[x$newid == sub$newid[j]] <- sub$pp_int[j]
    }
  }

  x$newid <- NULL
  x$diff <- NULL

  return(x)

}
