% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_round.R
\name{pp_round}
\alias{pp_round}
\title{Rounding Function}
\usage{
pp_round(target, tpop, spop, sid)
}
\arguments{
\item{target}{object of class \code{sf} obtained by the
\link[populR]{pp_estimate} function and includes population
estimates}

\item{tpop}{target population}

\item{spop}{source population}

\item{sid}{source id}
}
\value{
an object of class \code{sf} including rounded population counts stored
    in a new column called pp_int
}
\description{
Rounding Function
}
\examples{
# read lib data
data('src')
data('trg')

# areal weighted interpolation - awi
awi <- pp_estimate(trg, src, sid = sid, spop = pop,
    method = awi)

# volume weighted interpolation - vwi
vwi <- pp_estimate(trg, src, sid = sid, spop = pop,
    method = vwi, volume = floors)

# awi - round
pp_round(awi, tpop = pp_est, spop = pop, sid = sid)

# vwi - round
pp_round(vwi, tpop = pp_est, spop = pop, sid = sid)

}
