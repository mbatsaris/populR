% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pp_ancillary.R
\name{pp_ancillary}
\alias{pp_ancillary}
\title{Ancillary Information from OSM Features}
\usage{
pp_ancillary(x, volume = NULL, key)
}
\arguments{
\item{x}{an object of class \code{sf} that is used to associate OSM features
to. Usually, x may include polygon features representing building units}

\item{volume}{x volume information (height or number of floors) useful for
float ancillary information}

\item{key}{OSM feature keys or values available in x}
}
\value{
an object of class \code{sf} including ancillary information either for
    night or day estimates
}
\description{
Ancillary Information from OSM Features
}
\examples{
\dontrun{
    data('trg')

    # Download OSM amenities
    dt <- pp_vgi(trg, key = amenity)

    # create binary ancillary information
    dt <- pp_ancillary(dt, 'amenity')

    # create ancillary information both binary and float
    dt <- pp_ancillary(dt, floors, 'amenity')
}

}
