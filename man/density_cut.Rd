% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_events.R
\name{density_cut}
\alias{density_cut}
\title{Density cut.}
\usage{
density_cut(.x, .k, .lower = 0.1, .upper = 2, .step = 0.1)
}
\arguments{
\item{.x}{A numeric vector.}

\item{.k}{Numeric giving the number of expected clusters.}

\item{.lower, .upper}{The interval for possible value of adjust.}

\item{.step}{A numeric giving the increment to adjust. Sometimes low values
are needed to find a proper adjust value.}
}
\value{
A factor, see \code{\link[base:cut]{base::cut()}}
}
\description{
Cut data based on density.
}
\examples{
set.seed(1234)
.x <- c(rnorm(200, 0, 1), rnorm(200, 0.8, 1))
.k <-  2
density_cut(.x, .k)

}
\keyword{internal}
