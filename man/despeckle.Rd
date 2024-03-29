% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{despeckle}
\alias{despeckle}
\title{Despeckle parameters}
\usage{
despeckle(df, .parameters, .bins = 256L, .neighbours = 4L, .data = NULL, ...)
}
\arguments{
\item{df}{A tidy data.frame.}

\item{.parameters}{A character of the length of two giving the parameters to
despeckle.}

\item{.bins}{A numeric giving the resolution of the raster matrix. Increasing
the resolution results in more isolated events.}

\item{.neighbours}{A numeric giving the minimum number of neighbours. Points
with fewer neighbours are removed.}

\item{.data}{Deprecated. Use \code{df}.}

\item{...}{Deprecated. It's use has no effect.}
}
\value{
A \code{data.frame} with noisy points removed.
}
\description{
Remove lonely, noisy data points in a 2D scatter matrix
}
\details{
The values of the two parameters are binned into the given number of bins.
They are then cast into a 2D matrix, with the bins of the first of the
parameters ending up as rows, the bins of the second parameter as
columns, and combinations are marked by \code{1}.

The rows of the \code{df} where lonely points are found in \code{.parameters} are removed.
}
\examples{
library(beadplexr)
library(ggplot2)

data("lplex")

lplex[[1]] |>
  ggplot() +
  aes(x = `FL6-H`, y = `FL2-H`) +
  geom_point()

lplex[[1]] |>
  despeckle(.parameters = c("FL6-H", "FL2-H"), .neighbours = 8) |>
  ggplot() +
  aes(x = `FL6-H`, y = `FL2-H`) +
  geom_point()

lplex[[1]] |>
  despeckle(.parameters = c("FL6-H", "FL2-H"), .bin = 128) |>
  ggplot() +
  aes(x = `FL6-H`, y = `FL2-H`) +
  geom_point()

}
