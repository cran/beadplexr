% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualisations.R
\name{facs_plot}
\alias{facs_plot}
\alias{facs_scatter}
\alias{facs_density2d}
\alias{facs_density1d}
\alias{facs_hexbin}
\title{Plot FACS data.}
\usage{
facs_plot(
  df,
  .x = "FSC-A",
  .y = "SSC-A",
  .type = c("scatter", "density1d", "density2d", "hexbin"),
  .data = NULL,
  ...
)

facs_scatter(
  df,
  .x = "FSC-A",
  .y = "SSC-A",
  .beads = NULL,
  .plot_distinct = TRUE,
  .data = NULL
)

facs_density2d(df, .x = "FSC-A", .y = "SSC-A", .beads = NULL, .data = NULL)

facs_density1d(df, .x = "FSC-A", .beads = NULL, .data = NULL)

facs_hexbin(df, .x = "FSC-A", .y = "SSC-A", .bins = 75, .data = NULL)
}
\arguments{
\item{df}{The data to be plotted in a \code{data.frame}.}

\item{.x, .y}{Character vector with the column name for the variable to plot
on the x or y-axis.}

\item{.type}{Character vector giving the type of plot being used. Options are
one of \code{"scatter", "density", "hexbin"}.}

\item{.data}{Deprecated. Use \code{df}.}

\item{...}{Arguments passed to the individual functions.}

\item{.beads}{Character vector to with the column name with identification of
beads. If used it will show up with the aesthetic 'color'. Defaults to not
being used.}

\item{.plot_distinct}{Boolean to decide if only distinct events should be
plotted. If used, the number of data points might be greatly reduced which
could make for faster plotting. Defaults to TRUE.}

\item{.bins}{Numeric vector giving number of bins in both vertical and
horizontal directions. Set to 75 by default.}
}
\value{
A \code{ggplot}
}
\description{
Wrappers around building a ggplot with \code{geom_point},
\code{geom_density_2d}, and \code{geom_hex}.
}
\details{
These plot functions are meant to provide a quick way of viewing the FACS
data. For more control, use \code{ggplot2} directly.
}
\examples{
\dontrun{
library(beadplexr)
data("lplex")

df <-  lplex[[1]]
df$bead_group <- ifelse(df$`FSC-A` < 4e5L, "A", "B")

# Using facs_plot
facs_plot(df, .type = "scatter")
facs_plot(df, .type = "density1d")
facs_plot(df, .type = "density2d")
facs_plot(df, .type = "hexbin")

facs_plot(df, .type = "scatter", .beads = "bead_group")
facs_plot(df, .type = "density1d", .beads = "bead_group")
facs_plot(df, .type = "hexbin", .bins = 50)

facs_plot(df, .x = "FL2-H", .type = "scatter", .beads = "bead_group")

# Individual functions
facs_scatter(df)

facs_scatter(df, .beads = "bead_group", .plot_distinct = FALSE)
facs_scatter(df, .beads = "bead_group")

facs_scatter(df, .x = "FL2-H", .y = "FL6-H", .beads = "bead_group")

facs_density1d(df)
facs_density1d(df, .beads = "bead_group")

facs_density2d(df)
facs_density2d(df, .beads = "bead_group")

facs_hexbin(df)
facs_hexbin(df, .bins = 30)
}
}
