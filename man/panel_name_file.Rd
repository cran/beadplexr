% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/panel_information.R
\name{panel_name_file}
\alias{panel_name_file}
\title{Get file name from panel name}
\usage{
panel_name_file(.panel_name)
}
\arguments{
\item{.panel_name}{Character vector giving the name of the panel}
}
\value{
A character
}
\description{
Conversion from panel name to internal panel info file name
}
\examples{
beadplexr:::panel_name_file(.panel_name = "Human Th Cytokine Panel (13-plex)")

}
\keyword{internal}
