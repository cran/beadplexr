% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fcs.R
\name{read_fcs}
\alias{read_fcs}
\title{Read a fcs file.}
\usage{
read_fcs(
  .file_name,
  .fsc_ssc = c("FSC-A", "SSC-A"),
  .bead_channels = c("FL6-H", "FL2-H"),
  .filter = list(`FSC-A` = c(200000L, 800000L), `SSC-A` = c(200000L, 1000000L), `FL6-H` =
    c(7.3, Inf)),
  .compensation = "guess",
  ...
)
}
\arguments{
\item{.file_name}{The path and name of the file to be read.}

\item{.fsc_ssc}{The names of the forward and side scatter channels. A
character vector of length of two.}

\item{.bead_channels}{The names of the channels with bead events. A character
vector of length of at least two.}

\item{.filter}{Optional list of upper and lower cutoff for individual
channels. Use \code{.filter = NULL} for no filtering at all.}

\item{.compensation}{A character vector, a compensation matrix, or
\code{NULL}. See 'Details' for extended information of the argument.}

\item{...}{additional arguments passed to \code{flowCore::read.FCS}}
}
\description{
Is deprecated. See the vignette "Preparing flow-data for use with with
\code{beadplexr}" for an example of preparing flow-data to be used with
\code{beadplexr}.
}
