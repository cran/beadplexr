#' Read a fcs file.
#'
#' Is deprecated. See the vignette "Preparing flow-data for use with with
#' `beadplexr`" for an example of preparing flow-data to be used with
#' `beadplexr`.
#'
#' @param .file_name The path and name of the file to be read.
#' @param .fsc_ssc The names of the forward and side scatter channels. A
#'   character vector of length of two.
#' @param .bead_channels The names of the channels with bead events. A character
#'   vector of length of at least two.
#' @param .filter Optional list of upper and lower cutoff for individual
#'   channels. Use \code{.filter = NULL} for no filtering at all.
#' @param .compensation A character vector, a compensation matrix, or
#'   \code{NULL}. See 'Details' for extended information of the argument.
#'
#' @param ... additional arguments passed to \code{flowCore::read.FCS}
#'
#' @export

read_fcs <- function(.file_name, .fsc_ssc = c("FSC-A", "SSC-A"), .bead_channels = c("FL6-H", "FL2-H"),
                     .filter = list("FSC-A" = c(2e5L, 8e5L), "SSC-A"= c(2e5L, 1e6L), "FL6-H" = c(7.3, Inf)),
                     .compensation = "guess", ...){
  dep_str = c(
    "'read_fcs' is deprecated.",
    "Use 'flowCore::read.FCS' instead.")

  msg_str = c(
    "Due to licence issues, reading flow-data has been deprecated.",
    "See the vignette 'reading_fcs_data'",
    "for an example of preparing flow-data")
  msg_str = paste(msg_str, collapse = " ")

  .Deprecated(new = "flowCore::read.FCS",
              msg = paste(c(dep_str, strwrap(msg_str)), collapse = "\n"))
}

