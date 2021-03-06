## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' Despeckle parameters
#'
#' Remove lonely, noisy data points in a 2D scatter matrix
#'
#' @param df A tidy data.frame
#' @param .parameters A character of the length of two giving the parameters to despeckle.
#' @param .bins A numeric giving the resolution of the raster matrix.
#' @param .neighbours A numeric giving the minimum number of neighbors. Points with fewer neighbors are removed.
#' @param .data Deprecated. Use `df`.
#' @param ... Additional parameters passed to [raster::clump()]
#'
#' @details
#' The values of the two parameters are binned into the given number of bins.
#' They are then cast into a 2D matrix, with the bins of the first of the
#' parameters ending up as rows, the bins of the second parameter as
#' columns, and combinations are marked by `1`.
#'
#' This matrix is turned into a `RasterLayer` by [raster::raster()] and the
#' number of neighbors are calculated by [raster::clump()].
#'
#' The rows of the `df` where lonely points are found in `.parameters` are removed.
#'
#' @note
#' This function requires that the `igraph` package is available.
#'
#' @return A `data.frame` with noisy points removed.
#'
#' @importFrom raster "%in%"
#'
#' @export
#'
#' @examples
#' library(beadplexr)
#' library(magrittr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] %>%
#'   ggplot() +
#'   aes(x = `FL6-H`, y = `FL2-H`) +
#'   geom_point()
#'
#' lplex[[1]] %>%
#'   despeckle(.parameters = c("FL6-H", "FL2-H"), .neighbours = 8) %>%
#'   ggplot() +
#'   aes(x = `FL6-H`, y = `FL2-H`) +
#'   geom_point()
#'
#' lplex[[1]] %>%
#'   despeckle(.parameters = c("FL6-H", "FL2-H"), .bin = 128, direction = 4) %>%
#'   ggplot() +
#'   aes(x = `FL6-H`, y = `FL2-H`) +
#'   geom_point()
#'
despeckle <- function(df, .parameters, .bins = 256L, .neighbours = 4L, .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "despeckle")
    df <- .data
  }

  if(length(.parameters) != 2){
    stop(".parameters must be of length two. No more, and no less")
  }
  if(FALSE %in% (.parameters %in% names(df))){
    stop(".parameters not found in data")
  }

  .col_names_bin <- c("x_bin", "y_bin")
  names(.parameters) <- .col_names_bin
  # We use the raster package to calculate the number of neighbors each point
  # have when the two paramters are plotted against eachother. This works best
  # if we have integers, but we must also be able to  identify the points we
  # have discharge in the original data. By breaking the values of the two
  # parameters into numbered bins, and by keeping this information, we
  # have what we need
  .set_bins <- function(x, .bins){
    as.character(base::cut(x, breaks = .bins, labels = seq_along(1:.bins)))
  }

  .binned_data <-
    df %>%
      dplyr::mutate_at(.vars = .parameters, .funs = ~.set_bins(., .bins = .bins))

  # Raster only works with a matrix so this we need to have.
  # The columns of interest in have 'bin' in their names, and since the same
  # x_bin - y_bin combination can be found more than once we remove all
  # duplicates before spreading, setting rownames and cast to matrix
  .binned_matrix <-
    .binned_data %>%
    dplyr::select(dplyr::one_of(.col_names_bin)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(present = 1L) %>%
    tidyr::spread(key = .col_names_bin[2], value = "present", fill = 0L) %>%
    tibble::as_tibble() %>%
    tibble::column_to_rownames(var = .col_names_bin[1]) %>% as.matrix
  # It a little bit of work getting to the values with low number of neighbors.
  # First we have to find the number of neighboutrs ofcourse and the ones below
  # the cutoff so we can the number of neightbours to nothing
  .clump_res <- .binned_matrix %>%
    raster::raster() %>%
    raster::clump(...)

  # clump assigns each data point a number and a count of neighbors
  .speckles <-
    .clump_res %>%
    raster::freq() %>%
    tibble::as_tibble() %>%
    dplyr::filter(!!rlang::sym("count") < .neighbours)

  .clump_res[.clump_res %in% .speckles$value] <- NA
  # The number of neighbors are stored in a single vector which we must turn
  # into a tidy data.frame. The column and row names of the binned matrix
  # corrospond to the bin number, which is what we need to filter the original
  # data

  .despeckled_data <-
    .clump_res@data@values %>%
    as.integer() %>%
    matrix(ncol = ncol(.binned_matrix), byrow = TRUE) %>%
    magrittr::set_colnames(value = colnames(.binned_matrix)) %>%
    magrittr::set_rownames(value = NULL) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(!!.col_names_bin[1] := rownames(.binned_matrix)) %>%
    tidyr::gather(
      key = !!.col_names_bin[2],
      value = "raster_number",
      -dplyr::one_of(.col_names_bin[1])
    ) %>%
    dplyr::filter(!is.na(!!rlang::sym("raster_number")))

  # Having the bin numbers of all data points with more than the minimum number
  # of neighbors we can now filtermclean and return
  .despeckled_data %>%
    dplyr::left_join(.binned_data, by = .col_names_bin) %>%
    dplyr::select(-dplyr::one_of(c(.col_names_bin, "raster_number")))
}

#' Chebyshev distance
#'
#' @param x a numeric matrix or data frame.
#' @param diag logical value indicating whether the diagonal of the distance
#'   matrix should be printed by `print.dist`.
#' @param upper logical value indicating whether the upper triangle of the
#'   distance matrix should be printed by `print.dist` .
#'
#' @return Chebyshev distance returns an object of class "`dist`".
#' @export
#'
#' @examples
#'
#' x <- matrix(rnorm(100), nrow = 5)
#' dist_chebyshev(x)
#'
dist_chebyshev <- function(x, diag = FALSE, upper =FALSE) {
  x <- x %>% as.matrix()
  N <- nrow(x)
  .pairs <- utils::combn(N, 2)

  .dist <- apply(.pairs, MARGIN = 2, function(i){
    max(
      abs(
        x[i[1], ] - x[i[2], ]
      )
    )
  })

  attr(.dist, "Size") <- N
  attr(.dist, "Labels") <- dimnames(x)[[1]]
  attr(.dist, "Diag") <- diag
  attr(.dist, "Upper") <- upper
  attr(.dist, "method") <- "Chebyshev"
  attr(.dist, "call") <- match.call()
  class(.dist) <- "dist"
  .dist
}

#' Cast list of analytes to `data.frame`
#'
#' A well structured list, such at those loaded by [load_panel()], is cast to a data.frame.
#'
#' @param .analytes The named list to be cast. It usually is loaded using
#'   [load_panel()]. See Details for expected structure.
#' @param .id_bead,.id_analyte The name of the column to hold the bead group and the analyte ID, respectively.
#'
#' @details
#'
#' Each analyte in the `list` passed to the function is expected to be a named
#' list with named elements `name` and `concentration`. The name of the `list`
#' with the analyte specific information is the analyte ID.
#'
#' Because of the particular setup of the LEGENDplex assay with two bead groups,
#' the analytes are expected to be wrapped in another `list`.
#'
#' @return A `data-frame`
#' @export
#'
#' @examples
#' .analytes <-  list(A = list(
#'                 A1 = list(name = "name_a1", concentration = 500),
#'                 A2 = list(name = "name_a2", concentration = 50000)))
#'
#' as_data_frame_analyte(.analytes)
as_data_frame_analyte <- function(.analytes, .id_bead = "Bead group", .id_analyte = "Analyte ID"){
  if(! "list" %in% class(.analytes)){
    stop("I expect .analytes to be a list")
  }

  .analytes %>%
    purrr::map_df(function(.l){
      .l %>% purrr::map_df(function(.x){
        tibble::data_frame(name = .x$name, concentration = .x$concentration)
      }, .id = .id_analyte)
    }, .id = .id_bead)

}
