## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".data"))

#' Despeckle parameters
#'
#' Remove lonely, noisy data points in a 2D scatter matrix
#'
#' @param df A tidy data.frame.
#' @param .parameters A character of the length of two giving the parameters to
#'   despeckle.
#' @param .bins A numeric giving the resolution of the raster matrix. Increasing
#'   the resolution results in more isolated events.
#' @param .neighbours A numeric giving the minimum number of neighbours. Points
#'   with fewer neighbours are removed.
#' @param .data Deprecated. Use `df`.
#' @param ... Deprecated. It's use has no effect.
#'
#' @details
#' The values of the two parameters are binned into the given number of bins.
#' They are then cast into a 2D matrix, with the bins of the first of the
#' parameters ending up as rows, the bins of the second parameter as
#' columns, and combinations are marked by `1`.
#'
#' The rows of the `df` where lonely points are found in `.parameters` are removed.
#'
#' @return A `data.frame` with noisy points removed.
#'
#' @export
#'
#' @examples
#' library(beadplexr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] |>
#'   ggplot() +
#'   aes(x = `FL6-H`, y = `FL2-H`) +
#'   geom_point()
#'
#' lplex[[1]] |>
#'   despeckle(.parameters = c("FL6-H", "FL2-H"), .neighbours = 8) |>
#'   ggplot() +
#'   aes(x = `FL6-H`, y = `FL2-H`) +
#'   geom_point()
#'
#' lplex[[1]] |>
#'   despeckle(.parameters = c("FL6-H", "FL2-H"), .bin = 128) |>
#'   ggplot() +
#'   aes(x = `FL6-H`, y = `FL2-H`) +
#'   geom_point()
#'
despeckle <-
  function(df,
           .parameters,
           .bins = 256L,
           .neighbours = 4L,
           .data = NULL,
           ...) {
    if (!is.null(.data)) {
      raise_deprecated(old = ".data",
                       new = "df",
                       caller = "despeckle")
      df <- .data
    }
    if (length(list(...)) > 0) {
      raise_deprecated(old = "Additional args to `raster::clump()`",
                       new = "",
                       caller = "despeckle")
    }

    if (length(.parameters) != 2) {
      stop(".parameters must be of length two. No more, and no less")
    }
    if (FALSE %in% (.parameters %in% names(df))) {
      stop(".parameters not found in data")
    }

    .col_names_bin <- c("x_bin", "y_bin")
    names(.parameters) <- .col_names_bin

    # We calculate the number of neighbors each point have when the two parameters
    # are plotted against each other. This works best if we have integers, but we
    # must also be able to identify the points we have discharge in the original
    # data. By breaking the values of the two parameters into numbered bins, and
    # by keeping this information, we have what we need
    .set_bins <- function(x, .bins) {
      as.character(base::cut(x, breaks = .bins, labels = seq_along(1:.bins)))
    }

    .binned_data <-
      df |>
      dplyr::mutate(dplyr::across(dplyr::all_of(.parameters), ~ .set_bins(., .bins = .bins)))

    # Extract the information to a matrix.
    # The columns of interest in have 'bin' in their names, and since the same
    # x_bin - y_bin combination can be found more than once we remove all
    # duplicates before spreading, setting row names and cast to matrix
    .binned_matrix <-
      .binned_data |>
      dplyr::select(dplyr::all_of(.col_names_bin)) |>
      dplyr::distinct() |>
      dplyr::mutate(present = 1L) |>
      tidyr::pivot_wider(
        names_from = .col_names_bin[2],
        values_from = "present",
        values_fill = 0L
      ) |>
      tibble::column_to_rownames(var = .col_names_bin[1]) |>
      as.matrix()

    .despeckled_data <-
      .binned_matrix |>
      count_neighbours() |>
      tibble::as_tibble(rownames = .col_names_bin[1]) |>
      tidyr::pivot_longer(
        cols = !dplyr::any_of(.col_names_bin[1]),
        names_to = .col_names_bin[2],
        values_to = "num_neighbour"
      )

    # Having the bin numbers of all data points and their number of neighbours we
    # can now filter, clean and return
    .despeckled_data |>
      dplyr::filter(.data[["num_neighbour"]] >= .neighbours) |>
      dplyr::left_join(.binned_data, by = .col_names_bin) |>
      dplyr::select(-dplyr::all_of(c(.col_names_bin, "num_neighbour")))
  }

#' Pad a matrix
#'
#' @param x The matrix to pad.
#' @param pad_size An integer giving the number of columns/rows to pad with.
#' @param pad A single value giving the padding.
#'
#' @return
#' A matrix
#'
#' @keywords internal
#'
#' @examples
#'
#' x <- matrix(c(1:9), nrow = 3, ncol = 3)
#' beadplexr:::pad_matrix(x)
#'
#' x <- matrix(c(1:4), nrow = 2, ncol = 2)
#' beadplexr:::pad_matrix(x)
#' beadplexr:::pad_matrix(x, pad_size = 2L)
#' beadplexr:::pad_matrix(x, pad = NA)
#' beadplexr:::pad_matrix(x, pad = "xx")
#'
#' x <- matrix(c(1:6), nrow = 3, ncol = 2)
#' beadplexr:::pad_matrix(x)
#'
pad_matrix <- function(x, pad_size = 1L, pad = 0L) {
  nc <- ncol(x) + pad_size * 2
  nr <- nrow(x) + pad_size * 2
  pm <- matrix(rep(pad, nr * nc),
               nrow = nr,
               ncol = nc)

  pm[seq_len(nrow(x)) + pad_size,
     seq_len(ncol(x)) + pad_size] <- x
  pm
}

#' Count neighbours for each element in a binary matrix
#'
#' @param x The matrix to count the neighbours in.
#'
#' @return
#'
#' A matrix of the same size as `x` giving the number of elements with non-zero
#' values in neighbouring cells of the matrix.
#'
#' @keywords internal
#'
#' @examples
#'
#' x <- matrix(
#' c(1L, 1L, 0L,
#'   1L, 1L, 0L,
#'   0L, 1L, 1L),
#' nrow = 3,
#' ncol = 3,
#' byrow = TRUE
#' )
#' beadplexr:::count_neighbours(x)
#'
#' x <- matrix(
#'   c(TRUE, TRUE, FALSE,
#'     TRUE, TRUE, FALSE,
#'    FALSE, TRUE, TRUE),
#'   nrow = 3,
#'   ncol = 3,
#'  byrow = TRUE
#' )
#' beadplexr:::count_neighbours(x)
#'
#' x <- matrix(
#'   c(1, 1, 0, 0,
#'     1, 0, 0, 0,
#'     0, 1, 1 ,1,
#'     0, 1, 1, 1),
#'   nrow = 4,
#'   ncol = 4,
#'   byrow = TRUE
#' )
#' beadplexr:::count_neighbours(x)
#'
#' \dontrun{
#'   x <- matrix(
#'     c("1", "1", "0",
#'       "1", "1", "0",
#'       "0", "1", "1"),
#'     nrow = 3,
#'     ncol = 3,
#'     byrow = TRUE
#'   )
#'   #beadplexr:::count_neighbours(x)
#' }
#'
count_neighbours <- function(x) {
  if (!(is.numeric(x) | is.logical(x))) {
    stop(paste("x must be numeric or logical, not", typeof(x)))
  }
  nm <- matrix(rep(0, ncol(x) * nrow(x)),
               nrow = nrow(x),
               ncol = ncol(x))


  x_rn <- rownames(x)
  x_cn <- colnames(x)

  # Padd with 0s
  x <- pad_matrix(x)

  num_cols <- ncol(x)
  num_rows <- nrow(x)

  for (i in seq_len(num_rows)) {
    for (j in seq_len(num_cols)) {
      # Neighbourhood is a square of the size 3
      i_end <- i + 2
      j_end <- j + 2

      i_centre <- i + 1
      j_centre <- j + 1

      if (i_centre > num_rows) {
        next
      }

      if (j_centre > num_cols) {
        next
      }

      cv <- x[i_centre, j_centre]

      if (cv == 0) {
        next
      }

      nm[i, j] <- sum(x[c(i:i_end), c(j:j_end)]) - cv
    }
  }

  if (!is.null(x_rn)) {
    rownames(nm) <- x_rn
  }
  if (!is.null(x_cn)) {
    colnames(nm) <- x_cn
  }
  nm
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
#' x <- matrix(
#' c(1, 1, 0, 0,
#'   1, 0, 0, 0,
#'   0, 1, 1 ,1,
#'   0, 1, 1, 1),
#' nrow = 4,
#' ncol = 4,
#' byrow = TRUE
#' )
#' dist_chebyshev(x, diag = TRUE, upper = TRUE)
#'
dist_chebyshev <- function(x, diag = FALSE, upper = FALSE) {
  x <- x |> as.matrix()
  N <- nrow(x)
  .pairs <- utils::combn(N, 2)

  .dist <- apply(.pairs, MARGIN = 2, function(i) {
    max(abs(x[i[1],] - x[i[2],]))
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
as_data_frame_analyte <-
  function(.analytes,
           .id_bead = "Bead group",
           .id_analyte = "Analyte ID") {
    if (!inherits(.analytes, "list")) {
      stop("I expect .analytes to be a list")
    }

    .analytes |>
      purrr::map_df(function(.l) {
        .l |> purrr::map_df(function(.x) {
          tibble::tibble(name = .x$name,
                         concentration = .x$concentration)
        }, .id = .id_analyte)
      }, .id = .id_bead)

  }
