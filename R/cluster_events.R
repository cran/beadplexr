#' Clustering with trimming
#'
#' Cluster identification with various algorithms and subsequent trimming of each cluster
#'
#' @param df A tidy data.frame.
#' @param .parameter A character giving the name of column(s) where populations
#'   are identified.
#' @param .column_name A character giving the name of the column to store the
#'   population information.
#' @param .k Numeric giving the number of expected clusters, or a set of initial
#'   cluster centers.
#' @param .trim A numeric between 0 and 1, giving the fraction of points to
#'   remove by marking them NA.
#' @param .eps Reachability distance, see [fpc::dbscan()].
#' @param .MinPts Reachability minimum no. of points, see [fpc::dbscan()].
#' @param .sample_frac A numeric between 0 and 1 giving the fraction of points
#'   to use in initialisation of `Mclust()`.
#' @param .max_subset A numeric giving the maximum of events to use in
#'   initialisation of `Mclust()`, see below.
#' @param .data Deprecated. Use `df`.
#' @param ... Additional arguments passed to appropriate methods, see below.
#'
#' @return The data.frame in `df` with the cluster classification added in
#'   the column given by `.column_name`.
#'
#'
#' @section Additional parameters:
#' Information on additional arguments passed, can be found here:
#'
#' \describe{
#'   \item{clara}{[cluster::clara()]}
#'   \item{kmeans}{[kmeans()]}
#'   \item{dbscan}{[fpc::dbscan()]}
#'   \item{mclust}{[mclust::Mclust()]}
#'   \item{density_cut}{[approx_adjust()]}
#' }
#'
#' @name cluster_events
#'

#' @rdname cluster_events
#' @seealso [trim_population()], [identify_analyte()].
#'
#' Mclust and dbscan seems to do an excellent job at separating on the forward
#' and side scatter parameters. Mclust and clara both perform well separating
#' beads in the APC channel, but clara is about 3 times faster than Mclust.
#'
#' @export
#'
#' @examples
#' library(beadplexr)
#' library(dplyr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] |>
#'   # Speed things up a bit by selecting one fourth of the events.
#'   # Probably not something you'd usually do
#'   dplyr::sample_frac(0.25) |>
#'   bp_kmeans(.parameter = c("FSC-A", "SSC-A"),
#'             .column_name = "population", .trim = 0.1, .k = 2) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
#'
bp_kmeans <- function(df, .parameter, .column_name, .k, .trim = 0, .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "bp_kmeans")
    df <- .data
  }
  # Give function arguments useful values, if they are not set by the user
  .ellipsis <- list(...)
  .all_args <- list(x = df[, .parameter], centers = .k)

  if("centers" %in% names(.ellipsis)){
    .all_args[["centers"]] <- NULL
  }

  .all_args <- c(.all_args, .ellipsis)

  # It is much faster to just trim if needed at all
  if(.all_args[["centers"]] == 1){
    df[[.column_name]] <- "1"

    if(.trim > 0){
      return(trim_population(df = df, .parameter = .parameter, .column_name = .column_name, .trim = .trim))
    }
    return(df)
  }

  # Do kmeans clustering
  .clust_res <- do.call(stats::kmeans, .all_args)
  df[[.column_name]] <- .clust_res$cluster |> as.character()

  # Do trimming
  if(.trim > 0){
    split(df, factor(df[[.column_name]])) |>
      purrr::map_df(trim_population, .parameter = .parameter, .column_name = .column_name, .trim = .trim)
  }else{
    df
  }
}

#' @rdname cluster_events
#'
#' @section Default parameters to `clara()`:
#' [cluster::clara()] is by default called with the following parameters:
#'
#' \describe{
#'   \item{samples}{100}
#'   \item{pamLike}{TRUE}
#' }
#'
#' @export
#'
#' @examples
#' library(beadplexr)
#' library(dplyr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] |>
#'   # Speed things up a bit by selecting one fourth of the events.
#'   # Probably not something you'd usually do
#'   dplyr::sample_frac(0.25) |>
#'   bp_clara(.parameter = c("FSC-A", "SSC-A"),
#'            .column_name = "population", .trim = 0.1, .k = 2) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
#'
#' lplex[[1]] |>
#'   # Speed things up a bit by selecting one fourth of the events.
#'   # Probably not something you'd usually do
#'   dplyr::sample_frac(0.25) |>
#'   bp_clara(.parameter = c("FSC-A", "SSC-A"),
#'            .column_name = "population", .trim = 0, .k = 2) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
#'
bp_clara <- function(df, .parameter, .column_name, .k, .trim = 0, .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "bp_clara")
    df <- .data
  }
  # Give function arguments useful values, if they are not set by the user
  .ellipsis <- list(...)
  .all_args <- list(x = df[, .parameter], k = .k)

  if(! "samples" %in% names(.ellipsis)){
    .ellipsis[["samples"]] <- 100
  }
  if(! "pamLike" %in% names(.ellipsis)){
    .ellipsis[["pamLike"]] <- TRUE
  }
  if("k" %in% names(.ellipsis)){
    .all_args[["k"]] <- NULL
  }

  .all_args <- c(.all_args, .ellipsis)

  # It is much faster to just trim if needed at all
  if(.all_args[["k"]] == 1){
    df[[.column_name]] <- "1"

    if(.trim > 0){
      return(trim_population(df = df, .parameter = .parameter, .column_name = .column_name, .trim = .trim))
    }
    return(df)
  }

  .clust_res <- do.call(cluster::clara, .all_args)
  df[[.column_name]] <- .clust_res$cluster |> as.character()

  # Do trimming
  if(.trim > 0){
    split(df, factor(df[[.column_name]])) |>
      purrr::map_df(trim_population, .parameter = .parameter, .column_name = .column_name, .trim = .trim)
  }else{
    df
  }
}

#' @rdname cluster_events
#'
#' @section Parameters to dbscan:
#' It requires some trial and error to get the right parameters for the
#' density based clustering, but the parameters usually stay stable throughout an
#' entire experiment and over time (assuming that there is only little drift in
#' the flow cytometer). There is no guarantee that the correct number of clusters
#' are returned, and it might be better to use this on the forward - side
#' scatter discrimination.
#'
#' Scaling of the parameters seems to be appropriate in most cases for the
#' forward - side scatter discrimination and is automatically performed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' library(dplyr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] |>
#'   # Speed things up a bit by selecting one fourth of the events.
#'   # Probably not something you'd usually do
#'   dplyr::sample_frac(0.25) |>
#'   bp_dbscan(.parameter = c("FSC-A", "SSC-A"), .column_name = "population",
#'             eps = 0.2, MinPts = 50) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
#'
#' pop1 <- lplex[[1]] |>
#'   # Speed things up a bit by selecting one fourth of the events.
#'   # Probably not something you'd usually do
#'   dplyr::sample_frac(0.25) |>
#'   bp_dbscan(.parameter = c("FSC-A", "SSC-A"), .column_name = "population",
#'     eps = 0.2, MinPts = 50) |>
#'   dplyr::filter(population == "1")
#'
#' pop1 |>
#'   bp_dbscan(.parameter = c("FL6-H", "FL2-H"), .column_name = "population",
#'     eps = 0.2, MinPts = 50) |>
#'   pull(population) |>
#'   unique()
#'
#' pop1 |>
#'   bp_dbscan(.parameter = c("FL6-H", "FL2-H"), .column_name = "population",
#'     eps = 0.2, MinPts = 50, scale = FALSE) |>
#'   pull(population) |>
#'   unique()
#' }
bp_dbscan <- function(df, .parameter, .column_name, .eps = 0.2, .MinPts = 50 , .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "bp_dbscan")
    df <- .data
  }
  # Give function arguments useful values, if they are not set by the user
  .ellipsis <- list(...)
  .all_args <- list(data = df[, .parameter],
                    eps = .eps,
                    MinPts = .MinPts)

  if(! "scale" %in% names(.ellipsis)){
    .all_args[["scale"]] <- TRUE
  }

  if("eps" %in% names(.ellipsis)){
    .all_args[["eps"]] <- NULL
  }
  if("MinPts" %in% names(.ellipsis)){
    .all_args[["MinPts"]] <- NULL
  }

  .all_args <- c(.all_args, .ellipsis)

  .clust_res <- do.call(fpc::dbscan, .all_args)
  .clust_res <- .clust_res$cluster
  .clust_res <- ifelse(.clust_res == 0, NA, .clust_res)
  df[[.column_name]] <- .clust_res |> as.character()
  df
}

#' @rdname cluster_events
#'
#' @section Parameters to mclust:
#' Mclust is is slow and memory hungry on large datasets. Using a subset of the
#' data to initialise the clustering greatly improves the speed. I have found
#' that a subset sample of 500 even works well and gives no markedly better
#' clustering than a subset of 5000 events, but initialisation with 500 makes
#' the clustering complete about 12 times faster than with 5000 events.
#'
#' @import mclust
#' @export
#'
#' @examples
#' library(beadplexr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] |>
#'   bp_mclust(.parameter = c("FSC-A", "SSC-A"),
#'            .column_name = "population", .trim = 0, .k = 2) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
bp_mclust <- function(df, .parameter, .column_name, .k, .trim = 0, .sample_frac = 0.05, .max_subset = 500, .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "bp_mclust")
    df <- .data
  }
  # Need to load the mclust because it failes with:
  # Error: 'MclustBIC' is not an exported object from 'namespace:mclust'
  # Give function arguments useful values, if they are not set by the user
  .ellipsis <- list(...)

  init_subset <-  round(nrow(df), .sample_frac)
  init_subset <- if(init_subset >= .max_subset) 500
  init_subset <- sample(1:nrow(df), size = init_subset)

  .all_args <- list(data = df[, .parameter], G = .k, initialization = list(subset = init_subset))

  if("G" %in% names(.ellipsis)){
    .all_args[["G"]] <- NULL
  }

  if("initialization" %in% names(.ellipsis)){
    .all_args[["initialization"]] <- NULL
  }

  .all_args <- c(.all_args, .ellipsis)

  # It is much faster to just trim if needed at all
  if(.all_args[["G"]] == 1){
    df[[.column_name]] <- "1"

    if(.trim > 0){
      return(trim_population(df = df, .parameter = .parameter, .column_name = .column_name, .trim = .trim))
    }
    return(df)
  }

  .clust_res <- do.call(mclust::Mclust, .all_args)
  df[[.column_name]] <- .clust_res$classification |> as.character()

  # Do trimming
  if(.trim > 0){
    split(df, factor(df[[.column_name]])) |>
      purrr::map_df(trim_population, .parameter = .parameter, .column_name = .column_name, .trim = .trim)
  }else{
    df
  }
}

#' Density cut.
#'
#' Cut data based on density.
#'
#' @inheritParams approx_adjust
#' @inheritParams turning_point
#'
#' @return A factor, see [base::cut()]
#' @keywords internal
#' @export
#'
#' @examples
#' set.seed(1234)
#' .x <- c(rnorm(200, 0, 1), rnorm(200, 0.8, 1))
#' .k <-  2
#' density_cut(.x, .k)
#'
density_cut <- function(.x, .k, .lower = 0.1, .upper = 2, .step = 0.1){

  .adjust <- .x |> approx_adjust(.k = .k,
                                  .lower = .lower,
                                  .upper = .upper,
                                  .step = .step)

  if(is.na(.adjust)){
    stop("I found no optimal cluster prediction settings")
  }

  minima <-  turning_point(.x = .x,
                           .which = "minima",
                           .return = "value",
                           .adjust = .adjust)
  minima <- minima[["minima"]][[1]]

  .range <- .x |>
    range() |>
    c(minima) |>
    sort()

  .x |> cut(breaks = .range, labels = seq_len(length(.range) - 1))
}

#' @rdname cluster_events
#'
#' @section Parameters to density_cut:
#' This simple function works by smoothing a density function until the desired number
#' of clusters are found. The segregation of the clusters follows at the lowest
#' point between two clusters.
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
#'   bp_density_cut(.parameter = c("FSC-A"),
#'            .column_name = "population", .trim = 0, .k = 2) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
#'
bp_density_cut <- function(df, .parameter, .column_name, .k, .trim = 0, .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "bp_density_cut")
    df <- .data
  }
  .p <-  .parameter[1]
  call_cut_parameter <- function(.p){
    density_cut(.x = df[[.p]], .k = .k)
  }
  compare_vec_elements <- function(x, y){
    ifelse(x == y, x, NA)
  }

  # It is much faster to just trim if needed at all
  if(.k == 1){
    df[[.column_name]] <- "1"

    if(.trim > 0){
      return(trim_population(df = df, .parameter = .parameter, .column_name = .column_name, .trim = .trim))
    }
    return(df)
  }

  .clusters <- .parameter |> purrr::map(call_cut_parameter) |> stats::setNames(.parameter)

  df[[.column_name]] <- Reduce(compare_vec_elements, .clusters) |> as.character()

  # Do trimming
  if(.trim > 0){
    split(df, factor(df[[.column_name]])) |>
      purrr::map_df(trim_population, .parameter = .parameter, .column_name = .column_name, .trim = .trim)
  }else{
    df
  }
}

#' Calculate euclidean distance between two points.
#'
#' @param .x A numerical vector with coordinates to a point.
#' @param .c A numerical vector with the coordinates to the center.
#'
#' @return A numerical vector with the euclidean distance between the two points.
#'
#' @note This function does mot make use of the base function dist, as that
#'   dist-function is about twice as slow as the implementation here.
#'
#' @keywords internal
#'
#'
#' @examples
#' beadplexr:::calc_dist_to_centre(.x = c(10, 15), .c = c(1, 2))
#'
calc_dist_to_centre <- function(.x, .c){
  (.x - .c)^2 |>
    sum() |>
    sqrt()
}

#' Calculate population center
#'
#' @param .x A numerical vector.
#' @param .method A character giving the method to use. Currently only density is available.
#'
#' @return A numeric
#'
#' @keywords internal
#'
#' @examples
#' beadplexr:::calc_centre(.x = rnorm(100))
#'
calc_centre <- function(.x, .method = "density"){
  dens_res <- stats::density(.x)

  dens_res$x[dens_res$y == max(dens_res$y)]
}

#' Trim cluster.
#'
#' Remove the points furthest form the center of the cluster.
#'
#' @param df The tidy data.frame with clusters to be modified.
#' @param .parameter A character giving the name of dimensions to calculate
#'   distance on.
#' @param .column_name A character giving the name of the column with the
#'   cluster information.
#' @param .trim A numeric between 0 and 1, giving the fraction of points to
#'   remove.
#' @param .data Deprecated. Use `df`.
#'
#' @details The euclidean distance is calculated for each point defined by
#'   \code{.parameter} to the center of the cluster. The cluster designation of
#'   the \code{.trim} most distant points are changed to \code{NA}.
#'
#' @return A data.frame
#'
#'
#' @export
#'
#' @examples
#' library(beadplexr)
#' library(dplyr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' lplex[[1]] |>
#'   filter(`FSC-A` > 3.2e5L) |>
#'   mutate(population = "1") |>
#'   trim_population(.parameter = c("FSC-A", "SSC-A"), .column_name = "population", .trim = 0.1) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
#'
#' lplex[[1]] |>
#'   filter(`FSC-A` > 3.2e5L) |>
#'   mutate(population = "1") |>
#'   trim_population(.parameter = c("FSC-A", "SSC-A"),
#'                   .column_name = "population", .trim = 0.8) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = population) +
#'   geom_point()
trim_population <- function(df,
                            .parameter,
                            .column_name = "population",
                            .trim = 0.1,
                            .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "trim_population")
    df <- .data
  }

  if(FALSE %in% (.parameter %in% names(df))){
    stop(".parameter must all be valid column names of df")
  }
  if(FALSE %in% (.column_name %in% names(df))){
    stop(".column_name must be valid column names of df")
  }

  # It is much faster to to calculate per row of the matrix, than dplyrs rowwise
  # so we create the matrix now and calculate using this
  .data_matrix <- df |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(.parameter)) |>
    as.matrix()

  .centre <- .data_matrix |>
    apply(2, calc_centre, .method = "density")

  .distances <- .data_matrix |>
    apply(1, calc_dist_to_centre, .c = .centre)

  .in_cluster <- .distances  <  stats::quantile(.distances, probs = (1 - .trim))

  df[[.column_name]] <- ifelse(.in_cluster, as.character(df[[.column_name]]), NA)
  df
  }
