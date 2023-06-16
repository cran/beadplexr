#' Identify multiplex assay analytes
#'
#' Convenience functions to identify analytes in different multiplex systems.
#'
#' @details
#' These functions wraps around the process of:
#'
#'   * Trim or subset on forward side scatter
#'   * Identifying analytes. For LEGENDplex in both bead groups
#'
#' If the forward side scatter events are not trimmed, the function is equivalent
#' to call [identify_analyte()] with CBA or MACSPlex data.
#'
#' @section Analytes:
#' The parameter `.analytes` is either a simple vector with the IDs or, in the
#' case of the LEGENDplex system, a list giving the IDs of analytes among the groups A and B.
#'
#' A list for the LEGENDplex system might look like this:
#'
#'  ```
#'    list(A = c("A1", "A2"),
#'         B = c("B1", "B2"))
#'  ```
#'
#' The **order** of analyte IDs is important and must match the expected order of analytes.
#'
#' @section Method arguments:
#' The parameter `.method_args` is a list of key-value pairs passed to [identify_analyte()].
#'
#' @return A data.frame
#'
#'
#' @name identify_assay_analyte
#'
NULL

#' @rdname identify_assay_analyte
#'
#' @param df A tidy data.frame.
#' @param .analytes A vector or list giving the IDs of the analytes. The
#'   **order** is important and must match the expected order of analytes.
#' @param .method_args A list giving the parameters passed on to `identify_analyte()`.
#' @param .data Deprecated. Use `df`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' library(dplyr)
#' data("lplex")
#' df <- lplex[[1]]
#'
#' panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")
#'
#' args_ident_analyte <- list(fs = list(.parameter = c("FSC-A", "SSC-A"),
#'                                      .column_name = "Bead group",
#'                                      .trim = 0.1,
#'                                      .method = "clara"),
#'                            analytes = list(.parameter = "FL6-H",
#'                                            .column_name = "Analyte ID",
#'                                            .trim = 0,
#'                                            .method = "clara"))
#'
#' annot_events <- identify_legendplex_analyte(df = df,
#'                                             .analytes = panel_info$analytes,
#'                                             .method_args = args_ident_analyte)
#'
#' annot_events |> facs_plot(.beads = "Bead group")
#'
#' annot_events |>
#'   filter(`Bead group` == "A") |>
#'   facs_plot(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID")
#'
#' annot_events |>
#'   filter(`Bead group` == "B") |>
#'   facs_plot(.x = "FL2-H", .y = "FL6-H", .beads = "Analyte ID")
#' }
identify_legendplex_analyte <- function(df, .analytes, .method_args, .data = NULL){

  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "identify_legendplex_analyte")
    df <- .data
  }

  # ## Identify A and B ##
  #
  # Do bead identification
  df <- ident_bead_pop(.analytes = names(.analytes), .call_args = .method_args[[1]], df = df)

  # To be able to subset on the beads A and B, we need to know the name of the
  # column with identification of the two bead populations, but it is not
  # guaranteed to exist in the .method_args. If we don't find it, we set it to
  # the default
  .ab_col_name <- get_col_names_args(.method_args[[1]])

  if(is.null(.ab_col_name)){
    .ab_col_name <- "analyte"
  }

  # ## Identify analytes ##
  .analyte_gr12 <- .analytes |>
    purrr::map2_df(.y = names(.analytes), .f = ident_bead_pop,
                   .column_name =.ab_col_name,
                   .call_args = .method_args[[length(.method_args)]],
                   df = df)

  # ## Add trimmed data points ##
  # We need to add the excluded points of the forward side scatter back, we need
  # to know the column name of the analytes, so we can add this with the analyte
  # ID NA
  .id_col_name <- get_col_names_args(.method_args[[length(.method_args)]])
  if(is.null(.id_col_name)){
    .id_col_name <- "analyte"
  }

  df |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(.ab_col_name),
        .fns = is.na
      )
    ) |>
    dplyr::mutate({{.id_col_name}} := as.character(NA)) |>
    dplyr::bind_rows(.analyte_gr12)
}

#' @rdname identify_assay_analyte
#'
#' @param .trim_fs A numeric between 0 and 1, giving the fraction of points to
#'   remove from the forward side scatter.
#' @param .parameter_fs A character giving the names of the forward and side
#'   scatter parameters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' data(simplex)
#'
#' df <- simplex[["cba"]]
#'
#' analytes <- vector("list", 30) |> setNames(as.character(c(1:30)))
#'
#' args_ident_analyte <- list(.parameter = c("APC", "APC-Cy7"),
#'                            .column_name = "Analyte ID",
#'                            .trim = 0.1,
#'                            .method = "clara")
#' annot_events <- identify_cba_analyte(df = df,
#'                      .analytes = analytes,
#'                      .method_args = args_ident_analyte)
#'
#' annot_events |> facs_plot(.x = "FSC", .y = "SSC")
#'
#' annot_events |>
#'   facs_plot(.x = "APC", .y = "APC-Cy7", .beads = "Analyte ID")
#'
#' annot_events <- identify_cba_analyte(df = df,
#'                      .analytes = analytes,
#'                      .method_args = args_ident_analyte,
#'                      .trim_fs = 0.1,
#'                      .parameter_fs = c("FSC", "SSC"))
#'
#' annot_events |> facs_plot(.x = "FSC", .y = "SSC", .beads = "Bead events")
#'
#' # Looks strange because some true beads events have randomly been placed far
#' # from the center in the forward-side scatter when the data was created
#' annot_events |>
#'   facs_plot(.x = "APC", .y = "APC-Cy7", .beads = "Analyte ID")
#' }
identify_cba_analyte <- function(df, .analytes, .method_args, .trim_fs = NULL, .parameter_fs = NULL, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "identify_cba_analyte")
    df <- .data
  }

  identify_cba_macsplex_analyte(df = df,
                                .analytes = .analytes,
                                .method_args = .method_args,
                                .trim_fs = .trim_fs,
                                .parameter_fs = .parameter_fs)
}

#' @rdname identify_assay_analyte
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' data(simplex)
#'
#' df <- simplex[["mplex"]]
#' analytes <- vector("list", 10) |> setNames(as.character(c(1:10)))
#'
#' args_ident_analyte <- list(.parameter = c("FITC", "PE"),
#'                            .column_name = "Analyte ID",
#'                            .trim = 0.1,
#'                            .method = "clara")
#'
#' annot_events <- identify_macsplex_analyte(df = df,
#'                                      .analytes = analytes,
#'                                      .method_args = args_ident_analyte)
#'
#' annot_events |> facs_plot(.x = "FSC", .y = "SSC")
#'
#' annot_events |>
#'   facs_plot(.x = "FITC", .y = "PE", .beads = "Analyte ID")
#'
#' annot_events <- identify_macsplex_analyte(df = df,
#'                                      .analytes = analytes,
#'                                      .method_args = args_ident_analyte,
#'                                      .trim_fs = 0.1,
#'                                      .parameter_fs = c("FSC", "SSC"))
#'
#' annot_events |> facs_plot(.x = "FSC", .y = "SSC", .beads = "Bead events")
#' # Looks strange because some true beads events have randomly been placed far
#' # from the center in the forward-side scatter when the data was created
#' annot_events |>
#'   facs_plot(.x = "FITC", .y = "PE", .beads = "Analyte ID")
#' }
identify_macsplex_analyte <- function(df, .analytes, .method_args, .trim_fs = NULL, .parameter_fs = NULL, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "identify_macsplex_analyte")
    df <- .data
  }
  identify_cba_macsplex_analyte(df = df,
                                .analytes = .analytes,
                                .method_args = .method_args,
                                .trim_fs = .trim_fs,
                                .parameter_fs = .parameter_fs)
}

#' @keywords internal
identify_cba_macsplex_analyte <- function(df, .analytes, .method_args, .trim_fs = NULL, .parameter_fs = NULL, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "identify_cba_macsplex_analyte")
    df <- .data
  }
  if(!is.null(.trim_fs) & is.null(.parameter_fs)){
    stop("To trim on forward/side scatter I need to know the names of the parameters.")
  }

  if(!is.null(.trim_fs) & length(.parameter_fs) != 2){
    stop("To trim on forward/side scatter I need to know the names of both parameters.")
  }

  # Register non-accepted events
  .fs_col_name <- "Bead events"
  df[[.fs_col_name]] <- "1"

  if(!is.null(.trim_fs)){
    df <- df |>
      trim_population(.parameter = .parameter_fs, .column_name = .fs_col_name, .trim = .trim_fs)
  }

  # Extract non-accepted events
  .na_events  <- df |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(.fs_col_name),
        .fns = is.na
      )
    )

  # Get just real events
  df <- df |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of(.fs_col_name),
        .fns = ~!is.na(.)
      )
    )

  # Do bead identification
  df <- ident_bead_pop(.analytes = names(.analytes),.call_args = .method_args, df = df)

  # Find the column with the bead identification
  .analyte_col_name <- get_col_names_args(.method_args)

  if(is.null(.analyte_col_name)){
    .analyte_col_name <- "analyte"
  }

  # Add back trimmed events
  if(nrow(.na_events) > 0){
    .na_events[[.analyte_col_name]] <- NA

    df <- df |>
      dplyr::bind_rows(.na_events)
  }

  if(is.null(.trim_fs)){
    df[[.fs_col_name]] <- NULL
  }

  df
}


#' Identify analyte
#'
#' @inheritParams cluster_events
#' @param .analyte_id A character vector giving the ID of the analyte.
#'   The **order** is important and must match the expected order of analytes.
#' @param .desc A boolean to indicate if the centers of the analytes should be
#'   arranged in a descending fashion before assigning the names.
#' @param .method A character giving the clustering method to use.
#' @param ... Additional arguments passed to appropriate methods, see below.
#'
#' @section Additional parameters:
#' @inheritSection cluster_events Additional parameters
#'
#' @details
#' This function is a wrapper around the process of:
#'
#'   * Finding analyte clusters
#'   * Trimming the clusters by removing the cluster members most distant from
#'     the cluster center
#'   * Sorting the analyte clusters based on their centers
#'   * Giving each analyte cluster a useful name
#'
#' @return A data.frame with analyte IDs in a separate column
#'
#' @seealso [cluster_events()]
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' df <- lplex[[1]]
#' df |>
#'   identify_analyte(.parameter = c("FSC-A", "SSC-A"),
#'                       .analyte_id = c("A", "B"),
#'                       .column_name = "analyte",
#'                       .method = "clara", .trim = 0.02) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = analyte) +
#'   geom_point()
#'
#' df |>
#'   identify_analyte(.parameter = c("FSC-A", "SSC-A"),
#'                       .analyte_id = c("A", "B"),
#'                       .column_name = "analyte",
#'                       .method = "clara", .desc = TRUE) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = analyte) +
#'   geom_point()
#'
#' df |>
#'   identify_analyte(.parameter = c("FSC-A", "SSC-A"),
#'                       .analyte_id = c("A", "B"),
#'                       .column_name = "analyte",
#'                       .method = "dbscan") |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = analyte) +
#'   geom_point()
#' }
identify_analyte <-  function(df,
                                .parameter,
                                .analyte_id,
                                .column_name = "analyte",
                                .k = length(.analyte_id),
                                .trim = 0,
                                .desc = FALSE,
                                .method = c("clara", "kmeans", "dbscan", "mclust", "density_cut"),
                              .data = NULL,
                                ...) {

  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "identify_analyte")
    df <- .data
  }

  .method <- match.arg(.method)
  # Suggestions for further improvement
  # Watershed on binary (high resolution) matrix

  # We need to assign real cluster names to the cluster numbers later on, so we
  # just store these numbers in a temporary column based on the final column
  # name
  .cluster_column_name <- paste0("cluster_", .column_name)

  .clust_res <- switch(.method,
         clara = bp_clara(df, .parameter, .column_name = .cluster_column_name, .trim = .trim, .k = .k, ...),
         kmeans = bp_kmeans(df, .parameter, .column_name = .cluster_column_name, .trim = .trim, .k = .k, ...),
         mclust = bp_mclust(df, .parameter, .column_name = .cluster_column_name, .trim = .trim, .k = .k, ...),
         density_cut = bp_density_cut(df, .parameter, .column_name = .cluster_column_name, .trim = .trim, .k = .k),
         dbscan = bp_dbscan(df, .parameter, .column_name = .cluster_column_name, ...)
  )

  .num_clust_found <- .clust_res[[.cluster_column_name]]
  .num_clust_found <- .num_clust_found[!is.na(.num_clust_found)] |> unique() |> length()

  if(.num_clust_found != length(.analyte_id)){
    warning("The number of identified and expected clusters did not match. Setting everything to NA")

    .clust_res <- .clust_res |>
      dplyr::select(-dplyr::all_of(.cluster_column_name)) |>
      dplyr::mutate(!!.column_name := as.character(NA))

    return(.clust_res)
  }

  .clust_res |> assign_analyte_id(.parameter = .parameter,
                                      .analyte_id = .analyte_id,
                                      .column_name = .column_name,
                                      .cluster_column_name = .cluster_column_name,
                                      .desc = .desc)
}

#' Assign analyte ID
#'
#' Replace internal cluster IDs with informative analyte IDs
#'
#' @param df The tidy data.frame, with indication of clusters
#' @param .parameter The parameter to order the cluster centers by
#' @param .analyte_id A character vector giving the name of the clusters.
#'   The **order** is important and must match the expected order of clusters.
#' @param .column_name A character giving the name of the column to hold the
#'   analyte ID. If the column exists it will be silently dropped.
#' @param .cluster_column_name A character giving the name of the column where
#'   the clusters are identified. Will be dropped from the data.frame.
#' @param .desc A boolean giving whether the sort order is descending.
#' @param .data Deprecated. Use `df`.
#'
#' @return A _data.frame_ with cluster names instead of cluster ids.
#'
#' @keywords internal
#'
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' library(ggplot2)
#'
#' data("lplex")
#'
#' df <- lplex[[1]] |>
#'   bp_clara(.parameter = c("FSC-A", "SSC-A"), .column_name = "analyte", .k = 2)
#'
#' df |>
#'   beadplexr:::assign_analyte_id(.parameter = c("FSC-A", "SSC-A"),
#'                                    .analyte_id = c("A", "B"),
#'                                    .column_name = "pop name",
#'                                    .cluster_column_name = "analyte") |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = `pop name`) +
#'   geom_point()
#'
#' df |>
#'   beadplexr:::assign_analyte_id(.parameter = c("FSC-A", "SSC-A"),
#'                                    .analyte_id = c("A", "B"),
#'                                    .column_name = "pop name",
#'                                    .cluster_column_name = "analyte", .desc = TRUE) |>
#'   ggplot() +
#'   aes(x = `FSC-A`, y = `SSC-A`, colour = `pop name`) +
#'   geom_point()
#' }
#'
assign_analyte_id <- function(df,
                              .parameter,
                              .analyte_id,
                              .column_name,
                              .cluster_column_name = paste0("cluster_", .column_name),
                              .desc = FALSE,
                              .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "assign_analyte_id")
    df <- .data
  }

  col_names_present <- colnames(df)
  expected_col_names <- c(.parameter, .cluster_column_name)
  col_name_present <- expected_col_names %in% col_names_present

  if(FALSE %in% col_name_present){
    missing_cols <- paste(expected_col_names[!col_name_present], collapse = ", ")
    err_str <- paste("I could not find the column(s):", missing_cols)
    stop(err_str)
  }

  if(.column_name %in% col_names_present){
    df <- df |>
      dplyr::select(-dplyr::all_of(.column_name))
  }

  clust_order <-
    df |>
      dplyr::filter(
        dplyr::if_any(
          dplyr::all_of(.cluster_column_name),
          .fns = ~!is.na(.)
        )) |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            .cluster_column_name))) |>
      dplyr::summarise(dplyr::across(dplyr::all_of(.parameter), mean), .groups = "drop") |>
      dplyr::arrange(dplyr::across(dplyr::all_of(.parameter))) |>
      {\(x) if(.desc){
        x[nrow(x):1, ]
      }else{
        x
      }}() |>
      dplyr::mutate({{.column_name}} := .analyte_id)

  clust_order <-
    clust_order |>
    dplyr::select(dplyr::one_of(.cluster_column_name, .column_name))

  df |>
    dplyr::left_join(clust_order, by = .cluster_column_name) |>
    dplyr::select(-dplyr::all_of(.cluster_column_name))
}

#' Identify bead populations
#'
#' Convenience function to identify analytes in a subset
#'
#' @param .analytes A vector or list giving the IDs of the analytes.
#' @param .column_name A character giving the name of the column to subset by.
#' @param .cluster A character of the length of one giving the element to subset
#'   by.
#' @param .call_args A list giving the parameters passed on to `identify_analyte()`.
#' @param df A tidy data.frame.
#' @param .data Deprecated. Use `df`.
#'
#' @description
#' This is a convenience function which allows to subset the data before calling
#' [identify_analyte()]. The data is subset only if `.column_name` and
#' `.cluster` are given. Otherwise, the function is identical to calling
#' [identify_analyte()] directly.
#'
#' @return A data.frame.
#'
#' @keywords internal
#'
#' @examples
#'
#' library(beadplexr)
#' data("lplex")
#' df <- lplex[[1]]
#'
#' panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")
#' analytes <- panel_info$analytes
#' call_args <-
#'   list(.parameter = c("FSC-A", "SSC-A"),
#'        .column_name = "Bead group",
#'        .trim = 0.1,
#'        .method = "clara")
#'
#' ident_bead_pop(.analytes = analytes, .call_args = call_args, df = df)
#'
ident_bead_pop <- function(.analytes, .column_name = NULL, .cluster = NULL, .call_args, df, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "ident_bead_pop")
    df <- .data
  }

  if((!is.null(.cluster) & is.null(.column_name)) | (is.null(.cluster) & !is.null(.column_name))){
    stop("Both .cluster and .column_name must be NULL or have a value")
  }

  if(length(.cluster) > 1){
    .cluster <- .cluster[1]
  }

  if(inherits(.analytes, "list")){
    .analytes <- names(.analytes)
  }

  if(!is.null(.cluster)){
    df  <- df |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::all_of(.column_name), ~ . == .cluster
        )
      )
  }

  .call_args[[".analyte_id"]] <- .analytes
  .call_args[["df"]] <- df

  do.call(identify_analyte, .call_args)
}

#' Get column names from the method arguments
#'
#' @param .list
#'
#' @return a character with the column names. If an element names .column_name
#'   is *not* present in the `.list`, an empty vector is returned.
#'
#' @keywords internal
#'
#' @examples
#' library(beadplexr)
#'
#' list(.column_name = "XXX") |> beadplexr:::get_col_names_args()
#' list(A = list(.column_name = "XXX")) |> beadplexr:::get_col_names_args()
#' list(A = list(.column_name = "Inner"), .column_name = "Outer") |> beadplexr:::get_col_names_args()
#' list(A = "ccc") |> beadplexr:::get_col_names_args()
#'
get_col_names_args <- function(.list){
  .elem_names <- names(.list)

  .col_name_elems <- grep(".column_name", .elem_names)

  if(length(.col_name_elems) > 0){
    return(.list[.col_name_elems] |>
             purrr::list_c())
  }else{
    .list <- .list |> unlist(recursive = FALSE)
    if(inherits(.list, "list")){
      get_col_names_args(.list)
    }else{
      return(c())
    }
  }
}
