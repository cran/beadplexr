#' Plot FACS data.
#'
#' Wrappers around building a ggplot with \code{geom_point},
#' \code{geom_density_2d}, and \code{geom_hex}.
#'
#' @param df The data to be plotted in a \code{data.frame}.
#' @param .x,.y Character vector with the column name for the variable to plot
#'   on the x or y-axis.
#' @param .beads Character vector to with the column name with identification of
#'   beads. If used it will show up with the aesthetic 'color'. Defaults to not
#'   being used.
#' @param .plot_distinct Boolean to decide if only distinct events should be
#'   plotted. If used, the number of data points might be greatly reduced which
#'   could make for faster plotting. Defaults to TRUE.
#' @param .bins Numeric vector giving number of bins in both vertical and
#'   horizontal directions. Set to 75 by default.
#' @param .type Character vector giving the type of plot being used. Options are
#'   one of \code{"scatter", "density", "hexbin"}.
#' @param .data Deprecated. Use `df`.
#' @param ... Arguments passed to the individual functions.
#'
#' @details
#' These plot functions are meant to provide a quick way of viewing the FACS
#' data. For more control, use \code{ggplot2} directly.
#'
#' @return A \code{ggplot}
#' @export
#'
#' @examples
#' \dontrun{
#' library(beadplexr)
#' data("lplex")
#'
#' df <-  lplex[[1]]
#' df$bead_group <- ifelse(df$`FSC-A` < 4e5L, "A", "B")
#'
#' # Using facs_plot
#' facs_plot(df, .type = "scatter")
#' facs_plot(df, .type = "density1d")
#' facs_plot(df, .type = "density2d")
#' facs_plot(df, .type = "hexbin")
#'
#' facs_plot(df, .type = "scatter", .beads = "bead_group")
#' facs_plot(df, .type = "density1d", .beads = "bead_group")
#' facs_plot(df, .type = "hexbin", .bins = 50)
#'
#' facs_plot(df, .x = "FL2-H", .type = "scatter", .beads = "bead_group")
#'
#' # Individual functions
#' facs_scatter(df)
#'
#' facs_scatter(df, .beads = "bead_group", .plot_distinct = FALSE)
#' facs_scatter(df, .beads = "bead_group")
#'
#' facs_scatter(df, .x = "FL2-H", .y = "FL6-H", .beads = "bead_group")
#'
#' facs_density1d(df)
#' facs_density1d(df, .beads = "bead_group")
#'
#' facs_density2d(df)
#' facs_density2d(df, .beads = "bead_group")
#'
#' facs_hexbin(df)
#' facs_hexbin(df, .bins = 30)
#' }
facs_plot <- function(df, .x = "FSC-A", .y = "SSC-A", .type = c("scatter", "density1d", "density2d", "hexbin"), .data = NULL, ...){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "facs_plot")
    df <- .data
  }
  .type <- match.arg(.type)
  switch(.type,
         scatter = facs_scatter(df = df, .x = .x, .y = .y, ...),
         density1d = facs_density1d(df = df, .x = .x, ...),
         density2d = facs_density2d(df = df, .x = .x, .y = .y, ...),
         hexbin = facs_hexbin(df = df, .x = .x, .y = .y, ...))
}

#' @rdname facs_plot
#' @export
facs_scatter <- function(df, .x = "FSC-A", .y = "SSC-A", .beads = NULL, .plot_distinct = TRUE, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "facs_scatter")
    df <- .data
  }

  df <- df |> dplyr::ungroup()

  if(.plot_distinct){
    df <- df |>
      dplyr::select(
        dplyr::all_of(c(.x, .y)),
        # .beads may not exist
        dplyr::any_of(.beads)

      ) |>
      dplyr::distinct()
  }

  tmp_plot <- df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.x]], y = .data[[.y]]) +
    ggplot2::geom_point(size = 0.5)

  if(!is.null(.beads)){
    tmp_plot <- tmp_plot + ggplot2::aes(colour = .data[[.beads]])
  }
  tmp_plot
}

#' @rdname facs_plot
#' @export
facs_density2d <- function(df, .x = "FSC-A", .y = "SSC-A", .beads = NULL, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "facs_density2d")
    df <- .data
  }

  df <- df |> dplyr::ungroup()

  tmp_plot <- df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.x]], y = .data[[.y]]) +
    ggplot2::geom_density_2d()

    if(!is.null(.beads)){
      tmp_plot <- tmp_plot + ggplot2::aes(colour = .data[[.beads]])
    }
  tmp_plot
}

#' @rdname facs_plot
#' @export
facs_density1d <- function(df, .x = "FSC-A", .beads = NULL, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "facs_density1d")
    df <- .data
  }

  df <- df |> dplyr::ungroup()

  tmp_plot <- df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.x]]) +
    ggplot2::geom_density()

  if(!is.null(.beads)){
    tmp_plot <- tmp_plot + ggplot2::aes(fill = .data[[.beads]])
  }
  tmp_plot
}

#' @rdname facs_plot
#' @export
facs_hexbin <- function(df, .x = "FSC-A", .y = "SSC-A", .bins = 75, .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "facs_hexbin")
    df <- .data
  }

  df <- df |> dplyr::ungroup()
  df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.x]], y = .data[[.y]]) +
    ggplot2::geom_hex(bins = .bins) +
    ggplot2::scale_fill_gradientn(colours = c("blue", "green", "yellow", "red")) +
    ggplot2:: theme(legend.position = "none")
}

#' Plot concentrations
#'
#' @param df A `data.frame` with the data to be plotted.
#' @param .sample_data A `data.frame` with the calculated sample concentrations.
#' @param .standard_data A `data.frame` with the calculated standard
#'   concentrations.
#' @param .model An object of class `drc` with the fitted dose-response model.
#' @param .title A character giving the title of the plot.
#' @param .parameter A character giving the name of the column with the MFI
#' @param .concentration  A character giving the name of the column with the with the calculated concentrations.
#' @param .std_concentration A character giving the name of the column with the standard concentration.
#' @param .data Deprecated. Use `df`.
#'
#' @return A \code{ggplot}
#' @export
#'
#'
#' @name plot_concentrations
#'
#' @examples
#'
#' library(beadplexr)
#' library(drc)
#' data(ryegrass)
#'
#' ryegrass_m <-
#'   fit_standard_curve(df = ryegrass,
#'                      .parameter = "rootl",
#'                      .concentration = "conc")
#' recalc_std <-
#'   calculate_concentration(df = ryegrass,
#'                           .model = ryegrass_m,
#'                           .parameter = "rootl")
#' sample_data <-
#'   calculate_concentration(df = ryegrass[sample(1:nrow(ryegrass), 5),],
#'                           .model = ryegrass_m,
#'                           .parameter = "rootl")
#'
#' plot_std_curve(ryegrass,
#'                ryegrass_m,
#'                .parameter = "rootl",
#'                .concentration = "conc")
#'
#' plot_target_est_conc(df = recalc_std,
#'                      .concentration = "Calc.conc",
#'                      .std_concentration = "conc")
#' plot_estimate(
#'   .sample_data = sample_data,
#'   .standard_data = ryegrass,
#'   .model = ryegrass_m,
#'   .parameter = "rootl",
#'   .concentration = "conc")
#'
plot_std_curve <- function(df, .model, .title = NULL,
                           .parameter = "FL2.H",
                           .concentration = "Concentration",
                           .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "plot_std_curve")
    df <- .data
  }

  # Create data for displaying fit line and interval
  fit_line_range <-
    df |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(c(.concentration)),
        .fns = ~!is.infinite(.)
      )) |>
    dplyr::pull(dplyr::all_of(.concentration)) |>
    range()

  fit_line_range <- data.frame(x = seq(min(fit_line_range), max(fit_line_range), length = 100))
  fit_line_range <- dplyr::bind_cols(
    fit_line_range,
    suppressWarnings(stats::predict(.model,
                                    newdata = fit_line_range,
                                    interval = "confidence")) |>
      as.data.frame()
  ) |>
    stats::setNames(c(.concentration, .parameter, "Lower", "Upper"))

  df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.concentration]],
                        y = .data[[.parameter]]) +
    ggplot2::geom_point()  +
    ggplot2::geom_ribbon(data = fit_line_range,
                         ggplot2::aes(ymin = .data[["Lower"]], ymax = .data[["Upper"]]), alpha=0.2) +
    ggplot2::geom_line(data = fit_line_range, colour = "blue") +
    ggplot2::labs(title = .title)
}

#' @rdname plot_concentrations
#' @export
plot_target_est_conc <- function(df, .title = NULL,
                                 .concentration = "Calc.conc",
                                 .std_concentration = "Concentration",
                                 .data = NULL){
  if(!is.null(.data)){
    raise_deprecated(old = ".data", new = "df", caller = "plot_target_est_conc")
    df <- .data
  }

  df <-
    df |>
    dplyr::filter(
      dplyr::if_all(
        dplyr::all_of( c(.std_concentration, .concentration)),
        .fns = ~!is.infinite(.) & !is.nan(.) & !is.na(.)
      ))


  .fit_formula <- stats::as.formula(paste(.concentration, .std_concentration, sep = "~"))

  lm_res <- stats::lm(formula = .fit_formula, data = df)
  lm_res_summary <- summary(lm_res)
  r_squared <- lm_res_summary$r.squared
  p_value <- 1 - stats::pf(lm_res_summary$fstatistic[1],
                    lm_res_summary$fstatistic[2],
                    lm_res_summary$fstatistic[3])
  slope <- stats::coef(lm_res_summary)[2]

  conc_min <-
    df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(.std_concentration),
        .fns = min
      ))

  conc_max <-
    df |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(.concentration),
        .fns = max
      ))

  fit_text <-
    dplyr::bind_cols(conc_min, conc_max) |>
    dplyr::mutate(label = paste("R2:", format(r_squared, digits = 3), "\n",
                                "Slope:", format(slope, digits = 3), "\n",
                                "p:", format.pval(p_value, digits = 3)))

  df |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.std_concentration]], y = .data[[.concentration]]) +
    ggplot2::geom_point() +
    ggplot2::geom_text(data = fit_text,
                       ggplot2::aes(label = .data[["label"]]),
                       vjust = 1, hjust = 0) +
    ggplot2::stat_smooth(method = "lm", formula = y ~ x) +
    ggplot2::labs(title = .title)
}

#' @rdname plot_concentrations
#' @export
plot_estimate <- function(.sample_data, .standard_data, .model, .title = NULL,
                          .parameter = "FL2.H",
                          .concentration = "Concentration"){


  # Create data for displaying fit line and interval

  fit_line_range <- .standard_data |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(.concentration),
        .fns = ~!is.infinite(.)
      )) |>
    dplyr::pull(dplyr::all_of(.concentration)) |>
    range()

  fit_line_range <- data.frame(x = seq(min(fit_line_range),
                                       max(fit_line_range),
                                       length = 100))
  fit_line_range <-  dplyr::bind_cols(
    fit_line_range,
    suppressWarnings(stats::predict(.model,
                                    newdata = fit_line_range,
                                    interval = "confidence")) |>
      as.data.frame()
  ) |>
    stats::setNames(c(.concentration, .parameter, "Lower", "Upper"))

  .standard_data |>
    ggplot2::ggplot() +
    ggplot2::aes(x = .data[[.concentration]], y = .data[[.parameter]]) +
    ggplot2::geom_ribbon(data = fit_line_range,
                         ggplot2::aes(ymin = .data[["Lower"]], ymax = .data[["Upper"]]),
                         alpha=0.2) +
    ggplot2::geom_line(data = fit_line_range, colour = "blue") +
    ggplot2::geom_hline(data = .sample_data,
                        ggplot2::aes(yintercept = .data[[.parameter]]),
                        linetype = 2) +
    ggplot2::labs(title = .title)
}
