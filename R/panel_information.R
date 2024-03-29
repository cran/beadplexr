#' Load panel information
#'
#' The panel information are stored in \code{resources} in the package directory
#' and can be loaded by providing a file or panel name, or a search pattern
#'
#' @param .file_name Character vector giving the name of an external panel info
#'   file. See below for the expected components.
#' @param .panel_name Character vector giving the name of the panel. See below
#'   for accepted panel names.
#' @param .panel_pattern The pattern to look for. Can be a regular expression.
#'
#' @details
#'
#' If an explicit \code{.file_name} is given, \code{.panel_name} and
#' \code{.panel_pattern} are ignored. If no file is given but a
#' \code{.panel_name} is, the \code{.panel_pattern} i ignored. Only if no
#' \code{.file_name} and \code{.panel_name} are given, is the
#' \code{.panel_pattern} used.
#'
#' @section Included panels:
#'
#'   \itemize{
#'    \item Human Adipokine Panel* (13-plex)
#'    \item Human Anti-Virus Response Panel (13-plex)
#'    \item Human CD8/NK Panel (13-plex)
#'    \item Human Cytokine Panel 2 (13-plex)
#'    \item Human Growth Factor Panel (13-plex)
#'    \item Human Inflammation Panel (13-plex)
#'    \item Human Metabolic Panel 1 (4-plex)
#'    \item Human Proinflammatory Chemokine Panel (13-plex)
#'    \item Human T Helper Cytokine Panels (13-plex)
#'    \item Mouse Anti-Virus Response Panel (13-plex)
#'    \item Mouse Cytokine Panel 2 (13-plex)
#'    \item Mouse Free Active/Total TGF-b1 Panel (Mouse/Rat) (1-plex)
#'    \item Mouse HSC Panel (13-plex)
#'    \item Mouse IgE Panel (1-plex)
#'    \item Mouse Immunoglobulin Isotyping Panel (6-plex)
#'    \item Mouse Inflammation Panel (13-plex)
#'    \item Mouse Proinflammatory Chemokine Panel (13-plex)
#'    \item Mouse T Helper Cytokine Panels (13-plex)
#'   }
#'
#' @section Example panel file:
#'
#'   The panel information files are formatted in YAML. It has three main parts:
#'   - Some general information about the panel - The order of the major bead
#'   groups in the forward and side scatter - Analytes, where each bead ID is
#'   listed for each major bead group. The bead IDs are further complemented
#'   with the name of the analyte and the start concentration of the standard
#'   value
#'
#'   The 'Human Th Cytokine Panel (13-plex)' YAML file is found in
#'   \code{/resources/legendplex_human_th_cytokine_panel_13-plex.yml} of the
#'   package directory.
#'
#' @return A \code{list}
#' @export
#'
#' @examples
#' library(beadplexr)
#'
#' .panel_name <- "Human T Helper Cytokine Panels (13-plex)"
#' panel_info <- load_panel(.panel_name = .panel_name)
#' panel_info$panel_name
#'
#' .file_name <- system.file("resources",
#'                           "legendplex_human_cytokine_panel_2_13-plex.yml",
#'                           package = "beadplexr")
#' panel_info <- load_panel(.file_name = .file_name)
#' panel_info$panel_name
#'
#' panel_info <- load_panel(.file_name = .file_name, .panel_name = .panel_name)
#' panel_info$panel_name
#'
#' suppressWarnings(
#' # The pattern matches several files, which raises a warning
#' panel_info <- load_panel(.panel_pattern = "panel_2_13-plex")
#' )
#' panel_info$panel_name
#'
load_panel <- function(.file_name = NULL, .panel_name = NULL, .panel_pattern = NULL){

  if(is.null(.file_name)){
    if(!is.null(.panel_name)){
      .panel_pattern <- panel_name_file(.panel_name = .panel_name)
    }

    resource_dir <- file.path(system.file(package = "beadplexr"), "resources")

    .file_name <-  list.files(path = resource_dir, pattern = .panel_pattern, full.names = TRUE)

    if(length(.file_name) == 0){
      stop("No file found")
    }
    else if(length(.file_name) > 1){
      warning("The panel name gave more than one file. Using the first and ignoring the rest")
      .file_name <- .file_name[1]
    }
  }
  yaml::yaml.load_file(.file_name)
}

#' Get file name from panel name
#'
#' Conversion from panel name to internal panel info file name
#'
#' @param .panel_name Character vector giving the name of the panel
#'
#' @return A character
#' @keywords internal
#'
#' @examples
#' beadplexr:::panel_name_file(.panel_name = "Human Th Cytokine Panel (13-plex)")
#'
panel_name_file <- function(.panel_name){
  switch(
    .panel_name,
    `Human Adipokine Panel* (13-plex)` =
      "legendplex_human_adipokine_panel_13-plex.yml",
    `Human Anti-Virus Response Panel (13-plex)` =
      "legendplex_human_anti-virus_response_panel_13-plex.yml",
    `Human CD8/NK Panel (13-plex)` =
      "legendplex_human_cd8-nk_panel_13-plex.yml",
    `Human Cytokine Panel 2 (13-plex)` =
      "legendplex_human_cytokine_panel_2_13-plex.yml",
    `Human Growth Factor Panel (13-plex)` =
      "legendplex_human_growth_factor_panel_13-plex.yml",
    `Human Inflammation Panel (13-plex)` =
      "legendplex_human_inflammation_panel_13-plex.yml",
    `Human Metabolic Panel 1 (4-plex)` =
      "legendplex_human_metabolic_panel_1_4-plex.yml",
    `Human Proinflammatory Chemokine Panel (13-plex)` =
      "legendplex_human_proinflammatory_chemokine_panel_13-plex.yml",
    `Human T Helper Cytokine Panels (13-plex)` =
      "legendplex_human_t_helper_cytokine_panels_13-plex.yml",
    `Mouse Anti-Virus Response Panel (13-plex)` =
    "legendplex_mouse_anti-virus_response_panel_13-plex.yml",
    `Mouse Cytokine Panel 2 (13-plex)` =
    "legendplex_mouse_cytokine_panel_2_13-plex.yml",
    `Mouse Free Active/Total TGF-b1 Panel (Mouse/Rat) (1-plex)` =
    "legendplex_mouse_free_active-total_tgf-b1_panel_mouse-rat_1-plex.yml",
    `Mouse HSC Panel (13-plex)` =
    "legendplex_mouse_hsc_panel_13-plex.yml",
    `Mouse IgE Panel (1-plex)` =
    "legendplex_mouse_ige_panel_1-plex.yml",
    `Mouse Immunoglobulin Isotyping Panel (6-plex)` =
    "legendplex_mouse_immunoglobulin_isotyping_panel_6-plex.yml",
    `Mouse Inflammation Panel (13-plex)` =
    "legendplex_mouse_inflammation_panel_13-plex.yml",
    `Mouse Proinflammatory Chemokine Panel (13-plex)` =
    "legendplex_mouse_proinflammatory_chemokine_panel_13-plex.yml",
    `Mouse T Helper Cytokine Panels (13-plex)` =
    "legendplex_mouse_t_helper_cytokine_panels_13-plex.yml",

    warning(
      "Panel name not found. Did you write it in full? See the documentation for a list of accepted panel names."
    )
  )
}
