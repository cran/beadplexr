# Preparation -------------------------------------------------------------
.file_name <- system.file("resources", "legendplex_human_t_helper_cytokine_panels_13-plex.yml", package = "beadplexr")

# Read files --------------------------------------------------------------
test_that("Panel info file can be read", {
  expect_type(load_panel(.file_name = .file_name), "list")
  expect_type(load_panel(.panel_name = "Human T Helper Cytokine Panels (13-plex)"), "list")
  expect_type(load_panel(.file_name = .file_name, .panel_name = "Human T Helper Cytokine Panels (13-plex)"), "list")
  expect_type(load_panel(.panel_pattern = "cytokine.panel.2", .file_name = .file_name), "list")

  # Odd side effect
  expect_warning(load_panel())

  # No file found
  expect_error(load_panel(.panel_pattern = "xxx"))
})

# Map panel names and files -----------------------------------------------
test_that("Filenames for all included panel files can be found", {
  expect_type(panel_name_file(.panel_name = "Human T Helper Cytokine Panels (13-plex)"), "character")
  expect_type(panel_name_file(.panel_name = "Human Cytokine Panel 2 (13-plex)"), "character")
  expect_type(panel_name_file(.panel_name = "Human Proinflammatory Chemokine Panel (13-plex)"), "character")
  expect_type(panel_name_file(.panel_name = "Human Growth Factor Panel (13-plex)"), "character")
})

# Load all files ----------------------------------------------------------
test_that("All included panel files can be read", {
  expect_type(load_panel(.panel_name = "Human T Helper Cytokine Panels (13-plex)"), "list")
  expect_type(load_panel(.panel_name = "Human Cytokine Panel 2 (13-plex)"), "list")
  expect_type(load_panel(.panel_name = "Human Proinflammatory Chemokine Panel (13-plex)"), "list")
  expect_type(load_panel(.panel_name = "Human Growth Factor Panel (13-plex)"), "list")
})

