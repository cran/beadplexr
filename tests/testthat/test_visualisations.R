context("Visualise data")

# Preparation -------------------------------------------------------------
rds_file <- system.file("extdata", "K2-C07-A7.rds", package = "beadplexr")

df <- readRDS(rds_file)
df$bead_group <- ifelse(df$`FSC-A` < 4e5L, "A", "B")

# FACS plots --------------------------------------------------------------

test_that("FACS data can be visualised", {
  # Test main wrapper function
  expect_is(facs_plot(df, .type = "scatter"), "ggplot")
  expect_is(facs_plot(df, .type = "density1d"), "ggplot")
  expect_is(facs_plot(df, .type = "density2d"), "ggplot")
  expect_is(facs_plot(df, .type = "hexbin"), "ggplot")
  expect_is(facs_plot(df, .type = "scatter", .beads = "bead_group"), "ggplot")
  expect_is(facs_plot(df, .type = "density1d", .beads = "bead_group"), "ggplot")
  expect_is(facs_plot(df, .type = "density2d", .beads = "bead_group"), "ggplot")
  expect_is(facs_plot(df, .type = "hexbin", .bins = 50), "ggplot")
  expect_is(facs_plot(df, .x = "FL2-H", .type = "scatter", .beads = "bead_group"), "ggplot")
  # Test individual functions
  expect_is(facs_scatter(df), "ggplot")
  expect_is(facs_scatter(df, .beads = "bead_group", .plot_distinct = FALSE), "ggplot")
  expect_is(facs_scatter(df, .beads = "bead_group"), "ggplot")
  expect_is(facs_scatter(df, .x = "FL2-H", .y = "FL6-H", .beads = "bead_group"), "ggplot")
  expect_is(facs_density1d(df), "ggplot")
  expect_is(facs_density1d(df, .beads = "bead_group"), "ggplot")
  expect_is(facs_density2d(df), "ggplot")
  expect_is(facs_density2d(df, .beads = "bead_group"), "ggplot")
  expect_is(facs_hexbin(df), "ggplot")
  expect_is(facs_hexbin(df, .bins = 30), "ggplot")
})
