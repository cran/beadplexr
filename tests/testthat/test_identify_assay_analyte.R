exclude_dbscan <- FALSE

# Preparation -------------------------------------------------------------
library(beadplexr)
data("lplex")
data("simplex")

df <- lplex[[1]]

args_ident_analyte <- list(fs = list(.parameter = c("FSC-A", "SSC-A"),
                                     .column_name = "Bead group",
                                     .trim = 0.2),
                           analytes = list(.parameter = "FL6-H",
                                           .column_name = "Analyte ID"))
panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")


# ident_bead_pop() --------------------------------------------------------
test_that("ident_bead_pop() works",{

  expect_s3_class(beadplexr:::ident_bead_pop(.analytes = names(panel_info$analytes),
                           .call_args = args_ident_analyte[[1]],
                           df = df), "data.frame")

  expect_s3_class(beadplexr:::ident_bead_pop(.analytes = panel_info$analytes,
                           .call_args = args_ident_analyte[[1]],
                           df = df), "data.frame")


  annot_events <- beadplexr:::ident_bead_pop(.analytes = panel_info$analytes,
                                 .call_args = args_ident_analyte[[1]],
                                 df = df)

  expect_equal(names(annot_events), c("FSC-A", "SSC-A", "FL6-H", "FL2-H", "Bead group"))

  .tmp_data <- df
  .tmp_data$BeadID <- rep(c("A", "B"), nrow(.tmp_data)/2)

  expect_s3_class(beadplexr:::ident_bead_pop(.analytes = panel_info$analytes,
                              .column_name = "BeadID",
                              .cluster = c("A", "B"),
                              .call_args = args_ident_analyte[[1]],
                              df = .tmp_data), "data.frame")

  expect_error(beadplexr:::ident_bead_pop(.analytes = panel_info$analytes,
                           .column_name = "a column",
                           .cluster = c("A", "B"),
                           .call_args = args_ident_analyte[[1]],
                           df = df))

  expect_error(beadplexr:::ident_bead_pop(.analytes = names(panel_info$analytes),
                                .column_name = "XXX",
                                .call_args = args_ident_analyte[[1]],
                                df = df))
  expect_error(beadplexr:::ident_bead_pop(.analytes = names(panel_info$analytes),
                              .cluster = "XXX",
                              .call_args = args_ident_analyte[[1]],
                              df = df))
})

# get_col_names_args() ----------------------------------------------------
test_that(".column_name is identified in method argument list",{
  expect_equal(beadplexr:::get_col_names_args(list(.column_name = "XXX")), "XXX")
  expect_equal(beadplexr:::get_col_names_args(list(A = list(.column_name = "XXX"))), "XXX")
  expect_equal(beadplexr:::get_col_names_args(list(A = list(.column_name = "Inner"), .column_name = "Outer")), "Outer")
  expect_equal(beadplexr:::get_col_names_args(list(A = "ccc")), NULL)
})

# identify_legendplex_analyte() -------------------------------------------
test_that("Legendplex works with column_names in .method_args",{
  expect_s3_class(identify_legendplex_analyte(df = df,
                                        .analytes = panel_info$analytes,
                                        .method_args = args_ident_analyte), "data.frame")

  annot_events <- identify_legendplex_analyte(df = df,
                                              .analytes = panel_info$analytes,
                                              .method_args = args_ident_analyte)

  # Test columns are as expected
  expect_equal(names(annot_events), c("FSC-A", "SSC-A", "FL6-H", "FL2-H", "Bead group", "Analyte ID"))

  # Test clusters are as expected
  expect_true("A" %in% annot_events[["Bead group"]])
  expect_true("B" %in% annot_events[["Bead group"]])
  expect_true(NA %in% annot_events[["Bead group"]])

  expect_true(TRUE %in% grepl("A", annot_events[["Analyte ID"]]))
  expect_true(TRUE %in% grepl("B", annot_events[["Analyte ID"]]))
})

test_that("Legendplex works without column_names in .method_args",{
  args_ident_analyte <- list(fs = list(.parameter = c("FSC-A", "SSC-A"),
                                       .trim = 0.2),
                             analytes = list(.parameter = "FL6-H"))

  expect_s3_class(identify_legendplex_analyte(df = df,
                                        .analytes = panel_info$analytes,
                                        .method_args = args_ident_analyte), "data.frame")

  annot_events <- identify_legendplex_analyte(df = df,
                                              .analytes = panel_info$analytes,
                                              .method_args = args_ident_analyte)

  # Test columns are as expected
  expect_equal(ncol(annot_events), 5)
  expect_true("analyte" %in% names(annot_events))
  expect_equal(names(annot_events), c("FSC-A", "SSC-A", "FL6-H", "FL2-H", "analyte"))

  # There exist only one colum with analyte IDs
  # The major grouping is gone
  expect_false("A" %in% annot_events[["analyte"]])
  expect_false("B" %in% annot_events[["analyte"]])

  expect_true(TRUE %in% grepl("A", annot_events[["analyte"]]))
  expect_true(TRUE %in% grepl("B", annot_events[["analyte"]]))
})

# dbscan works as expected ------------------------------------------------
dbscan_num_clusters <- function(df, eps, MinPts){
  dbc <- fpc::dbscan(df, eps = eps, MinPts = MinPts)
  dbc <- dbc$cluster
  max(dbc)
}

test_that("dbscan works as expected", {
  if(exclude_dbscan){
    skip("In-depth test of bp_dbscan")
  }else{
    expect_warning(identify_analyte(df, .parameter = c("FSC-A", "SSC-A"),
                                    .analyte_id = c("A"), .method = "dbscan"))

    expect_s3_class(.analyte_set <- identify_analyte(df, .parameter = c("FSC-A", "SSC-A"),
                                               .analyte_id = c("A", "B"), .method = "dbscan",
                                               .column_name = "Bead group"), "data.frame")

    .bead_a <- dplyr::filter(.analyte_set, `Bead group` == "A")
    .bead_b <- dplyr::filter(.analyte_set, `Bead group` == "B")

    expect_equal(dbscan_num_clusters(.bead_a[, c("FL6-H", "FL2-H")], eps = 0.12, MinPts = 110), 6)
    expect_equal(dbscan_num_clusters(.bead_b[, c("FL6-H", "FL2-H")], eps = 0.2, MinPts = 110), 7)

    expect_warning(identify_analyte(.bead_a, .parameter = c("FL6-H", "FL2-H"),
                                    .analyte_id = "A1", .method = "dbscan",
                                    .column_name = "Analyte ID", .eps = 0.12, .MinPts = 110))

    expect_warning(identify_analyte(.bead_b, .parameter = c("FL6-H", "FL2-H"),
                                    .analyte_id = "A1", .method = "dbscan",
                                    .column_name = "Analyte ID", .eps = 0.2, .MinPts = 110, scale = FALSE))

    expect_silent(identify_analyte(.bead_a, .parameter = c("FL6-H", "FL2-H"),
                                   .analyte_id = names(panel_info$analytes$A),
                                   .method = "dbscan",
                                   .column_name = "Analyte ID",
                                   .eps = 0.12,
                                   .MinPts = 110, scale = FALSE))

    expect_silent(identify_analyte(df = .bead_b, .parameter = c("FL6-H", "FL2-H"),
                                   .analyte_id = names(panel_info$analytes$B),
                                   .method = "dbscan",
                                   .column_name = "Analyte ID",
                                   .eps = 0.2,
                                   .MinPts = 110, scale = FALSE))
  }
})

# Legendplex recalculation ------------------------------------------------

test_that("Recalculation is possible", {
  annot_events <- identify_legendplex_analyte(df = df,
                                              .analytes = panel_info$analytes,
                                              .method_args = args_ident_analyte)
  expect_s3_class(identify_legendplex_analyte(df = annot_events,
                                        .analytes = panel_info$analytes,
                                        .method_args = args_ident_analyte), "data.frame")

  annot_events <- identify_legendplex_analyte(df = df,
                                              .analytes = panel_info$analytes,
                                              .method_args = args_ident_analyte)

  # Test columns are as expected
  expect_equal(ncol(annot_events), 6)
  expect_true("Bead group" %in% names(annot_events))
  expect_true("Analyte ID" %in% names(annot_events))

  # Test clusters are as expected
  expect_true("A" %in% annot_events[["Bead group"]])
  expect_true("B" %in% annot_events[["Bead group"]])
  expect_true(NA %in% annot_events[["Bead group"]])

  expect_true(TRUE %in% grepl("A", annot_events[["Analyte ID"]]))
  expect_true(TRUE %in% grepl("B", annot_events[["Analyte ID"]]))
})

# identify_cba_macsplex_analyte() --------------------------------------------------
df <- simplex[["cba"]]
analytes <- vector("list", 30) |> setNames(as.character(c(1:30)))
args_ident_analyte <- list(.parameter = c("APC", "APC-Cy7"),
                           .column_name = "Analyte ID",
                           .method = "clara")
test_that("CBS/MACSplex works -- no FS trimming",{
  expect_s3_class(identify_cba_macsplex_analyte(df = df,
                                 .analytes = analytes,
                                 .method_args = args_ident_analyte), "data.frame")

  annot_events <- identify_cba_macsplex_analyte(df = df,
                                              .analytes = analytes,
                                              .method_args = args_ident_analyte)

  # Test columns are as expected
  expect_equal(ncol(annot_events), 6)
  expect_true("Analyte ID" %in% names(annot_events))

  # Test clusters are as expected
  expect_type(annot_events[["Analyte ID"]], "character")
})

test_that("CBS/MACSplex works -- with FS trimming",{
  expect_s3_class(identify_cba_macsplex_analyte(df = df,
                                 .analytes = analytes,
                                 .method_args = args_ident_analyte,
                                 .trim_fs = 0.1, .parameter_fs = c("FSC", "SSC")), "data.frame")

  annot_events <- identify_cba_macsplex_analyte(df = df,
                                       .analytes = analytes,
                                       .method_args = args_ident_analyte,
                                       .trim_fs = 0.1, .parameter_fs = c("FSC", "SSC"))

  # Test columns are as expected
  expect_equal(ncol(annot_events), 7)
  expect_true("Bead events" %in% names(annot_events))
  expect_true("Analyte ID" %in% names(annot_events))

  # Test clusters are as expected
  expect_type(annot_events[["Bead events"]], "character")
  expect_type(annot_events[["Analyte ID"]], "character")
})

test_that("CBS/MACSplex fails",{
  expect_s3_class(identify_cba_macsplex_analyte(df = df,
                                          .analytes = analytes,
                                          .method_args = args_ident_analyte,
                                          .trim_fs = NULL, .parameter_fs = c("FSC", "SSC")), "data.frame")

  expect_error(identify_cba_macsplex_analyte(df = df,
                                             .analytes = analytes,
                                             .method_args = args_ident_analyte,
                                             .trim_fs = 0.1, .parameter_fs = NULL))

  expect_error(identify_cba_macsplex_analyte(df = df,
                                             .analytes = analytes,
                                             .method_args = args_ident_analyte,
                                             .trim_fs = 0.1, .parameter_fs = c("FSC")))

  expect_error(identify_cba_macsplex_analyte(df = df,
                                             .analytes = analytes,
                                             .method_args = args_ident_analyte,
                                             .trim_fs = 0.1, .parameter_fs = c("FSCx", "SSCx")))
})
