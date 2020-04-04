context("Identify analytes")

# Preparation -------------------------------------------------------------
data("lplex")
df <- lplex[[1]]
.parameters <- c("FSC-A", "SSC-A")

# identify_analyte() ---------------------------------------------------
test_that("Identify analyte works", {
  expect_is(identify_analyte(df = df,
                                .parameter = c("FSC-A", "SSC-A"),
                                .analyte_id = c("A", "B"),
                                .column_name = "ana lyte",
                                .method = "clara", .trim = 0.02), "data.frame")
  expect_is(identify_analyte(df = df,
                                .parameter = c("FSC-A", "SSC-A"),
                                .analyte_id = c("A", "B"),
                                .column_name = "analyte",
                                .method = "kmeans", .trim = 0.02), "data.frame")
  expect_is(identify_analyte(df = df,
                                .parameter = c("FSC-A", "SSC-A"),
                                .analyte_id = c("A", "B"),
                                .column_name = "analyte",
                                .method = "dbscan"), "data.frame")
})

test_that("Identify analyte give warnings", {
  expect_warning(identify_analyte(df = df,
                      .parameter = c("FSC-A", "SSC-A"),
                      .analyte_id = c("A", "B"),
                      .column_name = "analyte",
                      .method = "clara", .k = 4))
})


# assign_analyte_id() -----------------------------------------------------

df <- bp_clara(df, .parameter = c("FSC-A", "SSC-A"), .column_name = "analyte", .k = 2)

test_that("AnalyteIDs are assigned", {
  expect_is(
    assign_analyte_id(
      df = df,
      .parameter = c("FSC-A", "SSC-A"),
      .analyte_id = c("A", "B"),
      .column_name = "pop name",
      .cluster_column_name = "analyte"
    ),
    "data.frame"
  )
  expect_is(
    assign_analyte_id(
      df = df,
      .parameter = c("FSC-A", "SSC-A"),
      .analyte_id = c("A", "B"),
      .column_name = "pop name",
      .cluster_column_name = "analyte",
      .desc = TRUE
    ),
    "data.frame"
  )
  expect_true("pop name" %in% names(
    beadplexr:::assign_analyte_id(
      df = df,
      .parameter = c("FSC-A", "SSC-A"),
      .analyte_id = c("A", "B"),
      .column_name = "pop name",
      .cluster_column_name = "analyte",
      .desc = TRUE
    )
  ))
  expect_false("analyte" %in% names(
    beadplexr:::assign_analyte_id(
      df = df,
      .parameter = c("FSC-A", "SSC-A"),
      .analyte_id = c("A", "B"),
      .column_name = "pop name",
      .cluster_column_name = "analyte",
      .desc = TRUE
    )
  ))
})

test_that("AnalyteID column is overwritten", {
  df2 <- beadplexr:::assign_analyte_id(df = df,
                                          .parameter = c("FSC-A", "SSC-A"),
                                          .analyte_id = c("A", "B"),
                                          .column_name = "pop name",
                                          .cluster_column_name = "analyte")

  df3 <- bp_clara(df2, .parameter = c("FSC-A", "SSC-A"), .column_name = "analyte", .k = 2)

  expect_is(beadplexr:::assign_analyte_id(df = df3,
                                          .parameter = c("FSC-A", "SSC-A"),
                                          .analyte_id = c("A", "B"),
                                          .column_name = "pop name",
                                          .cluster_column_name = "analyte"), "data.frame")
  expect_identical(beadplexr:::assign_analyte_id(df = df3,
                                                 .parameter = c("FSC-A", "SSC-A"),
                                                 .analyte_id = c("A", "B"),
                                                 .column_name = "pop name",
                                                 .cluster_column_name = "analyte"), df2)
})
