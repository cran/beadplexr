# Preparation -------------------------------------------------------------
library(beadplexr)
panel_info <- load_panel(.panel_name = "Human Growth Factor Panel (13-plex)")

# Perform test ------------------------------------------------------------
test_that("Conversion of list", {
  .analytes <-  list(A = list(
    A1 = list(name = "name_a1", concentration = 500),
    A2 = list(name = "name_a2", concentration = 50000)))

  expect_s3_class(as_data_frame_analyte(.analytes), "data.frame")
  expect_s3_class(as_data_frame_analyte(panel_info$analytes), "data.frame")
})

test_that("Matrix is padded", {
  x <- matrix(c(1:9), nrow = 3, ncol = 3)
  res_x <- beadplexr:::pad_matrix(x)

  expect_equal(dim(res_x), dim(x) + 2)
  expect_type(res_x, typeof(x))

  x <- matrix(c(1:4), nrow = 2, ncol = 2)
  res_x <- beadplexr:::pad_matrix(x)

  expect_equal(dim(res_x), dim(x) + 2)
  expect_type(res_x, typeof(x))

  res_x <- beadplexr:::pad_matrix(x, pad_size = 2L)
  expect_equal(dim(res_x), dim(x) + 4)
  expect_type(res_x, typeof(x))

  res_x <- beadplexr:::pad_matrix(x, pad = NA)
  expect_equal(dim(res_x), dim(x) + 2)
  expect_type(res_x, typeof(x))

  res_x <- beadplexr:::pad_matrix(x, pad = "xx")
  expect_equal(dim(res_x), dim(x) + 2)
  expect_type(res_x, "character")

  x <- matrix(c(1:6), nrow = 3, ncol = 2)
  res_x <- beadplexr:::pad_matrix(x)
  expect_equal(dim(res_x), dim(x) + 2)
  expect_type(res_x, typeof(x))
})

test_that("Neighbours are counted", {
  x <- matrix(
    c(1L, 1L, 0L,
      1L, 1L, 0L,
      0L, 1L, 1L),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  expectation <- matrix(
    c(3L, 3L, 0L,
      4L, 5L, 0L,
      0L, 3L, 2L),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  res_x <- beadplexr:::count_neighbours(x)
  expect_equal(res_x, expectation)
  expect_null(rownames(res_x))
  expect_null(colnames(res_x))

  mat_names <- list(c("r1", "r3", "r4"), c("c1", "c2", "c3"))
  x <- matrix(
    c(TRUE, TRUE, FALSE,
      TRUE, TRUE, FALSE,
     FALSE, TRUE, TRUE),
    nrow = 3,
    ncol = 3,
   byrow = TRUE,
   dimnames = mat_names
  )
  dimnames(expectation) <-  mat_names

  res_x <- beadplexr:::count_neighbours(x)
  expect_equal(res_x, expectation)
  expect_true(!is.null(rownames(res_x)))
  expect_true(!is.null(colnames(res_x)))

  x <- matrix(
    c("1", "1", "0",
      "1", "1", "0",
      "0", "1", "1"),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  expect_error(beadplexr:::count_neighbours(x))
})
