# Preparation -------------------------------------------------------------
data("lplex")
df <- lplex[[1]]
.nrow_data <- nrow(df)

# Perform test ------------------------------------------------------------
test_that("Despeckle works", {
  expect_lt(nrow(despeckle(df, .parameters = c("FL6-H", "FL2-H"))), .nrow_data)
  expect_lt(nrow(despeckle(df, .parameters = c("FL6-H", "FL2-H"), .bins = 128L)), .nrow_data)
  expect_lt(nrow(despeckle(df, .parameters = c("FL6-H", "FL2-H"), .neighbours = 8L)), .nrow_data)
  # If we have no points to remove, we should stay equal (the filter is neighbours < criteria)
  ddf <- despeckle(df, .parameters = c("FL6-H", "FL2-H"), .neighbours = 1L)

  expect_equal(nrow(dplyr::setdiff(ddf, df)), 0L)
})

test_that("Errors are thrown",{
  expect_error(despeckle(df, .parameters = c("FL6-H")))
  expect_error(despeckle(df, .parameters = c("xxxFL6-H", "FL2-H")))
  expect_warning(despeckle(df, .parameters = c("FL6-H", "FL2-H"), xxx = "zzzzz"))
})
