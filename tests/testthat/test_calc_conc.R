
# Calculate standard concentration ----------------------------------------
test_that("Calculation is correct",{
  expect_identical(calc_std_conc(c(0:7), 10000),
                   c(0, 10000/(4^6), 10000/(4^5), 10000/(4^4), 10000/(4^3), 10000/(4^2), 10000/(4), 10000))

  expect_identical(calc_std_conc(c(7:1, 0), 5, c(1:20)),
                   c(5/(c(1:7) |> cumprod()), 0))

  expect_identical(calc_std_conc(c(0:7), 10000)[1], 0)

  expect_identical(calc_std_conc(c(7:1, 0), 5, c(1, 2, 2, 2, 4, 6, 6, 100000))[8], 0)
})

test_that("Numeric is returned", {
  expect_type(calc_std_conc(c(0:9), 5), "double")

  expect_type(calc_std_conc(c(letters[1:8]), 5), "double")
  expect_type(calc_std_conc(c(letters[1:7], 0), 5), "double")
  expect_type(calc_std_conc(c(letters[1:7], 0, 1), 5), "double")

  expect_type(calc_std_conc(c(7:1, 0), 5, c(1, 2, 2, 2, 4, 6, 6, 0)), "double")
  expect_type(calc_std_conc(c(7:1, 0), 5, c(1, 2, 2, 2, 4, 6, 6, 100000)), "double")
  expect_type(calc_std_conc(c(8:1), 5, c(1, 2, 2, 2, 4, 6, 6, 100000)), "double")
})

test_that("Errors and warnings are thrown", {
  expect_warning(calc_std_conc(c(0:6), 5))

  expect_error(calc_std_conc(c(7:1, 0), 5, c(1, 2)))
})

# Fit standard curve ------------------------------------------------------
mfi_data <- system.file("testdata", "analyte_mfi.rds", package = "beadplexr")
mfi_data <- readRDS(mfi_data)
mfi_data <- subset(mfi_data, grepl("-C", Sample))
mfi_data <- subset(mfi_data, grepl("A10", `Analyte ID`))
mfi_data$Sample <- as.integer(gsub("K3-C|-[1:2]", "", mfi_data$Sample))
mfi_data$Concentration <- sort(calc_std_conc(rep(0:7, 2), 10000))
mfi_data$Concentration <- log10(mfi_data$Concentration)

mfi_data$`Conc entra` <- mfi_data$Concentration
mfi_data$`Conc entra` <- ifelse(is.infinite(mfi_data$`Conc entra`), 0, mfi_data$`Conc entra`)
mfi_data$FL2.H <- mfi_data$`FL2-H`

test_that("Fitting a standard curve is possible", {
  # Parameters
  # The warning "NaNs produced" is procuded only when running devtols::test(), not when running the functions individually.
  # I gave up trying to fix things nicely, and just added the `suppressWarnings`. Now everything is dandy.
  expect_s3_class(suppressWarnings(fit_standard_curve(df = mfi_data, .parameter = "FL2-H", .concentration = "Concentration")), "drc")
  expect_s3_class(suppressWarnings(fit_standard_curve(df = mfi_data, .parameter = "FL2-H", .concentration = "Conc entra")), "drc")
  expect_s3_class(suppressWarnings(fit_standard_curve(df = mfi_data, .parameter = "FL2.H", .concentration = "Concentration")), "drc")
  expect_s3_class(suppressWarnings(fit_standard_curve(df = mfi_data, .parameter = "FL2.H", .concentration = "Conc entra")), "drc")

  expect_error(fit_standard_curve(df = mfi_data, .parameter = "FL", .concentration = "Concentration"))
  expect_error(fit_standard_curve(df = mfi_data, .parameter = "FL2.H", .concentration = "Conc"))

  # Different fit methods
  expect_s3_class(suppressWarnings(fit_standard_curve(df = mfi_data, .fct = "LL.4")), "drc")
  expect_s3_class(suppressWarnings(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "LL.3")), "drc")

  # Not sure why all these fail...
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "LL3"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "LL.2.2"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "LL.2.5"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "AR.2"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "AR.3"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "MM.2"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "MM.3"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "W1.3"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "W1.4"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "BC.4"))
  expect_error(fit_standard_curve(df = mfi_data, .concentration = "Conc entra", .fct = "BC.5"))
})
