test_that("Deprecation waring is raised", {
  expect_warning(raise_deprecated("XX", "YY"), regexp = "'XX' is deprecated, use 'YY' instead")
  expect_warning(
    raise_deprecated(
      old = "XX",
      new = "YY",
      caller = "ZZZ"
    ),
    regexp = "'XX' in ZZZ() is deprecated, use 'YY' instead",
    fixed = TRUE
  )
  expect_warning(
    raise_deprecated(
      old = "XX",
      new = "YY",
      caller = "ZZZ()"
    ),
    regexp = "'XX' in ZZZ() is deprecated, use 'YY' instead",
    fixed = TRUE
  )
})
