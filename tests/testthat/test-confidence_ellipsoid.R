library(tibble)
# Test cases covered:
# confidence_ellipsoid function stops when no data is provided.
# confidence_ellipsoid function stops when input data is not a data frame or tibble.
# confidence_ellipsoid function stops when the conf_level is not numeric.
# confidence_ellipsoid function stops when the conf_level is not between 0 and 1.
# confidence_ellipsoid function returns a tibble.
# confidence_ellipsoid function works correctly with a simple example.
# confidence_ellipsoid function works correctly with a grouping factor.
# confidence_ellipsoid function stops when the covariance matrix contains NA's.
test_that("function stops with no data", {
  expect_error(confidence_ellipsoid(), "Missing 'data' argument.")
})
test_that("function stops with non df or tbl input", {
  expect_error(confidence_ellipsoid(.data = 123), "Input 'data' must be a data frame or tibble.")
})
test_that("function stops with non-numeric conf_level", {
  expect_error(
    confidence_ellipsoid(
      .data = glass,
      x = BaO,
      y = PbO,
      z = Na2O,
      conf_level = "high"),
    "'conf_level' must be numeric.")
})
test_that("function stops with invalid conf_level", {
  expect_error(
    confidence_ellipsoid(
      .data = glass,
      x = BaO,
      y = PbO,
      z = Na2O,
      conf_level = 37),
    "'conf_level' must be between 0 and 1.")
})
test_that("function returns a data frame or tibble", {
  result <- confidence_ellipsoid(glass, BaO, PbO, Na2O)
  expect_true(is_tibble(result))
})
test_that("function works with simple example", {
  df <- data.frame(x = 1:10, y = 11:20, z = 11:20)
  result <- confidence_ellipsoid(df, x, y, z)
  expect_true(is_tibble(result))
})
test_that("function works with grouping factor", {
  result <- confidence_ellipsoid(
    .data = glass,
    x = BaO,
    y = PbO,
    z = Na2O,
    .group_by = glassType)
  expect_true(is_tibble(result))
})
test_that("function stops with NA's in covariance matrix", {
  df <- data.frame(
    x = 1:10,
    y = c(11, 2, 4, NA, 6, 3, 1, NA, NA, 10),
    z = 11:20)
  expect_error(confidence_ellipsoid(df, x, y, z), "Covariance matrix contains NA values.")
})


