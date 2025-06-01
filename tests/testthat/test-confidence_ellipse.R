library(tibble)
# Test cases covered:
# confidence_ellipse function stops when no data is provided.
# confidence_ellipse function stops when input data is not a data frame or tibble.
# confidence_ellipse function stops when the conf_level is not numeric.
# confidence_ellipse function stops when the conf_level is not between 0 and 1.
# confidence_ellipse function returns a tibble.
# confidence_ellipse function works correctly with a simple example.
# confidence_ellipse function works correctly with a grouping factor.
# confidence_ellipse function stops when the covariance matrix contains NA's.
test_that("function stops with no data", {
  expect_error(confidence_ellipse(), "Missing 'data' argument.")
})
test_that("function stops with non df or tbl input", {
  expect_error(confidence_ellipse(.data = 123), "Input 'data' must be a data frame or tibble.")
})
test_that("function stops with non-numeric conf_level", {
  expect_error(
    confidence_ellipse(
      .data = glass,
      x = BaO,
      y = PbO,
      conf_level = "high"
    ),
    "'conf_level' must be numeric."
  )
})
test_that("function stops with invalid conf_level", {
  expect_error(
    confidence_ellipsoid(
      .data = glass,
      x = BaO,
      y = PbO,
      conf_level = 37
    ),
    "'conf_level' must be between 0 and 1."
  )
  expect_error(
    confidence_ellipsoid(
      .data = glass,
      x = BaO,
      y = PbO,
      conf_level = -.95
    ),
    "'conf_level' must be between 0 and 1."
  )
})
test_that("function returns a data frame or tibble", {
  result <- confidence_ellipse(glass, BaO, PbO)
  expect_true(is_tibble(result))
})
test_that("function works with simple example", {
  df <- data.frame(x = 1:10, y = 11:20)
  result <- confidence_ellipse(df, x, y)
  expect_true(is_tibble(result))
})
test_that("function works with grouping factor", {
  result <- confidence_ellipse(
    .data = glass,
    x = BaO,
    y = PbO,
    .group_by = glassType
  )
  expect_true(is_tibble(result))
})
test_that("function stops with NA's in covariance matrix", {
  df <- data.frame(x = 1:10, y = c(11, 2, 4, NA, 6, 3, 1, NA, NA, 10))
  expect_error(confidence_ellipse(df, x, y), "Covariance matrix contains NA values.")
})
