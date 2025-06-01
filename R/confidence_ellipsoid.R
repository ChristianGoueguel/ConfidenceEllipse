#' @title Confidence Ellipsoid Coordinates
#' @author Christian L. Goueguel
#' @description Compute the coordinate points of confidence ellipsoids at a specified confidence level.
#' @param .data data frame or tibble.
#' @param x column name for the x-axis variable.
#' @param y column name for the y-axis variable.
#' @param z column name for the z-axis variable.
#' @param .group_by column name for the grouping variable (`NULL` by default). Note that this grouping variable must be a factor.
#' @param conf_level confidence level for the ellipsoid (0.95 by default).
#' @param robust optional (`FALSE` by default). When set to `TRUE`, it indicates
#' that robust estimation method is employed to calculate the coordinates of the ellipse.
#' The location is estimated using a 1-step M-estimator with the biweight psi function,
#' while the scale is estimated using the Minimum Covariance Determinant (MCD) estimator.
#' This approach is more resistant to outliers and provides
#' more reliable ellipse boundaries when the data contains extreme values or follows
#' a non-normal distribution.
#' @param distribution optional (`"normal"` by default). The distribution used to
#' calculate the quantile for the ellipse. It can be either `"normal"` or `"hotelling"`.
#' @details The function computes the coordinates of the confidence ellipse based
#' on the specified confidence level and the provided data. It can handle both classical
#' and robust estimation methods, and it supports grouping by a factor variable.
#' The `distribution` parameter controls the statistical approach used for ellipse
#' calculation. The `"normal"` option uses the chi-square distribution quantile,
#' which is appropriate when working with very large samples.
#' Whereas the `"hotelling"` option uses Hotelling's T² distribution quantile.
#' This approach accounts for uncertainty in estimating both mean and covariance
#' from sample data, producing larger ellipses that better reflect sampling uncertainty.
#' This is statistically more rigorous for smaller sample sizes where parameter
#' estimation uncertainty is higher.
#'
#' The combination of `distribution = "hotelling"` and `robust = TRUE` offers the
#' most conservative and statistically rigorous approach, particularly recommended
#' for exploratory data analysis and when dealing with datasets that may
#' not meet ideal statistical assumptions. For very large samples, the default
#' settings (`distribution = "normal"`, `robust = FALSE`) may be sufficient, as
#' the differences between methods diminish with increasing sample size.
#'
#' @references
#'
#' - Raymaekers, J., Rousseeuw P.J. (2019). Fast robust correlation for high dimensional data. Technometrics, 63(2), 184-198.
#' - Brereton, R. G. (2016). Hotelling’s T-squared distribution, its relationship to the F distribution and its use in multivariate space. Journal of Chemometrics, 30(1), 18–21.
#'
#' @return Data frame of the coordinates points.
#' @export confidence_ellipsoid
#' @examples
#' # Data
#' data("glass", package = "ConfidenceEllipse")
#' # Confidence ellipsoid
#' ellipsoid <- confidence_ellipsoid(.data = glass, x = SiO2, y = Na2O, z = Fe2O3)
#' ellipsoid_grp <- confidence_ellipsoid(
#'   .data = glass,
#'   x = SiO2,
#'   y = Na2O,
#'   z = Fe2O3,
#'   .group_by = glassType
#' )
#'
confidence_ellipsoid <- function(.data, x, y, z, .group_by = NULL, conf_level = 0.95, robust = FALSE, distribution = "normal") {
  if (missing(.data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(.data) && !tibble::is_tibble(.data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.numeric(conf_level)) {
    stop("'conf_level' must be numeric.")
  }
  if (conf_level < 0 || conf_level > 1) {
    stop("'conf_level' must be between 0 and 1.")
  }
  if (!distribution %in% c("normal", "hotelling")) {
    stop("'distribution' must be either 'normal' or 'hotelling'.")
  }

  if (rlang::quo_is_null(rlang::enquo(.group_by))) {
    selected_data <- .data %>%
      dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
      as.matrix()
    ellipsoid_coord <- transform_3d(selected_data, conf_level, robust, distribution)
    colnames(ellipsoid_coord) <- c("x", "y", "z")
    ellipsoid_coord %<>% tibble::as_tibble()
  } else {
    if (!is.factor(.data[[deparse(substitute(.group_by))]])) {
      stop("'.group_by' must be a factor.")
    } else {
      nested_tbl <- .data %>%
        dplyr::select({{ .group_by }}, {{ x }}, {{ y }}, {{ z }}) %>%
        dplyr::group_by({{ .group_by }}) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

      data <- NULL
      ellipsoid_tbl <- nested_tbl %>%
        dplyr::mutate(data = purrr::map(data, ~ transform_3d(as.matrix(.x), conf_level, robust, distribution))) %>%
        tidyr::unnest(data)

      ellipsoid_coord <- tibble::tibble(
        x = ellipsoid_tbl$data[, 1],
        y = ellipsoid_tbl$data[, 2],
        z = ellipsoid_tbl$data[, 3]
      ) %>%
        dplyr::bind_cols(ellipsoid_tbl[1])
    }
  }
  return(ellipsoid_coord)
}


transform_3d <- function(.x, conf_level, robust, distribution) {
  n <- nrow(.x)
  if (n < 3) {
    stop("At least 3 observations are required.")
  }

  if (robust == FALSE) {
    mean_vec <- colMeans(.x)
    cov_matrix <- stats::cov(.x)
  } else {
    locScale <- cellWise::estLocScale(.x, type = "wrap", center = TRUE, nLocScale = 50e3)
    X_wrap <- cellWise::wrap(.x, locScale[["loc"]], locScale[["scale"]], imputeNA = FALSE, checkPars = list(coreOnly = TRUE)) %>% purrr::pluck("Xw")
    mean_vec <- colMeans(X_wrap)
    cov_matrix <- stats::cov(X_wrap)
  }
  if (any(is.na(cov_matrix))) {
    stop("Covariance matrix contains NA values.")
  } else {
    eig <- eigen(cov_matrix)
    theta <- seq(0, 2 * pi, length.out = 50)
    phi <- seq(0, pi, length.out = 50)
    grid <- expand.grid(Theta = theta, Phi = phi)
    if (distribution == "normal") {
      quantile <- stats::qchisq(conf_level, 3)
    } else {
      quantile <- ((3 * (n - 1)) / (n - 3)) * stats::qf(conf_level, 3, n - 3)
    }
    X <- sqrt(eig$values[1] * quantile) * sin(grid$Phi) * cos(grid$Theta)
    Y <- sqrt(eig$values[2] * quantile) * sin(grid$Phi) * sin(grid$Theta)
    Z <- sqrt(eig$values[3] * quantile) * cos(grid$Phi)
    R <- cbind(X, Y, Z) %*% t(eig$vectors)
    result <- R + matrix(rep(mean_vec, nrow(R)), ncol = length(mean_vec), byrow = TRUE)
    return(result)
  }
}
