#' @title Confidence Ellipsoid Coordinates
#' @author Christian L. Goueguel
#' @description Compute the coordinate points of confidence ellipsoids at a specified confidence level.
#' @param .data data frame or tibble.
#' @param x column name for the x-axis variable.
#' @param y column name for the y-axis variable.
#' @param z column name for the z-axis variable.
#' @param .group_by column name for the grouping variable (`NULL` by default). Note that this grouping variable must be a factor.
#' @param conf_level confidence level for the ellipsoid (0.95 by default).
#' @param robust optional (`FALSE` by default). When set to `TRUE`, it indicates that robust estimation method is employed to calculate the coordinates of the ellipsoid. The location is the 1-step M-estimator with the biweight psi function. The scale is the Minimum Covariance Determinant (MCD) estimator. Raymaekers and Rousseeuw (2019).
#' @return Data frame of the coordinate points.
#' @export confidence_ellipsoid
#' @examples
#' # Data
#' data("glass", package = "ConfidenceEllipse")
#' # Confidence ellipsoid
#' ellipsoid <- confidence_ellipsoid(.data = glass, x = SiO2, y = Na2O, z = Fe2O3)
#' ellipsoid_grp <- confidence_ellipsoid(
#' .data = glass,
#' x = SiO2,
#' y = Na2O,
#' z = Fe2O3,
#' .group_by = glassType
#' )
#'
confidence_ellipsoid <- function(.data, x, y, z, .group_by = NULL, conf_level = 0.95, robust = FALSE) {
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
  transform_data <- function(.x, conf_level) {
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
    }
    else {
      eig <- eigen(cov_matrix)
      theta <- seq(0, 2 * pi, length.out = 50)
      phi <- seq(0, pi, length.out = 50)
      grid <- expand.grid(Theta = theta, Phi = phi)
      X <- sqrt(eig$values[1] * stats::qchisq(conf_level, 3)) * sin(grid$Phi) * cos(grid$Theta)
      Y <- sqrt(eig$values[2] * stats::qchisq(conf_level, 3)) * sin(grid$Phi) * sin(grid$Theta)
      Z <- sqrt(eig$values[3] * stats::qchisq(conf_level, 3)) * cos(grid$Phi)
      R <- cbind(X, Y, Z) %*% t(eig$vectors)
      result <- R + matrix(rep(mean_vec, nrow(R)), ncol = length(mean_vec), byrow = TRUE)
      return(result)
    }
  }
  if (rlang::quo_is_null(rlang::enquo(.group_by))) {
    selected_data <- .data %>% dplyr::select({{x}}, {{y}}, {{z}}) %>% as.matrix()
    ellipsoid_coord <- transform_data(selected_data, conf_level)
    colnames(ellipsoid_coord) <- c("x", "y", "z")
    ellipsoid_coord %<>% tibble::as_tibble()
  } else {
    if (!is.factor(.data[[deparse(substitute(.group_by))]])) {
      stop("'.group_by' must be a factor.")
    } else {
      nested_tbl <- .data %>%
        dplyr::select({{.group_by}}, {{x}}, {{y}}, {{z}}) %>%
        dplyr::group_by({{.group_by}}) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

      data <- NULL
      ellipsoid_tbl <- nested_tbl %>%
        dplyr::mutate(data = purrr::map(data, ~ transform_data(as.matrix(.x), conf_level))) %>%
        tidyr::unnest(data)

      ellipsoid_coord <- tibble::tibble(
        x = ellipsoid_tbl$data[, 1],
        y = ellipsoid_tbl$data[, 2],
        z = ellipsoid_tbl$data[, 3]) %>%
        dplyr::bind_cols(ellipsoid_tbl[1])
    }
  }
  return(ellipsoid_coord)
}
