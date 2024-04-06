#' @title A Function that Computes the 3D Confidence Ellipsoid Coordinates for Trivariate Normal Data (with Optional Grouping)
#' @author Christian L. Goueguel
#' @param .data The data frame or tibble.
#' @param x The unquoted column name for the x-axis variable.
#' @param y The unquoted column name for the y-axis variable.
#' @param z The unquoted column name for the z-axis variable.
#' @param .group_by The unquoted column name for the grouping variable (optional). Note that this grouping variable must be a factor.
#' @param conf_level The confidence level for the ellipse (0.95 by default).
#' @return A data frame of the coordinates points of the ellipse.
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
confidence_ellipsoid <- function(.data, x, y, z, .group_by = NULL, conf_level = 0.95) {
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
    mean_vec <- colMeans(.x)
    cov_matrix <- stats::cov(.x)
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
