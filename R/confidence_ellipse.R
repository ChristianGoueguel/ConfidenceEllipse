#' @title A Function that Computes the Confidence Ellipse Coordinates for Bivariate Normal Data (with Optional Grouping)
#' @author Christian L. Goueguel
#' @description This function computes a confidence ellipse (assuming bivariate normality) at a specified confidence level.
#' @param .data The data frame or tibble.
#' @param x The unquoted column name for the x-axis variable.
#' @param y The unquoted column name for the y-axis variable.
#' @param .group_by The unquoted column name for the grouping variable (optional). Note that this grouping variable must be a factor.
#' @param conf_level The confidence level for the ellipse (0.95 by default).
#' @return A data frame of the coordinates points of the ellipse.
#' @export confidence_ellipse
#' @examples
#' # Data
#' data("glass", package = "ConfidenceEllipse")
#' # Confidence ellipse
#'  ellipse <- confidence_ellipse(.data = glass, x = SiO2, y = Na2O)
#'  ellipse_grp <- confidence_ellipse(.data = glass, x = SiO2, y = Na2O, .group_by = glassType)
#'
confidence_ellipse <- function(.data, x, y, .group_by = NULL, conf_level = 0.95) {
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
      theta <- (2 * pi * seq(0, 360, 1)) / 360
      X <- sqrt(eig$values[1] * stats::qchisq(conf_level, 2)) * cos(theta)
      Y <- sqrt(eig$values[2] * stats::qchisq(conf_level, 2)) * sin(theta)
      R <- cbind(X, Y) %*% t(eig$vectors)
      result <- R + matrix(rep(t(mean_vec), 361), ncol = ncol(t(mean_vec)), byrow = TRUE)
      return(result)
    }
  }
  if (rlang::quo_is_null(rlang::enquo(.group_by))) {
    selected_data <- .data %>% dplyr::select({{x}}, {{y}}) %>% as.matrix()
    ellipse_coord <- transform_data(selected_data, conf_level)
    colnames(ellipse_coord) <- c("x", "y")
    ellipse_coord %<>% tibble::as_tibble()
    }
  else {
    if (!is.factor(.data[[deparse(substitute(.group_by))]])) {
      stop("'.group_by' must be a factor.")
    } else {
      nested_tbl <- .data %>%
        dplyr::select({{.group_by}}, {{x}}, {{y}}) %>%
        dplyr::group_by({{.group_by}}) %>%
        tidyr::nest() %>%
        dplyr::ungroup()

      data <- NULL
      ellipse_tbl<- nested_tbl %>%
        dplyr::mutate(data = purrr::map(data, ~ transform_data(as.matrix(.x), conf_level))) %>%
        tidyr::unnest(data)

      ellipse_coord <- tibble::tibble(
        x = ellipse_tbl$data[, 1],
        y = ellipse_tbl$data[, 2]) %>%
        dplyr::bind_cols(ellipse_tbl[1])
    }
  }
  return(ellipse_coord)
}
