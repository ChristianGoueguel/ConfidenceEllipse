#' @title A Function that Computes the Confidence Ellipse Coordinates for Bivariate Normal Data (with Optional Grouping)
#' @author Christian L. Goueguel
#' @description This function computes a confidence ellipse (assuming bivariate normality) at a specified confidence level.
#' @param data the data frame or tibble.
#' @param x the x-axis column name.
#' @param y the y-axis column name.
#' @param conf_level the confidence level for the ellipse (0.95 by default).
#' @param by_group for grouping the bivariate data, if it contains a grouping third column (FALSE by default). Note that this third column must be a factor.
#' @return a data frame of the coordinates points of the ellipse.
#' @export confidence_ellipse
confidence_ellipse <- function(data, x = NULL, y = NULL, conf_level = 0.95, by_group = FALSE) {

  if (missing(data)) {
    stop("Missing 'data' argument.")
  }
  if (!is.data.frame(data) && !tibble::is_tibble(data)) {
    stop("Input 'data' must be a data frame or tibble.")
  }
  if (!is.numeric(conf_level)) {
    stop("'conf_level' must be numeric.")
  }
  if (conf_level < 0 && conf_level > 1) {
    stop("'conf_level' must be between 0 and 1.")
  }
  if(!is.logical(by_group)) {
    stop("'by_group' must be of boolean type (TRUE or FALSE).")
  }
  if (sum(purrr::map_lgl(data, is.factor)) != 1) {
    stop("Input 'data' must have one factor column as grouping variable.")
  }

  transform_data <- function(.x, conf_level) {
    mean_vec <- colMeans(.x)
    cov_mat <- stats::cov(.x)
    eig <- eigen(cov_mat)
    theta <- (2*pi*seq(0, 360, 1))/360
    B1 <- sqrt(eig$values[1]*stats::qchisq(conf_level, 2)) * cos(theta)
    B2 <- sqrt(eig$values[2]*stats::qchisq(conf_level, 2)) * sin(theta)
    R <- cbind(B1, B2) %*% t(eig$vectors)
    C <- R + matrix(rep(t(mean_vec), 361), ncol = ncol(t(mean_vec)), byrow = TRUE)
    return(C)
  }
  if (by_group == FALSE) {
    X_mat <- data %>%
      dplyr::select({{x}}, {{y}}) %>%
      as.matrix()
    res <- transform_data(X_mat, conf_level)
    res %<>%
      tibble::as_tibble() %>%
      dplyr::rename(x = rlang::.data$V1, y = rlang::.data$V2)
  } else {
    X_tbl <- data
    factor_col <- X_tbl %>%
      dplyr::select(tidyselect::where(is.factor)) %>%
      names() %>%
      dplyr::sym()
    nested_tbl <- X_tbl %>%
      dplyr::group_by(!!dplyr::sym(factor_col)) %>%
      dplyr::select({{x}}, {{y}}) %>%
      tidyr::nest() %>%
      dplyr::ungroup()
    res <- matrix(0, nrow = 361*length(nested_tbl$data), ncol = 3)
    for (i in seq_along(nested_tbl$data)) {
      grouped_tbl <- nested_tbl %>%
        purrr::pluck(2, i) %>%
        dplyr::select(tidyselect::where(is.numeric)) %>%
        as.matrix()
      Y_grp <- transform_data(grouped_tbl, conf_level)
      Y_grp <- cbind(Y_grp, replicate(361, nested_tbl$group[i]))
      res[seq(1+(361*(i-1)), 361*i), ] <- Y_grp
    }
    res %<>%
      tibble::as_tibble() %>%
      dplyr::rename(x = rlang::.data$V1, y = rlang::.data$V2, group = rlang::.data$V3) %>%
      purrr::modify_at("group", forcats::as_factor)
  }
  return(res)
}


