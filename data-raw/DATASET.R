## code to prepare `glass` dataset goes here
glass <-
  readr::read_csv("~/Documents/Projects/Packages/data/glass.csv", show_col_types = FALSE) %>%
  purrr::modify_at("glassType", as.factor)

usethis::use_data(glass, overwrite = TRUE)
