
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ConfidenceEllipse

<img src="man/figures/logo.png" align="right" height="239" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/ChristianGoueguel/ConfidenceEllipse/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChristianGoueguel/ConfidenceEllipse/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The objective of the `ConfidenceEllipse` package is to compute the
coordinate points of confidence ellipses and ellipsoids for a given
bivariate and trivariate normal data. The size of the ellipse and
ellipsoid is determined by the confidence level, and the shape is
determined by the correlation structure between the variables. The
confidence level is usually chosen to be 95% or 99%, and the resulting
confidence region contains the points that are expected to lie within
the multivariate distribution.

## Installation

You can install the development version of `ConfidenceEllipse` like so:

``` r
# install.packages("remotes")
# remotes::install_github("ChristianGoueguel/ConfidenceEllipse")
```

## Example

``` r
library(tidyverse)
library(magrittr)
```

### Dataset

The dataset is comprised of 13 different measurements for 180
archaeological glass vessels from different groups (Janssen, K.H.A., De
Raedt, I., Schalm, O., Veeckman, J.: Microchim. Acta 15 (suppl.) (1998)
253-267. Compositions of 15th - 17th century archaeological glass
vessels excavated in Antwerp.)

``` r
data(glass, package = "chemometrics")
data(glass.grp, package = "chemometrics")
```

``` r
df <-
  cbind(glass.grp, glass) %>%
  as_tibble() %>%
  rename(glassType = glass.grp) %>%
  modify_at("glassType", as_factor)
```

``` r
df %>% glimpse()
#> Rows: 180
#> Columns: 14
#> $ glassType <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, …
#> $ Na2O      <dbl> 13.904, 14.194, 14.668, 14.800, 14.078, 13.600, 12.942, 15.6…
#> $ MgO       <dbl> 2.244, 2.184, 3.034, 2.455, 2.480, 1.648, 2.690, 2.028, 2.25…
#> $ Al2O3     <dbl> 1.312, 1.310, 1.362, 1.385, 1.072, 2.012, 1.420, 1.242, 1.07…
#> $ SiO2      <dbl> 67.752, 67.076, 63.254, 63.790, 68.768, 69.628, 64.012, 70.6…
#> $ P2O5      <dbl> 0.884, 0.938, 0.988, 1.200, 0.682, 0.698, 0.966, 0.210, 0.75…
#> $ SO3       <dbl> 0.052, 0.024, 0.064, 0.115, 0.070, 0.038, 0.046, 0.310, 0.03…
#> $ Cl        <dbl> 0.936, 0.966, 0.886, 0.988, 0.966, 0.908, 0.896, 0.676, 0.93…
#> $ K2O       <dbl> 3.044, 3.396, 2.828, 2.878, 2.402, 3.196, 2.526, 2.326, 2.32…
#> $ CaO       <dbl> 8.784, 8.636, 11.088, 10.833, 8.808, 6.160, 12.982, 6.324, 9…
#> $ MnO       <dbl> 0.674, 0.698, 1.240, 0.978, 0.310, 1.170, 0.874, 0.214, 0.60…
#> $ Fe2O3     <dbl> 0.364, 0.336, 0.400, 0.433, 0.242, 0.650, 0.516, 0.278, 0.25…
#> $ BaO       <dbl> 0.040, 0.040, 0.046, 0.025, 0.022, 0.156, 0.014, 0.032, 0.02…
#> $ PbO       <dbl> 0.004, 0.198, 0.134, 0.120, 0.102, 0.136, 0.120, 0.062, 0.02…
```

### Confidence Ellipse

First, the `confidence_ellipse` function is used to compute coordinate
points of the confidence ellipse and then the ellipse is plotted on a
two-dimensional plot `x` and `y` of the data. Points that lie outside
the ellipse are considered to be outliers, while points that lie within
the ellipse are considered to be part of the underlying distribution
with the specified confidence level `conf_level`.

``` r
ellipse_99 <- confidence_ellipse(df, x = MgO, y = Cl, conf_level = 0.99)
ellipse_95 <- confidence_ellipse(df, x = MgO, y = Cl, conf_level = 0.95)
ellipse_90 <- confidence_ellipse(df, x = MgO, y = Cl, conf_level = 0.90)
```

``` r
ellipse_99 %>% glimpse()
#> Rows: 361
#> Columns: 2
#> $ x <dbl> -0.138094945, -0.138406455, -0.137962123, -0.136762084, -0.134806705…
#> $ y <dbl> 0.7702743, 0.7592797, 0.7482409, 0.7371614, 0.7260445, 0.7148935, 0.…
```

``` r
ggplot() +
  geom_point(data = ellipse_99, aes(x = x, y = y), color = "red", size = .1) +
  geom_point(data = ellipse_95, aes(x = x, y = y), color = "blue", size = .1) +
  geom_point(data = ellipse_90, aes(x = x, y = y), color = "green", size = .1) +
  geom_point(dat = df, aes(x = MgO, y = Cl), color = "black", size = 3L) +
  xlim(-.5, 6) +
  ylim(-0.05, 1.5) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  labs(x = "MgO (wt.%)", y = "Cl (wt.%)") +
  theme_bw() +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

#### Grouping

For grouping bivariate data, the `.group_by` argument can be used if the
data contains an unique grouping variable (`.group_by = NULL` by
default). When a grouping variable is provided, the function will
compute the ellipses separately for each level of the factor, allowing
you to explore potential differences or patterns within subgroups of the
data.

It’s important to note that the grouping variable should be
appropriately coded as a factor before passing it to the `.group_by`
argument. If the variable is currently stored as a character or numeric
type, you may need to convert it to a factor using functions like
`as.factor()` or `forcats::as_factor()`.

``` r
ellipse_grp <- df %>% confidence_ellipse(MgO, Cl, glassType)
```

``` r
ellipse_grp %>% glimpse()
#> Rows: 1,444
#> Columns: 3
#> $ x         <dbl> 0.5675946, 0.5678922, 0.5687056, 0.5700346, 0.5718788, 0.574…
#> $ y         <dbl> 0.6758368, 0.6688488, 0.6618660, 0.6548902, 0.6479238, 0.640…
#> $ glassType <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
```

``` r
ggplot() +
  geom_point(dat = df, aes(x = MgO, y = Cl, colour = glassType, shape = glassType), size = 3L) +
  geom_line(data = ellipse_grp, aes(x = x, y = y, colour = glassType), linewidth = .1) +
  xlim(0, 6) +
  ylim(-0.05, 1.5) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  labs(x = "MgO (wt.%)", y = "Cl (wt.%)") +
  theme_bw() +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

#### Principal Component Analysis

``` r
rpca_scores <- df %>%
  select(where(is.numeric) )%>% 
  pcaPP::PCAproj(method = "qn") %>%
  pluck("scores") %>%
  as_tibble() %>%
  mutate(glassType = df %>% pull(glassType)) %>%
  rename(PC1 = Comp.1, PC2 = Comp.2) 
```

``` r
ellipse_pca <- rpca_scores %>%
confidence_ellipse(PC1, PC2, glassType)
```

``` r
ggplot() +
  geom_point(data = rpca_scores, aes(x = PC1, y = PC2, colour = glassType, shape = glassType), size = 3L) +
  geom_line(data = ellipse_pca, aes(x = x, y = y, colour = glassType), linewidth = .1) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  labs(x = "PC1", y = "PC2") +
  theme_bw() +
  theme(legend.position = "none")
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

### Confidence Ellipsoid

The `confidence_ellipsoid` function accepts an additional variable `z`
and computes the ellipsoid for trivariate data.

``` r
ellipsoid_grp <- df %>% 
  confidence_ellipsoid(SiO2, Na2O, Fe2O3, glassType)
```

``` r
ellipsoid_grp %>% glimpse()
#> Rows: 10,000
#> Columns: 4
#> $ x         <dbl> 67.32486, 67.32486, 67.32486, 67.32486, 67.32486, 67.32486, …
#> $ y         <dbl> 14.51964, 14.51964, 14.51964, 14.51964, 14.51964, 14.51964, …
#> $ z         <dbl> 0.5971494, 0.5971494, 0.5971494, 0.5971494, 0.5971494, 0.597…
#> $ glassType <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
```

``` r
rgl::setupKnitr(autoprint = TRUE)
rgl::plot3d(
  x = ellipsoid_grp$x, 
  y = ellipsoid_grp$y, 
  z = ellipsoid_grp$z,
  xlab = "SiO2 (wt.%)", 
  ylab = "Na2O (wt.%)", 
  zlab = "Fe2O3 (wt.%)",
  type = "s", 
  radius = 0.03,
  col = as.numeric(ellipsoid_grp$glassType)
  )
rgl::points3d(
  x = df$SiO2, 
  y = df$Na2O, 
  z = df$Fe2O3, 
  col = as.numeric(df$glassType),
  size = 5
  )
rgl::view3d(theta = 260, phi = 30, fov = 60, zoom = .85)
```

<img src="man/figures/README-unnamed-chunk-19-1-rgl.png" width="100%" />
