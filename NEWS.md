# ConfidenceEllipse 1.1.0
* Added `distribution` parameter to the `confidence_ellipse()` and `confidence_ellipsoid` 
functions allowing users to choose between `"normal"` and `"hotelling"` (Hotelling's T²) distributions 
for ellipse calculation.
* Enhanced statistical rigor: Hotelling's T² distribution now properly accounts 
for parameter estimation uncertainty in small sample sizes.
* Updated documentation with detailed explanations of distribution choices and robust methods.
* Added recommendations for parameter selection based on sample size and data characteristics.
* Change name of `transform_data` helper function to `transform_2d` and `transform_3d` for clarity and consistency.

# ConfidenceEllipse 1.0.0
* Added `robust` parameter to the `confidence_ellipse()` and `confidence_ellipsoid` functions enabling robust 
estimation methods using 1-step M-estimator with biweight psi function for location 
and Minimum Covariance Determinant (MCD) estimator for scale.
* Improved handling of outliers and non-normal data through robust estimation options.
* Enhanced documentation with examples demonstrating the use of robust methods.
* CRAN release with comprehensive vignettes and examples.

# ConfidenceEllipse 0.2.1
* Improved code of `transform_data` helper function.

# ConfidenceEllipse 0.2.0
* Added `transform_data` helper function to streamline data transformation processes.

# ConfidenceEllipse 0.1.4
* Modified package logo to include a more visually appealing design.
* Updated package description to better reflect the functionality and features of the package.

# ConfidenceEllipse 0.1.3
* Cleaned up code and improved performance of `confidence_ellipse()` and `confidence_ellipsoid()` functions.

# ConfidenceEllipse 0.1.2
* Introduced `confidence_ellipse()` function for 2D confidence ellipse calculations.
* Introduced `confidence_ellipsoid()` function for 3D confidence ellipsoid calculations.

# ConfidenceEllipse 0.1.1 
* Fixed bug in `confidence_ellipse()` function where the `conf_level` parameter was not being correctly applied.

# ConfidenceEllipse 0.1.0
* Initial release of the ConfidenceEllipse package.
