# Changelog

## BMSCS 26.09.0

### Updates

- Updated base image version and added additional package installations.
- Increased the required DataTools version to the most recent version.
- Expanded and organized .Rbuildignore, .gitignore, and .dockerignore
  entries to reduce accidental inclusion of local/CI/build artifacts.

## BMSCS 25.09.2

### Bug Fixes

- Added validation of `FORMULA` terms (#44):
  - if a term has an exponent and the feature is not numeric, the term
    is removed from the formula
  - if a term has a negative exponent and the feature contains zero or
    negative values, the term is removed from the formula

## BMSCS 25.09.1

### Bug Fixes

- Made `renderPrint()` more robust to gracefully handle `NaN` values,
  preventing crash scenarios. (#44)

## BMSCS 25.09.0

### New Features

- option to add custom points to all plots (in tabs *Model Evaluation*,
  *Model Parameters*, *Model Predictions*, *ROC Curve*, *Variable
  Correlations*)

### Updates

- Switched all plotting from base R to ggplot2 to enable custom point
  overlays (#35).
  - The following tabs now show ggplot2 plots: *Model Predictions*, *ROC
    Curve*, and *Variable Correlations*.
  - Plot styles may look slightly different due to the migration from
    base R graphics to ggplot2.

### Bug Fixes

- Fixed an error in [`split()`](https://rdrr.io/r/base/split.html) that
  could occur when input lengths were inconsistent (e.g. differing sizes
  of `parameters` and `splitChains`).

## BMSCS 25.07.1

### Bug Fixes

- Fixed an issue where the app could crash if some models did not return
  valid results (#44). The update now handles NaN values in model
  outputs gracefully. If modeling results are missing or invalid, they
  are excluded from the output summaries or replaced with a clear error
  message, preventing unexpected crashes.

## BMSCS 25.07.0

### Bug Fixes

- Fixed an issue during the creation of the coefficient table from model
  output (#44): Replaced fragile text-parsing logic with a new
  extract_coeff_from_summary() function that:
  - Directly uses posterior samples from rstan::sampling()
  - Rescales coefficients (incl. intercept) to the original data scale
  - Returns Estimate, Median, SD, and credible intervals for any cLevel

## BMSCS 25.06.1

### Bug Fixes

- Fixed an issue during the model import (#44):
  - Before, all inputs were updated simultaneously during model import.
    Inputs that depended on others (e.g., for dynamic choices) could not
    be updated correctly.
  - Now, a staged update mechanism is implemented: secondary inputs are
    updated only after all primary inputs have been set, ensuring that
    dependent inputs are populated reliably.

## BMSCS 25.06.0

### Bug Fixes

- fixed broken model export due to passing a deprecated argument that
  was removed in the meanwhile (#44)

## BMSCS 25.03.1

### Updates

- skip large data tests in CI and add large test data to the
  `.Rbuildignore`

## BMSCS 25.03.0

### Updates

- update links in ReadMe and in app header
- reduce package size by including example files into the
  `.Rbuildignore`

## BMSCS 24.11.0

### Updates

- apply modules from *shinyTools* package for plot and text export
- remove old code for export of plots and text

## BMSCS 24.10.0

### Bug Fixes

- fix issues with model download and upload (#38)
- exchange current example model file with a new but smaller model file
  to decrease package size

## BMSCS 24.06.1

### New Features

- option to save all model results in a single Excel in separate sheets
  (#10):
  - *Model Evaluation*: for the different models across different rows
    values for all “Information / Cross-Validation Error criterion”
  - *Model Summary*: across different rows for each model
  - *Model Diagnostics*: for different models across different rows and
    in separate columns
  - *Durbin-Watson Test*: for different models across different rows
  - *Variable importance*: across different rows the results for option
    global and for each model for option “model based”

### Updates

- UI in *Variable importance* for the global option “Variable importance
  type” (#10):
  - added the regression sign of the coefficients associated to each
    variable (as “1” or “-1”)

### Bug Fixes

- solves issue with older `DataTools` version when loading models

## BMSCS 24.06.0

### New Features

- Renaming of the Package
- R-CMD check workflow
- pkgdown Documentation
