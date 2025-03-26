# BMSCS 25.03.0

## Updates
- update links in ReadMe and in app header
- reduce package size by including test-model files into the `.Rbuildignore`

# BMSCS 24.11.0

## Updates
- apply modules from _shinyTools_ package for plot and text export
- remove old code for export of plots and text

# BMSCS 24.10.0

## Bug Fixes
- fix issues with model download and upload (#38)
- exchange current example model file with a new but smaller model file to decrease package size

# BMSCS 24.06.1

## New Features
- option to save all model results in a single Excel in separate sheets (#10):
  - *Model Evaluation*: for the different models across different rows values for all “Information / Cross-Validation Error criterion”
  - *Model Summary*: across different rows for each model
  - *Model Diagnostics*: for different models across different rows and in separate columns
  - *Durbin-Watson Test*: for different models across different rows
  - *Variable importance*: across different rows the results for option global and for each model for option “model based”

## Updates
- UI in *Variable importance* for the global option “Variable importance type” (#10):
  - added the regression sign of the coefficients associated to each variable (as “1” or “-1”)
  
## Bug Fixes
- solves issue with older `DataTools` version when loading models

# BMSCS 24.06.0

## New Features
- Renaming of the Package
- R-CMD check workflow
- pkgdown Documentation

# BMSCApp 24.04.0

## Bug Fixes
- _Import of models_: added average model to the down- and upload of models

# BMSCApp 23.12.1

## Bug Fixes
- _Import of models from Pandora_: 
  - an error message occurred when trying to load a model from pandora.
  - fix: adding the missing download of the zip file from the url before unpacking the zip

# BMSCApp 23.12.0

## New Features
- _Import of models_: display of "About" information that is associated to a selected Pandora 
  Repository

# BMSCApp 23.09.0

## New Features
- _Import of models_:
  - option to import models from Pandora platform

# BMSCApp 23.07.0

## New Features
- New model evaluation option: Bayesian R-squared (following: https://avehtari.github.io/bayes_R2/bayes_R2.html)

# BMSCApp 23.04.1

## New Features
- _Upload and download of models_: 
  - new version of model up- and download (contains also the option to load model output)
  - a button opens a pop-up that contains the UI
  - the compression of model outputs may take a while

# BMSCApp 23.03.2

## New Features
- option to load remote models from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version
- option to save user inputs and data without the model output
- option to upload user inputs and data. Upload of model output is not supported since the model
output of BMSC is too large for the upload

# BMSCApp 23.03.1

## Bug fixes
- add remote package to enable gpt3 in the _Import Data_ module

# BMSCApp 23.02.2

## Bug Fixes
- add tryCatch to modeling with forwarding of warnings and error messages to the UI (#18)

# BMSCApp 23.02.1

## New Features
- in the tab _Model Input_
  - new slider _Max inverse exponent_: Now one can add x^-1, x^-2,.. as potential modelling 
  features
  - new checkbox _Impute missing values_: Imputation of missing values added (multiple
  imputation via the mice package)
  - new UI _Create model average_: Model averaging added. Now models can be averaged by a 
  criterion (AIC, AICc, WAIC, logLik, BIC and Loo)

# BMSCApp 23.01.1

## New Features
- the _Import Data_ module is now imported from the new package DataTools (#16, PR #17)
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_
- now, sidebars are fixed with auto scroll in all tabs (iso-app #4)
