# BMSCApp app 

## Version 23.03.2

### New Features
- option to load remote models from the github folder `inst/app/predefinedModels` of the respective 
repository
- if there is no internet connection remote models are taken from the models that were saved with
  the last deployed app version
- option to save user inputs and data without the model output
- option to upload user inputs and data. Upload of model output is not supported since the model
output of BMSC is too large for the upload

## Version 23.03.1

### Bug fixes
- add remote package to enable gpt3 in the _Import Data_ module

## Version 23.02.2

### Bug Fixes
- add tryCatch to modeling with forwarding of warnings and error messages to the UI (#18)

## Version 23.02.1

### New Features
- in the tab _Model Input_
  - new slider _Max inverse exponent_: Now one can add x^-1, x^-2,.. as potential modelling 
  features
  - new checkbox _Impute missing values_: Imputation of missing values added (multiple
  imputation via the mice package)
  - new UI _Create model average_: Model averaging added. Now models can be averaged by a 
  criterion (AIC, AICc, WAIC, logLik, BIC and Loo)

## Version 23.01.1

### New Features
- the _Import Data_ module is now imported from the new package DataTools (#16, PR #17)
  - additionally to file import, now import from _URL_ or from _Pandora Platform_ is possible
  - all redundant code was removed
  - using "file" as default source in _Import Data_
- now, sidebars are fixed with auto scroll in all tabs (iso-app #4)
