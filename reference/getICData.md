# Get Data of IC

Get Data of IC

## Usage

``` r
getICData(allFits, modelNames, ic, withColumnICName = FALSE)
```

## Arguments

- allFits:

  (list) list of model\$fits objects

- modelNames:

  (character) name of all models

- ic:

  (character) name of information criterion, e.g.
  `"AUC", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "logLik", "nagelkerke", "Loo", "WAIC"`

- withColumnICName:

  (logical) if TRUE, add a separate first column "IC_name" containing
  the name of ic. Useful, for `bind_rows` over several ic values
