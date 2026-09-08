# Bind All results

Bind All results

## Usage

``` r
bindAllResults(listOfDataframes, addEmptyRow = TRUE)
```

## Arguments

- listOfDataframes:

  (list) list of data.frames that should be bind with
  [`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)

- addEmptyRow:

  (logical) if TRUE, add an empty row with NA values after each element
  of `listOfDataframes`
