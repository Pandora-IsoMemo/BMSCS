#' Bind All results
#' 
#' @param listOfDataframes (list) list of data.frames that should be bind with 
#'  \code{dplyr::bind_rows}
#' @param addEmptyRow (logical) if TRUE, add an empty row with NA values after each element of
#'  \code{listOfDataframes}
bindAllResults <- function(listOfDataframes, addEmptyRow = TRUE) {
  if (addEmptyRow) {
    listOfDataframes <- lapply(listOfDataframes, function(x) x %>% add_na_row())
  }
  
  res <- listOfDataframes %>% 
    dplyr::bind_rows()
  
  rownames(res) <- NULL
  
  res
}

#' Add NA Row
#' 
#' Function to add a row with NA values at the end of a data.frame
#' 
#' @param df (data.frame) data.frame
add_na_row <- function(df) {
  na_row <-  matrix(rep(NA, ncol(df)), 
                    nrow = 1, 
                    dimnames = list("", colnames(df))) %>% 
    as.data.frame()
  bind_rows(df, na_row)
}
