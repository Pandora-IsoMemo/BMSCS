#' prepareData
#' 
#' @param rawData (reactive) data()
#' @param in_x (reactive) input$x
#' @param in_xUnc (reactive) input$xUnc
#' @param in_y (reactive) input$y
#' @param in_yUnc (reactive) input$yUnc
#' @param in_xCategorical (reactive) input$xCategorical
#' @param in_xCatUnc (reactive) input$xCatUnc
#' @param in_regType (reactive) input$regType
prepareData <- function(rawData, in_x, in_xUnc, in_y, in_yUnc, in_xCategorical, in_xCatUnc, in_regType) {
  if(is.null(in_y) || in_y == ""){
    stop("Please select an dependent variable")
  }
  
  if(is.null(in_x) || all(in_x == "")){
    stop("Please select an independent variable(s)")
  }
  
  xVars <- in_x
  if(!is.null(in_xCategorical) && any(in_xCategorical != "")){
    xVars <- c(xVars, in_xCategorical)
    xCategorical <- in_xCategorical
  } else {
    xCategorical <- ""
  }
  
  dataModel <- rawData
  
  if(all(is.na(dataModel[, in_y]))){
    stop("Dependent variable has no numeric values")
  }
  
  if(any(apply(dataModel[, in_x, drop = FALSE], 2, function(k) all(is.na(as.numeric(k)))))){
    stop("At least one x variable has no numeric values")
  }
  
  dataModel[, in_x] <- as.data.frame(sapply(dataModel[, in_x], as.numeric)) 
  
  if(!is.null(in_xUnc)){
    xUnc <- dataModel[, in_xUnc, drop = FALSE]
    names(xUnc) <- in_x
    if(any(apply(dataModel[, in_xUnc, drop = FALSE], 2, function(k) all(as.numeric(is.na(k)))))){
      stop("At least one x uncertainty variable has no numeric values")
    }
    if(length(in_xUnc) != length(in_x)){
      stop("x-uncertainty variables must have same length as x-variables")
    }
  } else {
    xUnc <- NULL
  }
  
  if(!is.null(in_xCatUnc)){
    xCatUnc <- dataModel[, in_xCatUnc, drop = FALSE]
    names(xCatUnc) <- in_xCategorical
    if(any(apply(dataModel[, in_xCatUnc, drop = FALSE], 2, function(k) all(as.numeric(is.na(k)))))){
      stop("At least one x categorical uncertainty variable has no numeric values")
    }
    if(length(in_xCatUnc) != length(in_xCategorical)){
      stop("x-missclassification variables must have same length as x-categorical variables")
    }
    
  } else {
    xCatUnc <- NULL
  }
  
  if(xCategorical[1] == ""){
    mVars <- c(in_x)
  } else {
    mVars <- c(in_x, xCategorical)
  }
  
  missingVars <- names(which(sapply(mVars, function(x) any(is.na(dataModel[, x])))))
  if(length(missingVars) > 0){
    dataModel2 <- dataModel
    dataModel <- na.omit(dataModel)
    nMissing <- nrow(dataModel2) - nrow(dataModel)
    warning(paste0("Missing values found in following variables: ", paste(missingVars, collapse = ", "), "; ", nMissing, " observations deleted."))
  }
  
  if(!is.numeric(dataModel[, in_y])){
    dataModel[, in_y] <- as.numeric(dataModel[, in_y])
  }
  
  if(!is.null(in_yUnc)){
    yUnc <- as.numeric(as.vector(dataModel[, in_yUnc]))
    if(all(is.na(yUnc))){
      stop("Dependent variable uncertainty has no numeric values")
    }
  } else {
    yUnc <- rep(0, nrow(dataModel))
  }
  
  if(in_regType == "logistic" & any(!(dataModel[, in_y] %in% c(0,1)))){
    stop("Dependent variable must have only 0 and 1 values if logistic regression is selected.")
  }
  
  return(list(xVars = xVars,
              xCategorical = xCategorical,
              xCatUnc = xCatUnc,
              xUnc = xUnc,
              yUnc = yUnc,
              dataModel = dataModel))
}
