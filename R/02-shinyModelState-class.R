new_ShinyModelState <- function(data, inputs, model = NULL, notes = NULL) {
  # normalize data
  if (is.null(data) || length(data) == 0) {
    data <- NULL
  }
  
  # normalize inputs
  if (is.null(inputs) || length(inputs) == 0) {
    inputs <- list()
  }
  
  # normalize empty model
  if (is.null(model) || length(model$models) == 0) {
    model <- NULL
  }
  
  structure(list(
    data = data,
    inputs = inputs,
    model = model,
    notes = notes
  ), class = "ShinyModelState")
}

get_secondary_input_names <- function(obj,
                                      secondaryIDs = c("mustExclude",
                                                       "mustInclude",
                                                       "modelSelection",
                                                       "variableSelectionCat",
                                                       "v1",
                                                       "v2")) {
  inputList <- obj$inputs
  inputNames <- names(inputList)
  
  pattern <- paste0(secondaryIDs, collapse = "|")
  matches <- grepl(pattern, inputNames)
  
  inputNames[matches]
}

restoreData <- function(obj, session, dataSetter, notesSetter) {
  dataSetter(obj$data)
  notesSetter("")
  notesSetter(obj$notes)
}

restoreInputs <- function(obj, session, validInputs) {
  inputs <- obj$inputs
  
  inputIDs <- names(inputs)
  # filter inputs to only those that are present in the session
  inputIDs <- inputIDs[inputIDs %in% validInputs]
  for (id in inputIDs) {
    session$sendInputMessage(id, list(value = inputs[[id]]))
  }
}

restoreModel <- function(obj, session, modelSetter, m_AVGSetter) {
  if (!is.null(obj$model) && length(obj$model$models) > 0) {
    modelNames <- names(obj$model$models)
    indexAvgModel <- grepl("model_average", modelNames)

    # extract average model if it exists
    if (any(indexAvgModel)) {
      # get average model
      avgModel <- obj$model$models[modelNames[indexAvgModel]]
      # remove average model from models list
      obj$model$models[[modelNames[indexAvgModel]]] <- NULL
      # set the single models without the average model
      modelSetter(obj$model)
      # set the average model separately
      m_AVGSetter(avgModel)
    } else {
      modelSetter(obj$model)
    }
  }
}

has_model <- function(x) {
  inherits(x, "ShinyModelState") && !is.null(x$model) && length(x$model$models) > 0
}