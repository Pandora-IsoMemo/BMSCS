# tests/testthat/test-plot_roc_gg_app.R

test_that("plot_roc_gg_app: guards return NULL with messages", {
  data(aSAH, package = "pROC")
  dat <- aSAH
  
  mPar <- glm(outcome ~ s100b, data = dat, family = binomial)
  
  model    <- list(models = list(logit = structure(mPar, type = "logistic")),
                   dependent = "outcome")
  modelAVG <- list()
  
  # 1) missing modelSelection
  expect_message(
    res1 <- plot_roc_gg_app(
      modelSelection = NULL, AUCI = TRUE, AUC = TRUE,
      rocAxis = 1, rocAxisT = 1, roctitle = "t", rocT = 1,
      model = model, modelAVG = modelAVG, dat = dat
    ),
    "modelSelection is missing\\.", perl = TRUE
  )
  expect_null(res1)
  
  # 2) model not found
  expect_message(
    res2 <- plot_roc_gg_app(
      modelSelection = "nope", AUCI = TRUE, AUC = TRUE,
      rocAxis = 1, rocAxisT = 1, roctitle = "t", rocT = 1,
      model = model, modelAVG = modelAVG, dat = dat
    ),
    "Selected model not found\\.", perl = TRUE
  )
  expect_null(res2)
  
  # 3) non-logistic: returns NULL with message
  mod_lin <- lm(as.numeric(outcome == "Poor") ~ s100b, data = dat)
  model_nonlog <- list(models = list(lin = structure(mod_lin, type = "linear")),
                       dependent = "outcome")
  
  expect_message(
    res3 <- plot_roc_gg_app(
      modelSelection = "lin", AUCI = TRUE, AUC = TRUE,
      rocAxis = 1, rocAxisT = 1, roctitle = "t", rocT = 1,
      model = model_nonlog, modelAVG = list(), dat = dat
    ),
    "ROC curve is only available for logistic models\\.", perl = TRUE
  )
  expect_null(res3)
})

test_that("plot_roc_gg_app: baseline ggplot structure without AUC text", {
  data(aSAH, package = "pROC")
  dat <- aSAH
  
  mPar <- glm(outcome ~ s100b, data = dat, family = binomial)
  model    <- list(models = list(logit = structure(mPar, type = "logistic")),
                   dependent = "outcome")
  modelAVG <- list()
  
  p <- plot_roc_gg_app(
    modelSelection = "logit", AUCI = FALSE, AUC = FALSE,
    rocAxis = 1, rocAxisT = 1, roctitle = "ROC (aSAH)", rocT = 1.1,
    model = model, modelAVG = modelAVG, dat = dat
  )
  
  expect_s3_class(p, c("gg", "ggplot"))
  
  # diagonal present
  has_abline <- any(vapply(p$layers, function(L) inherits(L$geom, "GeomAbline"), logical(1)))
  expect_true(has_abline)
  
  # axis labels
  expect_equal(p$labels$x, "Specificity (%)")
  expect_equal(p$labels$y, "Sensitivity (%)")
  
  # no AUC text layer when AUC = FALSE
  has_text <- any(vapply(p$layers, function(L) inherits(L$geom, "GeomText"), logical(1)))
  expect_false(has_text)
})

test_that("plot_roc_gg_app: AUC text toggles and CI content", {
  data(aSAH, package = "pROC")
  dat <- aSAH
  mPar <- glm(outcome ~ s100b, data = dat, family = binomial)
  model    <- list(models = list(logit = structure(mPar, type = "logistic")),
                   dependent = "outcome")
  modelAVG <- list()
  
  get_text_labels <- function(p) {
    labs <- unlist(lapply(p$layers, function(L) {
      if (!inherits(L$geom, "GeomText")) return(character(0))
      c(
        if (!is.null(L$data) && "label" %in% names(L$data)) as.character(L$data$label) else character(0),
        if (!is.null(L$aes_params$label)) as.character(L$aes_params$label) else character(0),
        if (!is.null(L$geom_params$label)) as.character(L$geom_params$label) else character(0)
      )
    }))
    pb <- ggplot_build(p)
    labs_built <- unlist(lapply(pb$data, function(d) if ("label" %in% names(d)) as.character(d$label) else character(0)))
    unique(c(labs, labs_built))
  }
  
  # AUC shown, no CI
  p1 <- plot_roc_gg_app(
    modelSelection = "logit", AUCI = FALSE, AUC = TRUE,
    rocAxis = 1, rocAxisT = 1, roctitle = "ROC (aSAH)", rocT = 1.1,
    model = model, modelAVG = modelAVG, dat = dat
  )
  lbl1 <- get_text_labels(p1)
  expect_true(any(grepl("^AUC:\\s*[0-9]+\\.?[0-9]*%\\s*$", lbl1)))
  
  # AUC shown, CI included
  p2 <- plot_roc_gg_app(
    modelSelection = "logit", AUCI = TRUE, AUC = TRUE,
    rocAxis = 1, rocAxisT = 1, roctitle = "ROC (aSAH)", rocT = 1.1,
    model = model, modelAVG = modelAVG, dat = dat
  )
  lbl2 <- get_text_labels(p2)
  expect_true(any(grepl("^AUC:\\s*[0-9]+\\.?[0-9]*%\\s*\\([0-9]+\\.?[0-9]*%[[:space:]]*-[[:space:]]*[0-9]+\\.?[0-9]*%\\)$", lbl2)))
  
  # AUC hidden (regardless of AUCI)
  p3 <- plot_roc_gg_app(
    modelSelection = "logit", AUCI = TRUE, AUC = FALSE,
    rocAxis = 1, rocAxisT = 1, roctitle = "ROC (aSAH)", rocT = 1.1,
    model = model, modelAVG = modelAVG, dat = dat
  )
  lbl3 <- get_text_labels(p3)
  expect_false(any(grepl("^AUC:", lbl3)))
})

test_that("plot_roc_gg_app: sizing inputs produce a plot", {
  data(aSAH, package = "pROC")
  dat <- aSAH
  mPar <- glm(outcome ~ s100b, data = dat, family = binomial)
  
  model    <- list(models = list(logit = structure(mPar, type = "logistic")),
                   dependent = "outcome")
  modelAVG <- list()
  
  p <- plot_roc_gg_app(
    modelSelection = "logit", AUCI = TRUE, AUC = TRUE,
    rocAxis = 1.2, rocAxisT = 0.9, roctitle = "ROC (aSAH)", rocT = 1.3,
    model = model, modelAVG = modelAVG, dat = dat
  )
  
  expect_s3_class(p, c("gg", "ggplot"))
  # sanity: AUC label present
  pb <- ggplot_build(p)
  labels <- unlist(lapply(pb$data, function(d) if ("label" %in% names(d)) as.character(d$label) else character(0)))
  expect_true(any(grepl("^AUC:", labels)))
})
