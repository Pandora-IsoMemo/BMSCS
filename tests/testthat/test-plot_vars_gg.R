test_that("plot_vars_gg: numeric-numeric (data source) gives scatter", {
  set.seed(1)
  dat <- data.frame(a = rnorm(10), b = rnorm(10))
  p <- plot_vars_gg(source = "data", v1 = "a", v2 = "b", dat = dat)
  expect_s3_class(p, c("gg", "ggplot"))
  has_points <- any(vapply(p$layers, function(L) inherits(L$geom, "GeomPoint"), logical(1)))
  expect_true(has_points)
  expect_equal(p$labels$x, "b")
  expect_equal(p$labels$y, "a")
})

test_that("plot_vars_gg: categorical-numeric (data source) gives boxplot", {
  dat <- data.frame(g = rep(LETTERS[1:3], each = 5), y = rnorm(15))
  p <- plot_vars_gg(source = "data", v1 = "g", v2 = "y", dat = dat)
  expect_s3_class(p, c("gg", "ggplot"))
  has_box <- any(vapply(p$layers, function(L) inherits(L$geom, "GeomBoxplot"), logical(1)))
  expect_true(has_box)
  expect_equal(p$labels$x, "g")
  expect_equal(p$labels$y, "y")
})

test_that("plot_vars_gg: numeric-categorical (data source) gives boxplot", {
  dat <- data.frame(y = rnorm(15), g = rep(LETTERS[1:3], each = 5))
  p <- plot_vars_gg(source = "data", v1 = "y", v2 = "g", dat = dat)
  expect_s3_class(p, c("gg", "ggplot"))
  has_box <- any(vapply(p$layers, function(L) inherits(L$geom, "GeomBoxplot"), logical(1)))
  expect_true(has_box)
  expect_equal(p$labels$x, "g")
  expect_equal(p$labels$y, "y")
})

test_that("plot_vars_gg: both categorical returns NULL", {
  dat <- data.frame(g1 = rep(LETTERS[1:2], each = 5), g2 = rep(letters[1:2], times = 5))
  res <- plot_vars_gg(source = "data", v1 = "g1", v2 = "g2", dat = dat)
  expect_null(res)
})

test_that("plot_vars_gg: model source uses designMatrix", {
  # minimal fake "model" with a designMatrix slot (S4)
  setClass("MockModel", slots = c(designMatrix = "data.frame", type = "character"))
  DM <- data.frame(x = rnorm(12), g = rep(LETTERS[1:3], each = 4))
  mPar <- methods::new("MockModel", designMatrix = DM, type = "whatever")
  model <- list(models = list(sel = mPar), dependent = "ignored")
  modelAVG <- list()
  
  # numeric~categorical via model designMatrix
  p <- plot_vars_gg(
    source = "model", v1 = "x", v2 = "g",
    model = model, modelAVG = modelAVG, modelSelection = "sel"
  )
  expect_s3_class(p, c("gg", "ggplot"))
  has_box <- any(vapply(p$layers, function(L) inherits(L$geom, "GeomBoxplot"), logical(1)))
  expect_true(has_box)
  expect_equal(p$labels$x, "g")
  expect_equal(p$labels$y, "x")
})
