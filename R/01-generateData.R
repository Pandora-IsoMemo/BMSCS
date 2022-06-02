generateExampleData <- function() {
  set.seed(72)
  n <- 75
  x1 <- rnorm(n, sd = 1)
  x2 <- rnorm(n, sd = 1)
  x3 <- rnorm(n, sd = 1)
  # x1Unc <- rnorm(n, sd = 0.1)
  # x2Unc <- rnorm(n, sd = 0.1)
  # x3UNc <- rnorm(n, sd = 0.1)
  x4 <- rpois(n, lambda = 2)
  #regression formula
  y <-
    0.1 + 0.3 * x1 + 0.3 * x1 * x3 + 0.4 * x1 * x2 ^ 2 + 0.3 * 4 + rnorm(n, sd = 0.3)
  y[y<0.1] <- 0
  y[y>=0.1] <- 1
  yUncertainty <- rexp(n, 10) * 0.01
  data <- round(data.frame(x1, x2, x3, y, yUncertainty, x4), 3)
  data$x4 <- as.character(data$x4)
  data
}

generateFormula <- function(y, x){
  as.formula(paste0(y, "~", paste0(x, collapse = "+")))
}
