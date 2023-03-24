testthat::test_that("Test prepareData", {
  # Test module prepareData with example data
  testData <- generateExampleData()
  
  testPrepData <-
    prepareData(testData,
                in_x = c("x1", "x2", "x3"),
                in_xUnc = NULL,
                in_y = "y",
                in_yUnc = "yUncertainty",
                in_xCategorical = "x4",
                in_xCatUnc = NULL,
                in_regType = "logistic")
  
  testthat::expect_length(testPrepData, 5)
  testthat::expect_equal(names(testPrepData), c("xCategorical", "xCatUnc", "xUnc", "yUnc", "dataModel"))
  testthat::expect_equal(testPrepData[1:3] = list(xCategorical = "x4", xCatUnc = NULL, xUnc = NULL))
  testthat::expect_equal(head(testPrepData[[4]]) = c(0.001, 0.001, 0.001, 0.002, 0.001, 0))
  testthat::expect_equal(head(testPrepData[[5]]),
                         structure(
                           list(
                             x1 = c(1.371, 1.16, 0.034, 1.035, 0.636, 0.319),
                             x2 = c(-1.65, 0.559, -1.573, -0.067, 0.755, -0.159),
                             x3 = c(0.75,
                                    0.323, 0.944, 0.469, 0.182, 1.349),
                             y = c(1, 1, 1, 1, 1, 1),
                             yUncertainty = c(0.001, 0.001, 0.001, 0.002, 0.001, 0),
                             x4 = c("5",
                                    "0", "5", "4", "4", "2")
                           ),
                           row.names = c(NA, 6L),
                           class = "data.frame"
                         ))
})

testthat::test_that("Test generateFormula", {
  # Test module prepareData with example data
  testData <- generateExampleData()
  
  testPrepData <-
    prepareData(testData,
                in_x = c("x1", "x2", "x3"),
                in_xUnc = NULL,
                in_y = "y",
                in_yUnc = "yUncertainty",
                in_xCategorical = "x4",
                in_xCatUnc = NULL,
                in_regType = "logistic")
  
  testthat::expect_equal(generateFormula("y", testPrepData$xVars) %>%
                           as.character(),
                         c("~", "y", "x1 + x2 + x3 + x4"))
})
