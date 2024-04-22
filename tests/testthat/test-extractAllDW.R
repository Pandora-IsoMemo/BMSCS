testthat::test_that("Test extractAllDW", {
  testData <-
    readRDS(testthat::test_path("testdata/test_inData.rds"))
  
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractAllDW(allModels = testModels$models,
                 maxLag = 1,
                 dependent = "y",
                 inDat = testData, 
                 asDataFrame = TRUE) %>% 
      bindAllResults(addEmptyRow = TRUE) %>%
      colnames(),
    c("model", "Durbin-Watson Test")
  )
})
