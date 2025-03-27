testthat::test_that("Test extractAllDW", {
  data_path <- testthat::test_path("testdata_large/test_models.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  testModels <- readRDS(data_path)
  
  testData <-
    readRDS(testthat::test_path("testdata_large/test_inData.rds"))
  
  expect_equal(
    extractAllDW(allModels = testModels$models,
                 maxLag = 1,
                 dependent = "y",
                 inDat = testData, 
                 asDataFrame = TRUE) %>% 
      bindAllResults(addEmptyRow = TRUE) %>%
      colnames(),
    c("Model", "Durbin-Watson Test")
  )
})
