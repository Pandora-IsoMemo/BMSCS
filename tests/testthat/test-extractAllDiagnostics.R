testthat::test_that("Test extractAllDiagnostics", {
  data_path <- testthat::test_path("testdata_large/test_models.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  testModels <- readRDS(data_path)
  
  expect_equal(
    extractAllDiagnostics(allModels = testModels$models,
                          nChains = 3) %>% 
      bindAllResults(addEmptyRow = TRUE) %>%
      colnames(),
    c("Model", "Gelman Scale Reduction Factor", "Raftery and Lewis", "Geweke z-Score", 
      "Heidelberger-Welch")
  )
})
