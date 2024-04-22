testthat::test_that("Test extractAllDiagnostics", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractAllDiagnostics(allModels = testModels$models,
                          nChains = 3) %>% 
      bindAllResults(addEmptyRow = TRUE) %>%
      colnames(),
    c("model", "Gelman Scale Reduction Factor", "Raftery and Lewis", "Geweke z-Score", 
      "Heidelberger-Welch")
  )
})
