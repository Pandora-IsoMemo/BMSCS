testthat::test_that("Test extractRegressionSign", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractSummary(model = testModels$models[[3]], cLevel = 0.8) %>%
      extractCoefTable() %>%
      extractRegressionSign(),
    structure(list(
      c("x1", "x41", "x42", "x43", "x44", "x45", "x3"
      ),
      Estimate = c(3.68, -2.19, -1.16, 7.82, -2.26, 24.5, 0.971), 
      sign = c(1, -1, -1, 1, -1, 1, 1)), row.names = 3:9, class = "data.frame")
  )
})

testthat::test_that("Test extractVariableImportance", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractGlobalVariableImportance(variableData = testModels$variableData),
    structure(list(Variable = c("x1", "x4", "x3", "x2"), 
                   Importance = c(0.99876, 0.945968, 0.9441, 0.93732)),
              row.names = c(NA, -4L),
              class = c("tbl_df", "tbl", "data.frame"))
  )
  
  expect_equal(
    extractModelBasedVariableImportance(model = testModels$models[[3]]),
    structure(list(Variable = c("x45", "x1", "x43", "x41", "x3", "x44", "x42"), 
                   Importance = c(3.96568309126455, 3.88496702913103, 
                                  3.22526985562661, 0.942401200886773, 0.874989835067909, 0.738855561060386, 
                                  0.50772549303037)), 
              class = "data.frame", 
              row.names = c("x45", "x1", "x43", "x41", "x3", "x44", "x42"))
  )
    
  expect_equal(
    extractModelBasedVariableImportance(model = testModels$models[[3]]) %>%
      joinRegressionSign(signData = extractSummary(model = testModels$models[[3]], cLevel = 0.8) %>%
                           extractCoefTable() %>%
                           extractRegressionSign()),
    structure(list(Variable = c("x45", "x1", "x43", "x41", "x3", "x44", "x42"),
                   Importance = c(3.96568309126455, 3.88496702913103, 
                                  3.22526985562661, 0.942401200886773, 0.874989835067909, 0.738855561060386, 
                                  0.50772549303037),
                   Estimate = c(24.5, 3.68, 7.82, -2.19, 0.971, -2.26, -1.16), 
                   Sign = c(1, 1, 1, -1, 1, -1, -1)),
              class = "data.frame",
              row.names = c(NA, -7L))
  )
    
    # join all importance tables but in long format with first column "model" including "global" renderDataTable for filtering options ...
  expect_equal(
    extractAllVariableImportance(models = testModels),
    structure(list(Model = c("global", "global", "global", "global", 
                             "x1", "x1 + x4", "x1 + x4", "x1 + x4", "x1 + x4", "x1 + x4", 
                             "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3", "x1 + x4 + x3", "x1 + x4 + x3", 
                             "x1 + x4 + x3", "x1 + x4 + x3", "x1 + x4 + x3", "x1 + x4 + x3 + x2", 
                             "x1 + x4 + x3 + x2", "x1 + x4 + x3 + x2", "x1 + x4 + x3 + x2", 
                             "x1 + x4 + x3 + x2", "x1 + x4 + x3 + x2", "x1 + x4 + x3 + x2", 
                             "x1 + x4 + x3 + x2"), 
                   Variable = c("x1", "x4", "x3", "x2", "x1", 
                                "x1", "x45", "x43", "x41", "x44", "x42", "x45", "x1", "x43", 
                                "x41", "x3", "x44", "x42", "x1", "x45", "x43", "x41", "x44", 
                                "x3", "x42", "x2"), 
                   Importance = c(0.99876, 0.945968, 0.9441, 
                                  0.93732, 3.36835800698973, 4.33578889416172, 4.19156872371605, 
                                  3.80433905151885, 0.965102390139439, 0.682275509447947, 0.400745239657929, 
                                  3.96568309126455, 3.88496702913103, 3.22526985562661, 0.942401200886773, 
                                  0.874989835067909, 0.738855561060386, 0.50772549303037, 4.36207868625867, 
                                  3.82118581623893, 3.00701898796198, 1.45271712678277, 1.10989718652468, 
                                  0.852060089302514, 0.820489084872084, 0.443167683735877), 
                   Estimate = c(NA, 
                                NA, NA, NA, 3.19, 4.11, 25.8, 9.22, -2.24, -2.09, -0.915, 24.5, 
                                3.68, 7.82, -2.19, 0.971, -2.26, -1.16, 4.13, 23.6, 7.29, -3.38, 
                                -3.39, 0.946, -1.87, 0.414), 
                   Sign = c(NA, NA, NA, NA, 1, 1, 1, 
                            1, -1, -1, -1, 1, 1, 1, -1, 1, -1, -1, 1, 1, 1, -1, -1, 1, -1, 
                            1)),
              row.names = c(NA, -26L), 
              class = c("tbl_df", "tbl", "data.frame"))
  )
  
})
