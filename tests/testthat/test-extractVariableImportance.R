testthat::test_that("Test extractRegressionSign", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractSummary(model = testModels$models[[3]], cLevel = 0.8) %>%
      extractCoefTable() %>%
      extractRegressionSign(),
    structure(list(Variable = c("x1", "x41", "x42", "x43", "x44", "x45", "x3"), 
                   Estimate = c(3.68, -2.19, -1.16, 7.82, -2.26, 24.5, 0.971), 
                   Sign = c(1, -1, -1, 1, -1, 1, 1)),
              row.names = 3:9,
              class = "data.frame")
  )
})

testthat::test_that("Test extractVariableImportance", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractGlobalVariableImportance(variableData = testModels$variableData),
    structure(list(Variable = c("x1", "x4", "x3", "x2"), 
                   Importance = c(1, 0.95, 0.94, 0.94)), 
              row.names = c(NA, -4L), 
              class = c("tbl_df", "tbl", "data.frame"))
  )
  
  expect_equal(
    extractModelBasedVariableImportance(model = testModels$models[[3]]),
    structure(list(Variable = c("x45", "x1", "x43", "x41", "x3", "x44", "x42"), 
                   Importance = c(3.97, 3.88, 3.23, 0.94, 0.87, 0.74, 0.51)), 
              class = "data.frame", 
              row.names = c("x45", "x1", "x43", "x41", "x3", "x44", "x42"))
  )
    
  expect_equal(
    extractModelBasedVariableImportance(model = testModels$models[[3]]) %>%
      joinRegressionSign(signData = extractSummary(model = testModels$models[[3]], cLevel = 0.8) %>%
                           extractCoefTable() %>%
                           extractRegressionSign()),
    structure(list(Variable = c("x45", "x1", "x43", "x41", "x3", "x44", "x42"),
                   Importance = c(3.97, 3.88, 3.23, 0.94, 0.87, 0.74, 0.51),
                   Estimate = c(24.5, 3.68, 7.82, -2.19, 0.971, -2.26, -1.16), 
                   Sign = c(1, 1, 1, -1, 1, -1, -1)),
              class = "data.frame",
              row.names = c(NA, -7L))
  )
    
    # join all importance tables but in long format with first column "model" including "global" renderDataTable for filtering options ...
  expect_equal(
    extractAllVariableImportance(
      models = testModels$models,
      variableData = testModels$variableData
    ),
    list(
      global = structure(
        list(
          Variable = c("x1", "x4", "x3", "x2"),
          Importance = c(1, 0.95, 0.94, 0.94),
          Model = c("global", "global",
                    "global", "global"),
          Estimate = c(NA, NA, NA, NA),
          Sign = c(NA,
                   NA, NA, NA)
        ),
        row.names = c(NA, -4L),
        class = c("tbl_df", "tbl",
                  "data.frame")
      ),
      x1 = structure(
        list(
          Model = "x1",
          Variable = "x1",
          Importance = 3.37,
          Estimate = 3.19,
          Sign = 1
        ),
        class = "data.frame",
        row.names = c(NA,
                      -1L)
      ),
      `x1 + x4` = structure(
        list(
          Model = c("x1 + x4", "x1 + x4",
                    "x1 + x4", "x1 + x4", "x1 + x4", "x1 + x4"),
          Variable = c("x1",
                       "x45", "x43", "x41", "x44", "x42"),
          Importance = c(4.34, 4.19,
                         3.8, 0.97, 0.68, 0.4),
          Estimate = c(4.11, 25.8, 9.22, -2.24,
                       -2.09, -0.915),
          Sign = c(1, 1, 1, -1, -1, -1)
        ),
        class = "data.frame",
        row.names = c(NA,
                      -6L)
      ),
      `x1 + x4 + x3` = structure(
        list(
          Model = c(
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3"
          ),
          Variable = c("x45", "x1", "x43",
                       "x41", "x3", "x44", "x42"),
          Importance = c(3.97, 3.88, 3.23,
                         0.94, 0.87, 0.74, 0.51),
          Estimate = c(24.5, 3.68, 7.82, -2.19,
                       0.971, -2.26, -1.16),
          Sign = c(1, 1, 1, -1, 1, -1, -1)
        ),
        class = "data.frame",
        row.names = c(NA,
                      -7L)
      ),
      `x1 + x4 + x3 + x2` = structure(
        list(
          Model = c(
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2"
          ),
          Variable = c("x1", "x45", "x43", "x41",
                       "x44", "x3", "x42", "x2"),
          Importance = c(4.36, 3.82, 3.01, 1.45,
                         1.11, 0.85, 0.82, 0.44),
          Estimate = c(4.13, 23.6, 7.29, -3.38,
                       -3.39, 0.946, -1.87, 0.414),
          Sign = c(1, 1, 1, -1, -1, 1, -1,
                   1)
        ),
        class = "data.frame",
        row.names = c(NA, -8L)
      )
    )
  )
})
