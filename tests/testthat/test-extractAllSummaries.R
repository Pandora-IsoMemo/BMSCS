testthat::test_that("Test extractAllSummaries", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    extractAllSummaries(allModels = testModels$models,
                        cLevel = 0.8),
    list(
      x1 = structure(
        list(
          Model = c("x1", "x1", "x1", "x1", "x1",
                    "x1", "x1", "x1", "x1"),
          `Model Summary` = c(
            "Model formula: y ~ x1 ",
            "",
            "            Estimate Median   SD Cred_Interval_",
            "(Intercept)     5.77   5.53 1.65    [3.9, 8.05]",
            "x1              3.19   3.05 1.09   [1.94, 4.65]",
            "",
            "AR1_par: -0.0117 (Cred_Interval_0.8: [-0.756, 0.781])",
            "",
            "Log-likelihood: -10.23"
          )
        ),
        class = "data.frame",
        row.names = c(NA,
                      -9L)
      ),
      `x1 + x4` = structure(
        list(
          Model = c(
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4",
            "x1 + x4"
          ),
          `Model Summary` = c(
            "Model formula: y ~ x1 + x4 ",
            "",
            "            Estimate Median    SD Cred_Interval_",
            "(Intercept)    8.240  7.940  2.93   [4.76, 12.1]",
            "x1             4.110  3.980  1.44   [2.41, 5.91]",
            "x41           -2.240 -2.160  2.38 [-5.36, 0.588]",
            "x42           -0.915 -0.841  1.93  [-3.34, 1.45]",
            "x43            9.220  6.520 10.40 [-0.729, 22.4]",
            "x44           -2.090 -2.000  2.86  [-5.86, 1.27]",
            "x45           25.800 17.300 32.30  [-2.87, 62.4]",
            "",
            "AR1_par: -0.00486 (Cred_Interval_0.8: [-0.812, 0.812])",
            "",
            "Log-likelihood: -11.016"
          )
        ),
        class = "data.frame",
        row.names = c(NA,
                      -14L)
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
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3",
            "x1 + x4 + x3"
          ),
          `Model Summary` = c(
            "Model formula: y ~ x1 + x4 + x3 ",
            "",
            "            Estimate Median    SD Cred_Interval_",
            "(Intercept)    8.730  8.540  2.97   [5.02, 12.7]",
            "x1             3.680  3.490  1.58    [1.8, 5.74]",
            "x41           -2.190 -2.120  2.51 [-5.39, 0.799]",
            "x42           -1.160 -1.070  2.11   [-4.01, 1.5]",
            "x43            7.820  5.580  9.93  [-2.02, 20.1]",
            "x44           -2.260 -2.100  2.97   [-6.17, 1.4]",
            "x45           24.500 15.200 33.80  [-3.96, 62.2]",
            "x3             0.971  0.948  1.24 [-0.557, 2.52]",
            "",
            "AR1_par: 0.0288 (Cred_Interval_0.8: [-0.79, 0.827])",
            "",
            "Log-likelihood: -11.569"
          )
        ),
        class = "data.frame",
        row.names = c(NA,
                      -15L)
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
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2",
            "x1 + x4 + x3 + x2"
          ),
          `Model Summary` = c(
            "Model formula: y ~ x1 + x4 + x3 + x2 ",
            "",
            "            Estimate Median     SD Cred_Interval_",
            "(Intercept)    9.990  9.680  4.070   [4.93, 15.7]",
            "x1             4.130  3.940  1.780   [2.01, 6.53]",
            "x41           -3.380 -3.080  3.490 [-8.29, 0.864]",
            "x42           -1.870 -1.680  2.680  [-5.51, 1.47]",
            "x43            7.290  5.260  9.970  [-2.58, 19.8]",
            "x44           -3.390 -3.370  3.740  [-8.26, 1.03]",
            "x45           23.600 14.900 31.600  [-5.25, 65.5]",
            "x3             0.946  0.932  1.190 [-0.572, 2.41]",
            "x2             0.414  0.381  0.844 [-0.618, 1.53]",
            "",
            "AR1_par: -0.0219 (Cred_Interval_0.8: [-0.785, 0.756])",
            "",
            "Log-likelihood: -11.532"
          )
        ),
        class = "data.frame",
        row.names = c(NA,
                      -16L)
      )
    )
  )
  
  expect_equal(
    extractAllSummaries(allModels = testModels$models,
                        cLevel = 0.8) %>%
      bindAllResults(addEmptyRow = TRUE),
    structure(
      list(
        Model = c(
          "x1",
          "x1",
          "x1",
          "x1",
          "x1",
          "x1",
          "x1",
          "x1",
          "x1",
          NA,
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          "x1 + x4",
          NA,
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          "x1 + x4 + x3",
          NA,
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          "x1 + x4 + x3 + x2",
          NA
        ),
        `Model Summary` = c(
          "Model formula: y ~ x1 ",
          "",
          "            Estimate Median   SD Cred_Interval_",
          "(Intercept)     5.77   5.53 1.65    [3.9, 8.05]",
          "x1              3.19   3.05 1.09   [1.94, 4.65]",
          "",
          "AR1_par: -0.0117 (Cred_Interval_0.8: [-0.756, 0.781])",
          "",
          "Log-likelihood: -10.23",
          NA,
          "Model formula: y ~ x1 + x4 ",
          "",
          "            Estimate Median    SD Cred_Interval_",
          "(Intercept)    8.240  7.940  2.93   [4.76, 12.1]",
          "x1             4.110  3.980  1.44   [2.41, 5.91]",
          "x41           -2.240 -2.160  2.38 [-5.36, 0.588]",
          "x42           -0.915 -0.841  1.93  [-3.34, 1.45]",
          "x43            9.220  6.520 10.40 [-0.729, 22.4]",
          "x44           -2.090 -2.000  2.86  [-5.86, 1.27]",
          "x45           25.800 17.300 32.30  [-2.87, 62.4]",
          "",
          "AR1_par: -0.00486 (Cred_Interval_0.8: [-0.812, 0.812])",
          "",
          "Log-likelihood: -11.016",
          NA,
          "Model formula: y ~ x1 + x4 + x3 ",
          "",
          "            Estimate Median    SD Cred_Interval_",
          "(Intercept)    8.730  8.540  2.97   [5.02, 12.7]",
          "x1             3.680  3.490  1.58    [1.8, 5.74]",
          "x41           -2.190 -2.120  2.51 [-5.39, 0.799]",
          "x42           -1.160 -1.070  2.11   [-4.01, 1.5]",
          "x43            7.820  5.580  9.93  [-2.02, 20.1]",
          "x44           -2.260 -2.100  2.97   [-6.17, 1.4]",
          "x45           24.500 15.200 33.80  [-3.96, 62.2]",
          "x3             0.971  0.948  1.24 [-0.557, 2.52]",
          "",
          "AR1_par: 0.0288 (Cred_Interval_0.8: [-0.79, 0.827])",
          "",
          "Log-likelihood: -11.569",
          NA,
          "Model formula: y ~ x1 + x4 + x3 + x2 ",
          "",
          "            Estimate Median     SD Cred_Interval_",
          "(Intercept)    9.990  9.680  4.070   [4.93, 15.7]",
          "x1             4.130  3.940  1.780   [2.01, 6.53]",
          "x41           -3.380 -3.080  3.490 [-8.29, 0.864]",
          "x42           -1.870 -1.680  2.680  [-5.51, 1.47]",
          "x43            7.290  5.260  9.970  [-2.58, 19.8]",
          "x44           -3.390 -3.370  3.740  [-8.26, 1.03]",
          "x45           23.600 14.900 31.600  [-5.25, 65.5]",
          "x3             0.946  0.932  1.190 [-0.572, 2.41]",
          "x2             0.414  0.381  0.844 [-0.618, 1.53]",
          "",
          "AR1_par: -0.0219 (Cred_Interval_0.8: [-0.785, 0.756])",
          "",
          "Log-likelihood: -11.532",
          NA
        )
      ),
      class = "data.frame",
      row.names = c(NA,
                    -58L)
    )
  )
  
  # expect_no_error(
  #   shinyTools::exportXLSX(file = file.path(tempdir(), "testSummary.xlsx"),
  #                          dat = extractAllSummaries(
  #                            allModels = testModels$models,
  #                            cLevel = 0.8
  #                          ))
  # )
})
