testthat::test_that("Test getICData", {
  data_path <- testthat::test_path("testdata_large/test_models.rds")
  skip_if_not(file.exists(data_path), "Skipping large data test on CI or for devtools:check()")
  
  testModels <- readRDS(data_path)
  
  expect_equal(
    getICData(
      allFits = testModels$fits,
      modelNames = c("x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2"),
      ic = "Loo"
    ),
    structure(
      list(
        Model = c("x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2"),
        Loo = c(
          -11.4598064197826,
          -16.2727908673405,
          -18.9843684391482,
          -19.2971285901485
        ),
        Rank = 1:4
      ),
      class = "data.frame",
      row.names = c("x1",
                    "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2")
    )
  )
  
  ICList <- c("AUC", "Rsq", "RsqAdj", "Bayes_Rsq", "df", "logLik", "nagelkerke", "Loo", "WAIC")
  dataList <- lapply(ICList,
                     function(x)
                     getICData(ic = x,
                     allFits = testModels$fits,
                     modelNames = c("x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2"),
                     withColumnICName = TRUE)
                     )
  
  expect_equal(dataList %>% bindAllResults(addEmptyRow = TRUE),
               structure(list(
                 IC_name = c("AUC", "AUC", "AUC", "AUC", NA, "Rsq", 
                             "Rsq", "Rsq", "Rsq", NA, "RsqAdj", "RsqAdj", "RsqAdj", "RsqAdj", 
                             NA, "Bayes_Rsq", "Bayes_Rsq", "Bayes_Rsq", "Bayes_Rsq", NA, "df", 
                             "df", "df", "df", NA, "logLik", "logLik", "logLik", "logLik", 
                             NA, "nagelkerke", "nagelkerke", "nagelkerke", "nagelkerke", NA, 
                             "Loo", "Loo", "Loo", "Loo", NA, "WAIC", "WAIC", "WAIC", "WAIC", 
                             NA),
                 Model = c("x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", 
                           NA, "x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", NA, 
                           "x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", NA, "x1", 
                           "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", NA, "x1", "x1 + x4", 
                           "x1 + x4 + x3", "x1 + x4 + x3 + x2", NA, "x1", "x1 + x4", "x1 + x4 + x3", 
                           "x1 + x4 + x3 + x2", NA, "x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", 
                           NA, "x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", NA, 
                           "x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2", NA), 
                 IC_value = c(0.957142857142857, 
                              0.962857142857143, 0.957142857142857, 0.96, NA, 0.36068144378523, 
                              0.425942775926767, 0.467135690741719, 0.455680256920982, NA, 
                              -2726460732183829504, -2.77223026876677e+32, -1462177.30365661, 
                              -56728840240360.9, NA, NA, NA, NA, NA, NA, 73, 68, 67, 66, NA, 
                              -10.2300467734975, -11.0160492452429, -11.569378364397, -11.5322963907839, 
                              NA, 0.550548160892273, 0.583401713015593, 0.587400590469505, 
                              0.598045330919951, NA, -11.4598064197826, -16.2727908673405, 
                              -18.9843684391482, -19.2971285901485, NA, -11.3830747202506, 
                              -15.1187771450832, -17.1693691600503, -17.6905535609402, NA), 
                 Rank = c(4L, 1L, 4L, 2L, NA, 4L, 3L, 1L, 2L, NA, 3L, 4L, 
                          1L, 2L, NA, NA, NA, NA, NA, NA, 1L, 2L, 3L, 4L, NA, 1L, 2L, 
                          4L, 3L, NA, 4L, 3L, 2L, 1L, NA, 1L, 2L, 3L, 4L, NA, 1L, 2L, 
                          3L, 4L, NA)),
                 class = "data.frame", 
                 row.names = c(NA, -45L)
                 )
  )
})
