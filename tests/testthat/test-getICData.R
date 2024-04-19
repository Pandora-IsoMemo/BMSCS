testthat::test_that("Test getICData", {
  testModels <-
    readRDS(testthat::test_path("testdata/test_models.rds"))
  
  expect_equal(
    getICData(
      allFits = testModels$fits,
      modelNames = c("x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2"),
      ic = "Loo"
    ),
    structure(
      list(
        model = c("x1", "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2"),
        Loo = c(
          -11.4598064197826,
          -16.2727908673405,
          -18.9843684391482,
          -19.2971285901485
        ),
        rank = 1:4
      ),
      class = "data.frame",
      row.names = c("x1",
                    "x1 + x4", "x1 + x4 + x3", "x1 + x4 + x3 + x2")
    )
  )
})
