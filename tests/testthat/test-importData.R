test_that("Test formatColumnNames()", {
  vNames <- c("abc", "12hgf", "#j.f", "jg-$jhfl+4")
  formatColumnNames(vNames, isTest = TRUE)
  
  expect_equal(formatColumnNames(vNames, isTest = TRUE),
               c("abc", "x12hgf", "j.f", "jg..jhfl.4"))
})
