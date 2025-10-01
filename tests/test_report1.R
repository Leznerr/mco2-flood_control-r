source_module <- function(...) {
  rel <- file.path(...)
  candidates <- c(file.path("..", rel), rel)
  for (path in candidates) {
    if (file.exists(path)) {
      source(path, chdir = TRUE)
      return(invisible(TRUE))
    }
  }
  stop(sprintf("Unable to locate module '%s' from test.", rel))
}

source_module("R", "utils_format.R")
source_module("R", "report1.R")

library(testthat)
library(tibble)

test_that("report 1 computes efficiency metrics within bounds", {
  df <- tibble(
    Region = c("Region A", "Region B"),
    MainIsland = c("Island 1", "Island 2"),
    ApprovedBudgetForContract = c(100, 200),
    ContractCost = c(80, 150),
    CostSavings = c(20, 50),
    CompletionDelayDays = c(20, 120)
  )
  report <- report_regional_efficiency(df)
  expect_equal(
    colnames(report),
    c(
      "Region",
      "MainIsland",

      "EfficiencyScore"
    )
  )
  expect_true(all(report$EfficiencyScore >= 0 & report$EfficiencyScore <= 100, na.rm = TRUE))
  expect_equal(report$Region[1], "Region A")
  expect_equal(report$Delay30Rate, c(0, 100))
})

test_that("report 1 handles all-NA groups without crashing", {
  df <- tibble(
    Region = c("Region C", "Region C"),
    MainIsland = c("Island 3", "Island 3"),
    ApprovedBudgetForContract = c(NA_real_, NA_real_),
    ContractCost = c(NA_real_, NA_real_),
    CostSavings = c(NA_real_, NA_real_),
    CompletionDelayDays = c(NA_real_, NA_real_)
  )
  report <- report_regional_efficiency(df)
  expect_true(all(is.na(report$MedianSavings)))
  expect_true(all(is.na(report$AvgDelay)))
  expect_true(all(is.na(report$Delay30Rate)))
  expect_true(all(is.na(report$EfficiencyScore)))
})
