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
    CompletionDelayDays = c(20, 40)
  )
  report <- report_regional_efficiency(df)
  expect_equal(colnames(report), c("Region", "MainIsland", "TotalApprovedBudget", "MedianSavings", "AvgDelay", "Delay30Rate", "EfficiencyScore"))
  expect_true(all(report$EfficiencyScore >= 0 & report$EfficiencyScore <= 100, na.rm = TRUE))
  expect_equal(report$Region[1], "Region B")
  expect_equal(report$Delay30Rate, c(100, 0))
})

