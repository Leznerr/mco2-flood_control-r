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
source_module("R", "report2.R")

library(testthat)
library(tibble)

make_contractor <- function(name, n = 5, cost = 100, savings = 20, delay = 10) {
  tibble(
    Region = "R", MainIsland = "I", Province = "P", FundingYear = 2021L,
    TypeOfWork = "Work", StartDate = as.Date("2021-01-01"),
    ActualCompletionDate = as.Date("2021-01-10"),
    ApprovedBudgetForContract = cost + savings,
    ContractCost = cost,
    Contractor = name,
    Latitude = 1, Longitude = 1,
    CostSavings = savings,
    CompletionDelayDays = delay
  )[rep(1, n), ]
}

test_that("report 2 enforces eligibility and ranking rules", {
  contractors <- lapply(1:16, function(i) {
    make_contractor(sprintf("Contractor %02d", i), cost = 100 + i * 10, savings = 20 + i, delay = 15)
  })
  contractors[[1]] <- make_contractor("Contractor 01", cost = 200, savings = -50, delay = 60)
  contractors <- append(contractors, list(make_contractor("Short Firm", n = 4)))
  df <- dplyr::bind_rows(contractors)
  report <- report_contractor_ranking(df)
  expect_lte(nrow(report), 15)
  expect_false("Short Firm" %in% report$Contractor)
  risk <- report[report$Contractor == "Contractor 01", "RiskFlag", drop = TRUE]
  expect_equal(risk, "High Risk")
})

