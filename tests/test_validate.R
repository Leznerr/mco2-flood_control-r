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

source_module("R", "validate.R")

library(testthat)
library(tibble)

test_that("validate_schema detects missing columns", {
  df <- tibble(Region = "NCR", FundingYear = 2021)
  expect_error(validate_schema(df), "missing required columns")
})

test_that("validate_schema rejects unexpected columns", {
  df <- tibble(
    Region = "NCR", MainIsland = "Luzon", Province = "Metro Manila",
    FundingYear = 2021, TypeOfWork = "Work", StartDate = "2021-01-01",
    ActualCompletionDate = "2021-02-01", ApprovedBudgetForContract = 1,
    ContractCost = 1, Contractor = "A", Latitude = 1, Longitude = 1,
    ExtraColumn = 123
  )
  expect_error(validate_schema(df), "unexpected extra columns")
})

test_that("validate_schema enforces integer-like FundingYear", {
  df <- tibble(
    Region = "NCR", MainIsland = "Luzon", Province = "Metro Manila",
    FundingYear = "2021.5", TypeOfWork = "Work", StartDate = "2021-01-01",
    ActualCompletionDate = "2021-02-01", ApprovedBudgetForContract = 1,
    ContractCost = 1, Contractor = "A", Latitude = 1, Longitude = 1
  )
  expect_error(validate_schema(df), "FundingYear values must be coercible")
})

test_that("assert_year_filter detects unexpected years", {
  df <- tibble(FundingYear = c(2021L, 2024L))
  expect_error(assert_year_filter(df, 2021:2023), "unexpected FundingYear")
})

