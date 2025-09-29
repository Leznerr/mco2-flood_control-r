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

test_that("validate_schema accepts coordinate synonyms", {
  df <- data.frame(
    Region = "NCR", MainIsland = "Luzon", Province = "Metro Manila", FundingYear = 2021,
    TypeOfWork = "Dredging", StartDate = "2021-01-01", ActualCompletionDate = "2021-01-10",
    ApprovedBudgetForContract = 1, ContractCost = 0.9, Contractor = "ABC",
    ProjectLatitude = 14.6, ProjectLongitude = 121.0,
    check.names = FALSE
  )
  expect_silent(validate_schema(df))
})

test_that("validate_schema fails if neither coordinate pair exists", {
  df <- data.frame(
    Region = "NCR", MainIsland = "Luzon", Province = "Metro Manila", FundingYear = 2021,
    TypeOfWork = "Dredging", StartDate = "2021-01-01", ActualCompletionDate = "2021-01-10",
    ApprovedBudgetForContract = 1, ContractCost = 0.9, Contractor = "ABC",
    check.names = FALSE
  )
  expect_error(validate_schema(df), "missing coordinates")
})

test_that("assert_year_filter detects unexpected years", {
  df <- tibble(FundingYear = c(2021L, 2024L))
  expect_error(assert_year_filter(df, 2021:2023), "found disallowed FundingYear")
})

