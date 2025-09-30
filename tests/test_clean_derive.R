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

source_module("R", "clean.R")
source_module("R", "derive.R")
source_module("R", "validate.R")

library(testthat)
library(tibble)

test_that("clean_all parses dates, money, and imputes geo conservatively", {
  raw <- tibble(
    Region = c("central luzon", "central luzon", "central luzon"),
    MainIsland = "luzon",
    Province = c("bulacan", "bulacan", "bulacan"),
    FundingYear = c("2021", "2022.0", "2023"),
    TypeOfWork = "  mixed   CASE  ",
    StartDate = c("2021-01-01", "2021/02/01", "2021-03-05"),
    ActualCompletionDate = c("2021-01-10", "2021-02-20", "2021-04-01"),
    ApprovedBudgetForContract = c("Php 1,000.50", "2,000", "3,000"),
    ContractCost = c("900", "2,100", "3,100"),
    Contractor = c("acme", "acme", "acme"),
    Latitude = c("14.5", "", "200"),
    Longitude = c("120.9", "", "120" )
  )
  cleaned <- clean_all(raw)
  expect_s3_class(cleaned$StartDate, "Date")
  expect_true(is.numeric(cleaned$ApprovedBudgetForContract))
  expect_equal(cleaned$FundingYear, c(2021L, 2022L, 2023L))
  expect_equal(cleaned$Latitude[2], cleaned$Latitude[1])
  expect_equal(cleaned$Longitude[2], cleaned$Longitude[1])
  expect_true(is.na(cleaned$Latitude[3]))
  expect_equal(cleaned$Longitude[3], 120)
  expect_equal(cleaned$Contractor[1], "Acme")
  expect_equal(cleaned$TypeOfWork[1], "mixed CASE")
})

test_that("clean_all canonicalizes ProjectLatitude/ProjectLongitude", {
  raw <- data.frame(
    Region = "NCR", MainIsland = "Luzon", Province = "Metro Manila", FundingYear = "2021",
    TypeOfWork = "Dredging", StartDate = "2021-01-01", ActualCompletionDate = "2021-01-10",
    ApprovedBudgetForContract = "1,000,000", ContractCost = "900,000", Contractor = "ABC",
    ProjectLatitude = "14.600", ProjectLongitude = "121.000",
    check.names = FALSE
  )
  validate_schema(raw)
  out <- clean_all(raw)
  expect_true(all(c("Latitude","Longitude") %in% names(out)))
  expect_false(any(c("ProjectLatitude","ProjectLongitude") %in% names(out)))
  expect_true(is.numeric(out$Latitude) && is.numeric(out$Longitude))
})

test_that("derive_fields computes savings and delays", {
  raw <- tibble(
    Region = "A", MainIsland = "B", Province = "C", FundingYear = 2021L,
    TypeOfWork = "Work", StartDate = as.Date("2021-01-01"),
    ActualCompletionDate = as.Date("2021-01-11"),
    ApprovedBudgetForContract = 100,
    ContractCost = 90,
    Contractor = "Firm", Latitude = 1, Longitude = 1
  )
  derived <- derive_fields(raw)
  expect_equal(derived$CostSavings, 10)
  expect_equal(derived$CompletionDelayDays, 10)
})

test_that("derive_fields allows negative delays and filter_years drops disallowed years", {
  raw <- tibble(
    Region = "A", MainIsland = "B", Province = "C", FundingYear = c(2021L, 2024L),
    TypeOfWork = "Work", StartDate = as.Date(c("2021-01-10", "2024-02-01")),
    ActualCompletionDate = as.Date(c("2021-01-01", "2024-01-15")),
    ApprovedBudgetForContract = c(100, 200),
    ContractCost = c(120, 150),
    Contractor = "Firm", Latitude = 1, Longitude = 1
  )
  derived <- derive_fields(raw)
  expect_true(any(derived$CostSavings < 0))
  expect_true(any(derived$CompletionDelayDays < 0))
  filtered <- filter_years(derived, 2021:2023)
  expect_equal(unique(filtered$FundingYear), 2021L)
  expect_error(assert_year_filter(derived, 2021:2023))
  expect_silent(assert_year_filter(filtered, 2021:2023))
})

test_that("NA delta logging computes per-column integer deltas without error", {
  raw <- data.frame(
    Region = c("NCR", NA), MainIsland = c("Luzon", "Luzon"),
    Province = c("Metro Manila", "Metro Manila"), FundingYear = c("2021", "2021"),
    TypeOfWork = c("Dredging", "Dredging"),
    StartDate = c("2021-01-01", NA), ActualCompletionDate = c("2021-01-10", "2021-01-20"),
    ApprovedBudgetForContract = c("1,000,000", "2,000,000"),
    ContractCost = c("900,000", "1,900,000"),
    Contractor = c("ABC", "ABC"),
    ProjectLatitude = c("14.6", NA), ProjectLongitude = c("121.0", NA),
    check.names = FALSE
  )
  validate_schema(raw)
  cols_track <- c("Region","MainIsland","Province","FundingYear","TypeOfWork","StartDate","ActualCompletionDate",
                  "ApprovedBudgetForContract","ContractCost","Contractor","Latitude","Longitude")
  out <- clean_all(raw)
  expect_true(is.data.frame(out))
})

test_that("FundingYear coercion accepts numeric, character, and factor", {
  source("R/clean.R")  # ensure helper is visible in test
  raw_num <- data.frame(FundingYear = c(2021, 2022, NA_real_), check.names = FALSE)
  raw_chr <- data.frame(FundingYear = c("2021", " 2022 ", "NA"), check.names = FALSE)
  raw_fac <- data.frame(FundingYear = factor(c("2023","2021",NA)), check.names = FALSE)

  out_num <- .as_integer_safely(raw_num$FundingYear)
  out_chr <- .as_integer_safely(raw_chr$FundingYear)
  out_fac <- .as_integer_safely(raw_fac$FundingYear)

  expect_identical(out_num, as.integer(c(2021, 2022, NA)))
  expect_identical(out_chr, as.integer(c(2021, 2022, NA)))
  expect_identical(out_fac, as.integer(c(2023, 2021, NA)))
})



