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
source_module("R", "report3.R")

library(testthat)
library(tibble)

test_that("report 3 applies baseline logic correctly", {
  df <- tibble(
    FundingYear = c(2021L, 2022L, 2023L, 2021L, 2022L),
    TypeOfWork = c("Type A", "Type A", "Type A", "Type B", "Type B"),
    CostSavings = c(100, 150, -50, 0, 10)
  )
  report <- report_overrun_trends(df)
  a_rows <- report[report$TypeOfWork == "Type A", ]
  b_rows <- report[report$TypeOfWork == "Type B", ]
  expect_true(all(is.na(a_rows$YoY_vs_2021[a_rows$FundingYear == 2021])))
  expect_true(is.na(b_rows$YoY_vs_2021[b_rows$FundingYear == 2022]))
  expect_equal(report$FundingYear, sort(report$FundingYear))
})

test_that("report 3 survives all-NA savings groups", {
  df <- tibble(
    FundingYear = c(2021L, 2022L),
    TypeOfWork = c("Type C", "Type C"),
    CostSavings = c(NA_real_, NA_real_)
  )
  report <- report_overrun_trends(df)
  expect_true(all(is.na(report$AvgSavings)))
  expect_true(all(is.na(report$OverrunRate)))
  expect_true(all(is.na(report$YoY_vs_2021)))
})

