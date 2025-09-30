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
source_module("R", "summary.R")

library(testthat)
library(tibble)

test_that("summary aggregates scalar metrics", {
  df <- tibble(
    Contractor = c("A", "B", "B"),
    Province = c("P1", "P2", "P2"),
    CompletionDelayDays = c(10, NA, 20),
    CostSavings = c(100, 200, -50)
  )
  payload <- build_summary(df)
  expect_equal(payload$total_projects, 3)
  expect_equal(payload$total_contractors, 2)
  expect_equal(payload$total_provinces, 2)
  expect_equal(payload$global_avg_delay, 15)
  expect_equal(payload$total_savings, 250)
})

test_that("summary total_savings is finite or NA but never absurd", {
  df <- tibble(
    Contractor = rep("C", 1001),
    Province = rep("P", 1001),
    CompletionDelayDays = rep(NA_real_, 1001),
    CostSavings = c(rep(1e6, 1000), 1e146)
  )
  payload <- build_summary(df)
  expect_true(is.na(payload$total_savings) || abs(payload$total_savings) <= 1e13)
})

