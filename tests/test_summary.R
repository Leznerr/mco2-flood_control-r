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

source_module("R", "utils_log.R")
source_module("R", "utils_format.R")
source_module("R", "io.R")
source_module("R", "ingest.R")
source_module("R", "validate.R")
source_module("R", "clean.R")
source_module("R", "derive.R")
source_module("R", "summary.R")

library(testthat)
library(tibble)
library(jsonlite)

ensure_outputs_ready <- function() {
  target <- path_summary("outputs")
  if (file.exists(target)) return(invisible(NULL))
  input_path <- "dpwh_flood_control_projects.csv"
  df_raw <- ingest_csv(input_path)
  validate_schema(df_raw)
  df_clean <- clean_all(df_raw)
  df_plus <- derive_fields(df_clean)
  df_filtered <- filter_years(df_plus, years = 2021:2023)
  sumry <- build_summary(df_filtered)
  ensure_outdir("outputs")
  write_summary_json(sumry, target)
}

ensure_outputs_ready()

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

test_that("summary fields exist and totals are realistic", {
  j <- jsonlite::read_json(path_summary("outputs"))
  expect_true(all(c("total_projects", "total_contractors", "total_provinces", "global_avg_delay", "total_savings") %in% names(j)))
  expect_true(is.null(j$total_savings) || is.finite(j$total_savings))
  if (!is.null(j$total_savings) && is.finite(j$total_savings)) {
    expect_true(abs(j$total_savings) <= 1e13)
  }
  expect_true(is.null(j$global_avg_delay) || is.finite(j$global_avg_delay))
})
