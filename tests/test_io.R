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
source_module("R", "io.R")

library(testthat)
library(tibble)

with_tempdir <- function(code) {
  dir <- tempfile("io-test-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  force(code)
}

test_that("write_report_csv formats numeric columns with commas", {
  with_tempdir({
    df <- tibble(Contractor = "A", NProjects = 5L, TotalCost = 1234567.891)
    path <- file.path(dir, "report.csv")
    write_report_csv(df, path)
    lines <- readLines(path)
    expect_match(lines[2], '"1,234,567.89"')
    expect_match(lines[2], ',5,')
  })
})

test_that("write_report_csv overwrites atomically", {
  with_tempdir({
    df1 <- tibble(Contractor = "A", NProjects = 5L, TotalCost = 1000)
    df2 <- tibble(Contractor = "A", NProjects = 5L, TotalCost = 2000)
    path <- file.path(dir, "report.csv")
    write_report_csv(df1, path)
    first <- readLines(path)
    write_report_csv(df2, path)
    second <- readLines(path)
    expect_false(identical(first, second))
    expect_match(second[2], '"2,000.00"')
  })
})

test_that("write_summary_json writes pretty auto-unboxed scalars", {
  with_tempdir({
    payload <- list(total_projects = 10L, total_savings = 123.45)
    path <- file.path(dir, "summary.json")
    write_summary_json(payload, path)
    lines <- readLines(path)
    expect_match(lines[1], "\\{")
    expect_true(any(grepl('"total_projects"\\s*:\\s*10', lines)))
    expect_true(any(grepl('"total_savings"\\s*:\\s*123.45', lines)))
  })
})

test_that("write_csv_compat works with current readr", {
  source("R/io.R")
  df <- data.frame(a = c(1, 2), b = c('x', 'y'))
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  expect_silent(write_csv_compat(df, file = tmp, na = "", col_names = TRUE, delim = ",", progress = FALSE))
  back <- readr::read_csv(tmp, show_col_types = FALSE)
  expect_equal(nrow(back), 2L)
  expect_true(all(names(back) == c("a","b")))
})
