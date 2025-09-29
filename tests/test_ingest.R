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

source_module("R", "ingest.R")

library(testthat)

test_that("ingest requires a path", {
  expect_error(ingest_csv(), "missing")
  expect_error(ingest_csv("nonexistent.csv"), "file not found")
})

test_that("ingest rejects zero-row files", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("Region\n", tmp)
  expect_error(ingest_csv(tmp), "zero data rows")
})

test_that("ingest rejects truly empty files", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  file.create(tmp)
  expect_error(ingest_csv(tmp), "empty or unreadable")
})

test_that("ingest detects duplicate headers", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("Region,Region\nNCR,NCR", tmp)
  expect_error(ingest_csv(tmp), "duplicated column names")
})

test_that("ingest attaches parse problems attribute", {
  df <- ingest_csv(file.path("sample-data", "tiny_fixture.csv"))
  problems <- attr(df, "ingest_problems")
  expect_s3_class(problems, "tbl_df")
  expect_true(nrow(problems) >= 0)
})

