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
source_module("R", "ingest.R")
source_module("R", "validate.R")
source_module("R", "clean.R")

library(testthat)

test_that("logging never clobbers data frames", {
  df <- ingest_csv("dpwh_flood_control_projects.csv")
  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 0)

  .log_info("class=%s n=%s", paste(class(df), collapse = "/"), nrow(df))
  expect_true(is.data.frame(df))

  validate_schema(df)
  cleaned <- clean_all(df)
  expect_true(is.data.frame(cleaned))
  expect_gt(nrow(cleaned), 0)
})
