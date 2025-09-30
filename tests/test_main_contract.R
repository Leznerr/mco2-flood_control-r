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
source_module("R", "validate.R")
source_module("R", "clean.R")

library(testthat)

csv_path <- if (file.exists("dpwh_flood_control_projects.csv")) {
  "dpwh_flood_control_projects.csv"
} else {
  file.path("..", "dpwh_flood_control_projects.csv")
}

test_that("main pipeline hooks: ingest -> validate -> clean", {
  x <- ingest_csv(csv_path)
  expect_silent(validate_schema(x))
  y <- clean_all(x)
  expect_true(is.data.frame(y))
  expect_gt(nrow(y), 0)
})
