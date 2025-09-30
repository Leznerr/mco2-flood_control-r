test_that("main pipeline does not clobber data frames via logging", {
  source("R/ingest.R")
  source("R/validate.R")
  source("R/clean.R")
  source("R/derive.R")

  raw <- ingest_csv("dpwh_flood_control_projects.csv")
  expect_true(is.data.frame(raw))
  expect_gt(nrow(raw), 0)

  .log_info("class=%s n=%s", paste(class(raw), collapse = "/"), nrow(raw))
  expect_true(is.data.frame(raw))

  validate_schema(raw)
  cleaned  <- clean_all(raw)
  derived  <- derive_fields(cleaned)
  expect_true(is.data.frame(derived))
  expect_gt(nrow(derived), 0)
})
