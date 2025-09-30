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

find_path <- function(...) {
  rel <- file.path(...)
  candidates <- c(file.path("..", rel), rel)
  for (path in candidates) {
    if (file.exists(path)) return(path)
  }
  stop(sprintf("Unable to locate file '%s' from test.", rel))
}

source_module("R", "utils_log.R")
source_module("R", "utils_format.R")
source_module("R", "io.R")
source_module("R", "ingest.R")
source_module("R", "validate.R")
source_module("R", "clean.R")
source_module("R", "derive.R")
source_module("R", "report1.R")
source_module("R", "report2.R")
source_module("R", "report3.R")
source_module("R", "summary.R")

library(testthat)

with_tempdir <- function(code) {
  dir <- tempfile("pipeline-out-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  force(code)
}

test_that("tiny fixture end-to-end pipeline respects invariants", {
  input <- find_path("sample-data", "tiny_fixture.csv")
  df_raw <- ingest_csv(input)
  validate_schema(df_raw)
  df_clean <- clean_all(df_raw)
  df_plus <- derive_fields(df_clean)
  df_filtered <- filter_years(df_plus, 2021:2023)
  expect_silent(assert_year_filter(df_filtered, 2021:2023))
  expect_true(all(df_filtered$FundingYear %in% 2021:2023))

  r1 <- report_regional_efficiency(df_filtered)
  r2 <- report_contractor_ranking(df_filtered)
  r3 <- report_overrun_trends(df_filtered)
  sumry <- build_summary(df_filtered)

  expect_true(all(r1$EfficiencyScore >= 0 & r1$EfficiencyScore <= 100, na.rm = TRUE))
  expect_lte(nrow(r2), 15)
  expect_true(all(r3$FundingYear == sort(r3$FundingYear)))
  expect_true(all(is.na(r3$YoYChange[r3$FundingYear == 2021])))

  with_tempdir({
    outdir <- dir
    f1 <- path_report1(outdir)
    f2 <- path_report2(outdir)
    f3 <- path_report3(outdir)
    fj <- file.path(outdir, "summary.json")

    write_report_csv(r1, f1)
    write_report_csv(r2, f2)
    write_report_csv(r3, f3)
    write_summary_json(sumry, fj)

    expect_true(file.exists(f1))
    expect_true(file.exists(f2))
    expect_true(file.exists(f3))
    expect_true(file.exists(fj))

    lines1 <- readLines(f1)
    expect_match(lines1[2], '"')
    json <- readLines(fj)
    expect_true(any(grepl('"total_projects"', json)))
  })
})
