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
  dir <- tempfile("fname-test-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  code(dir)
}

test_that("output filenames and headers align to spec", {
  with_tempdir(function(tmp) {
    candidates <- c(
      file.path("..", "sample-data", "tiny_fixture.csv"),
      file.path("sample-data", "tiny_fixture.csv")
    )
    input_path <- candidates[file.exists(candidates)][1]
    expect_true(length(input_path) == 1, info = "Fixture CSV not found")
    raw <- ingest_csv(input_path)
    validate_schema(raw)
    cleaned <- clean_all(raw)
    derived <- derive_fields(cleaned)
    filtered <- filter_years(derived, years = 2021:2023)

    report1 <- report_regional_efficiency(filtered)
    report2 <- report_contractor_ranking(filtered)
    report3 <- report_overrun_trends(filtered)
    summary <- build_summary(filtered)

    outdir <- file.path(tmp, "outputs")
    dir.create(outdir, showWarnings = FALSE)

    paths <- c(
      write_report1(report1, outdir),
      write_report2(report2, outdir),
      write_report3(report3, outdir),
      write_summary_outdir(summary, outdir)
    )

    expect_true(all(file.exists(paths)))
    expect_setequal(basename(paths), unlist(REPORT_FILES))

    header1 <- readLines(file.path(outdir, REPORT_FILES$report1), n = 1L)

  })
})
