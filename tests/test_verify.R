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

source_module("R", "constants.R")
source_module("R", "verify.R")

library(testthat)
library(tibble)
library(readr)
library(jsonlite)

test_that("verification report includes rubric mapping section", {
  outdir <- tempfile("verify-out-")
  dir.create(outdir)
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)

  dataset <- tibble(
    FundingYear = c(2021L, 2022L),
    ApprovedBudgetForContract = c(200, 300),
    ContractCost = c(180, 250),
    CostSavings = c(20, 50),
    ActualCompletionDate = as.Date(c("2021-02-01", "2022-03-05")),
    StartDate = as.Date(c("2021-01-01", "2022-02-01")),
    CompletionDelayDays = as.numeric(c(31, 32))
  )

  report1 <- tibble(
    Region = c("Region A", "Region B"),
    MainIsland = c("Island 1", "Island 2"),
    TotalApprovedBudget = c(200, 300),
    MedianSavings = c(20, 50),
    AvgDelay = c(31, 32),
    Delay30Rate = c(0, 100),
    EfficiencyScore = c(80, 70)
  )

  report2 <- tibble(
    Contractor = "Firm A",
    NumProjects = 5L,
    TotalCost = 500,
    AvgDelay = 10,
    TotalSavings = 120,
    ReliabilityIndex = 75,
    RiskFlag = "Low Risk"
  )

  report3 <- tibble(
    FundingYear = c(2021L, 2022L),
    TypeOfWork = c("Dredging", "Dredging"),
    TotalProjects = c(4L, 6L),
    AvgSavings = c(100, 120),
    OverrunRate = c(10, 15),
    YoYChange = c(NA_real_, ((120 - 100) / abs(100)) * 100)
  )

  summary_list <- list(
    total_projects = length(dataset$FundingYear),
    total_contractors = length(unique(report2$Contractor)),
    total_provinces = 1,
    global_avg_delay = mean(dataset$CompletionDelayDays),
    total_savings = sum(dataset$CostSavings)
  )

  fmt_opts <- list(comma_strings = TRUE, digits = 2)

  path1_local <- file.path(outdir, REPORT_FILES$r1)
  path2_local <- file.path(outdir, REPORT_FILES$r2)
  path3_local <- file.path(outdir, REPORT_FILES$r3)
  path_summary_local <- file.path(outdir, REPORT_FILES$summary)

  readr::write_csv(report1, path1_local, na = "")
  readr::write_csv(report2, path2_local, na = "")
  readr::write_csv(report3, path3_local, na = "")
  jsonlite::write_json(summary_list, path_summary_local, auto_unbox = TRUE, pretty = TRUE)

  globals <- c("expected_r1", "path1", "path2", "path3", "path_summary_json", "overrun_ok", "preview_ok")
  old_globals <- mget(globals, envir = globalenv(), ifnotfound = as.list(rep(list(NULL), length(globals))))
  on.exit({
    for (nm in globals) {
      previous <- old_globals[[nm]]
      if (is.null(previous)) {
        if (exists(nm, envir = globalenv(), inherits = FALSE)) rm(list = nm, envir = globalenv())
      } else {
        assign(nm, previous, envir = globalenv())
      }
    }
  }, add = TRUE)

  assign("expected_r1", names(report1), envir = globalenv())
  assign("path1", path1_local, envir = globalenv())
  assign("path2", path2_local, envir = globalenv())
  assign("path3", path3_local, envir = globalenv())
  assign("path_summary_json", path_summary_local, envir = globalenv())
  assign("overrun_ok", TRUE, envir = globalenv())
  assign("preview_ok", TRUE, envir = globalenv())

  reports <- list(report1 = report1, report2 = report2, report3 = report3)

  verification_path <- verify_outputs(dataset, reports, summary_list, outdir, fmt_opts)

  lines <- readLines(verification_path)
  rubric_index <- which(lines == "Rubric Mapping")
  expect_length(rubric_index, 1)
  rubric_block <- lines[(rubric_index + 1):length(lines)]
  rubric_entries <- rubric_block[rubric_block != ""]
  expect_gte(length(rubric_entries), 5)
  expect_true(any(grepl("\\[PASS\\].*Simplicity", rubric_entries)))
  expect_true(any(grepl("\\[PASS\\].*Performance", rubric_entries)))
  expect_true(any(grepl("\\[PASS\\].*Readability", rubric_entries)))
  expect_true(any(grepl("\\[PASS\\].*Correctness", rubric_entries)))
  expect_true(any(grepl("\\[PASS\\].*(User Experience|UX)", rubric_entries)))
  expect_equal(sum(grepl("^\u002d \\[PASS\\]", rubric_entries)), 5)
})

