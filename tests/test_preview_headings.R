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
source_module("R", "interactive_preview.R")
source_module("R", "verify.R")

library(testthat)

sample_reports <- list(
  report1 = data.frame(Region = "R1", MainIsland = "Luzon", TotalBudget = 1, MedianSavings = 0, AvgDelay = 0, HighDelayPct = 0, EfficiencyScore = 50),
  report2 = data.frame(Contractor = "C1", NumProjects = 5, TotalCost = 1, AvgDelay = 0, TotalSavings = 0, ReliabilityIndex = 75, RiskFlag = "Low Risk"),
  report3 = data.frame(FundingYear = 2021L, TypeOfWork = "Work", TotalProjects = 1, AvgSavings = 0, OverrunRate = 0, YoYChange = NA_real_)
)

sample_summary <- list(total_projects = 1, total_contractors = 1, total_provinces = 1, global_avg_delay = 0, total_savings = 0)

sample_fmt <- list(comma_strings = TRUE, digits = 2)

test_that("interactive preview prints the shared headings", {
  output <- capture.output(.run_interactive_spec(sample_reports, sample_summary, sample_fmt, preview_limit = 1))
  observed <- output[output %in% INTERACTIVE_PREVIEW_HEADINGS]
  expect_identical(observed, INTERACTIVE_PREVIEW_HEADINGS)
})

test_that("preview heading guard flags drift when helper output changes", {
  expect_true(.preview_heading_guard(sample_reports, sample_summary, sample_fmt, preview_limit = 1))
  original <- .run_interactive_spec
  on.exit(assign(".run_interactive_spec", original, envir = globalenv()), add = TRUE)
  assign(".run_interactive_spec", function(...) { cat("Unexpected heading\n") }, envir = globalenv())
  expect_false(.preview_heading_guard(sample_reports, sample_summary, sample_fmt, preview_limit = 1))
})
