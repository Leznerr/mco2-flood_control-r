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
source_module("R", "report1.R")
source_module("R", "report2.R")
source_module("R", "report3.R")
source_module("R", "summary.R")

library(testthat)

ensure_outputs_ready <- function() {
  files <- c(path_report1("outputs"), path_report2("outputs"), path_report3("outputs"), path_summary("outputs"))
  if (all(file.exists(files))) return(invisible(NULL))
  input_path <- "dpwh_flood_control_projects.csv"
  df_raw <- ingest_csv(input_path)
  validate_schema(df_raw)
  df_clean <- clean_all(df_raw)
  df_plus <- derive_fields(df_clean)
  df_filtered <- filter_years(df_plus, years = 2021:2023)
  r1 <- report_regional_efficiency(df_filtered)
  r2 <- report_contractor_ranking(df_filtered)
  r3 <- report_overrun_trends(df_filtered)
  sumry <- build_summary(df_filtered)
  ensure_outdir("outputs")
  fmt_opts <- list(
    exclude = c("FundingYear", "Year", "N", "NProjects", "NumProjects", "Rank"),
    comma_strings = TRUE,
    digits = 2
  )
  write_report_csv(do.call(format_dataframe, c(list(r1), fmt_opts)), path_report1("outputs"))
  write_report_csv(do.call(format_dataframe, c(list(r2), fmt_opts)), path_report2("outputs"))
  write_report_csv(do.call(format_dataframe, c(list(r3), fmt_opts)), path_report3("outputs"))
  write_summary_json(sumry, path_summary("outputs"))
}

ensure_outputs_ready()

library(readr)

test_that("report filenames and schemas match spec", {
  r1 <- readr::read_csv(path_report1("outputs"), show_col_types = FALSE)
  r2 <- readr::read_csv(path_report2("outputs"), show_col_types = FALSE)
  r3 <- readr::read_csv(path_report3("outputs"), show_col_types = FALSE)

  expect_identical(names(r1), c("Region", "MainIsland", "TotalBudget", "MedianSavings", "AvgDelay", "HighDelayPct", "EfficiencyScore"))
  expect_identical(names(r2), c("Rank", "Contractor", "TotalCost", "NumProjects", "AvgDelay", "TotalSavings", "ReliabilityIndex", "RiskFlag"))
  expect_identical(names(r3), c("FundingYear", "TypeOfWork", "TotalProjects", "AvgSavings", "OverrunRate", "YoYChange"))
})

test_that("report2 constraints and ordering hold", {
  r2 <- readr::read_csv(path_report2("outputs"), show_col_types = FALSE)
  expect_true(nrow(r2) <= 15)
  expect_true(all(r2$NumProjects >= 5))
  expect_true(!is.unsorted(-r2$TotalCost))
})

test_that("report1 score bounds and report3 years", {
  r1 <- readr::read_csv(path_report1("outputs"), show_col_types = FALSE)
  expect_true(all(r1$EfficiencyScore >= 0 & r1$EfficiencyScore <= 100, na.rm = TRUE))

  r3 <- readr::read_csv(path_report3("outputs"), show_col_types = FALSE)
  expect_true(all(r3$FundingYear %in% 2021:2023))
  expect_true(all(is.na(r3$YoYChange[r3$FundingYear == 2021])))
})
