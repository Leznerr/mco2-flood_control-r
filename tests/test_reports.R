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
    exclude = c("FundingYear", "Year", "N", "NProjects", "NumProjects", "Rank", "TotalProjects"),
    exclude_regex = NULL,
    comma_strings = TRUE,
    digits = 2
  )
  write_report1(r1, "outputs", fmt_opts)
  write_report2(r2, "outputs", fmt_opts)
  write_report3(r3, "outputs", fmt_opts)
  write_summary_outdir(sumry, "outputs")
}

ensure_outputs_ready()

library(readr)

locale_comma <- readr::locale(grouping_mark = ",")

test_that("report 1 schema & sort are exact", {
  r1 <- readr::read_csv(path_report1("outputs"), show_col_types = FALSE, locale = locale_comma)
  expect_identical(
    names(r1),
    c(
      "Region",
      "MainIsland",
      "TotalBudget",
      "MedianSavings",
      "AvgDelay",
      "HighDelayPct",
      "EfficiencyScore"
    )
  )
  expect_true(!is.unsorted(-r1$EfficiencyScore))
})

test_that("report 2 top-15, \u22655 projects, sorted by cost", {
  r2 <- readr::read_csv(path_report2("outputs"), show_col_types = FALSE, locale = locale_comma)
  expect_true(nrow(r2) <= 15)
  expect_true(all(r2$NumProjects >= 5))
  expect_true(!is.unsorted(-r2$TotalCost))
  expect_true(all(r2$ReliabilityIndex >= 0 & r2$ReliabilityIndex <= 100, na.rm = TRUE))
  expected_flag <- ifelse(is.na(r2$ReliabilityIndex) | r2$ReliabilityIndex < 50, "High Risk", "Low Risk")
  expect_identical(r2$RiskFlag, expected_flag)
})

test_that("report 3 YoY vs 2021 within type", {
  r3 <- readr::read_csv(path_report3("outputs"), show_col_types = FALSE, locale = locale_comma)
  expect_identical(names(r3), c("FundingYear", "TypeOfWork", "TotalProjects", "AvgSavings", "OverrunRate", "YoYChange"))
  expect_true(all(r3$FundingYear %in% 2021:2023))
  expect_true(all(is.na(r3$YoYChange[r3$FundingYear == 2021])))

  baseline <- r3[r3$FundingYear == 2021, c("TypeOfWork", "AvgSavings")]
  names(baseline)[2] <- "BaseAvgSavings"
  joined <- merge(r3, baseline, by = "TypeOfWork", all.x = TRUE, sort = FALSE)
  subset <- joined[joined$FundingYear != 2021 & !is.na(joined$BaseAvgSavings) & joined$BaseAvgSavings != 0, ]
  if (nrow(subset) > 0) {
    expected <- ((subset$AvgSavings - subset$BaseAvgSavings) / abs(subset$BaseAvgSavings)) * 100
    expect_equal(subset$YoYChange, expected, tolerance = 1e-6)
  }
})
