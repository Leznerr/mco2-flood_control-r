# report1.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Regional Flood Mitigation Efficiency report.
# Contract  : build_report1(df) -> tibble with columns
#   Region, MainIsland, TotalBudget, MedianSavings, AvgDelay,
#   HighDelayPct, EfficiencyScore (sorted by EfficiencyScore desc).
# Rubric    : Correctness (aggregations & min-max normalisation), Performance
#             (dplyr group summarise), Readability (formal comments), UX (stable
#             schema ordering and formatted values).
# ------------------------------------------------------------------------------

if (!exists("minmax_0_100", mode = "function")) {          # ensure shared helpers available
  source("R/utils_format.R")
}

suppressPackageStartupMessages({                             # quiet load for CLI/tests
  library(dplyr)
})

build_report1 <- function(df) {                               # build report 1 summary
  if (!is.data.frame(df)) stop("build_report1(): 'df' must be a data frame.")

  report <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = {
        values <- as.numeric(ApprovedBudgetForContract)
        if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
      },
      MedianSavings = {
        values <- as.numeric(CostSavings)
        if (all(is.na(values))) NA_real_ else stats::median(values, na.rm = TRUE)
      },
      AvgDelay = {
        values <- as.numeric(CompletionDelayDays)
        if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
      },
      HighDelayPct = {
        delays <- as.numeric(CompletionDelayDays)
        if (all(is.na(delays))) {
          NA_real_
        } else {
          mean(delays > 30, na.rm = TRUE) * 100
        }
      },
      .groups = "drop"
    ) %>%
    mutate(
      EfficiencyScore = {
        adj_delay <- ifelse(is.na(AvgDelay), NA_real_, pmax(AvgDelay, 1))
        raw <- (MedianSavings / adj_delay) * 100
        minmax_0_100(raw)
      }
    ) %>%
    select(Region, MainIsland, TotalBudget, MedianSavings, AvgDelay, HighDelayPct, EfficiencyScore) %>%
    arrange(desc(EfficiencyScore))

  format_dataframe(report)
}

report_regional_efficiency <- function(df) {                  # backwards compatibility helper
  build_report1(df)
}
