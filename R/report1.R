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

  summary_tbl <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = safe_sum(ApprovedBudgetForContract),
      MedianSavings = safe_median(CostSavings),
      AvgDelay = safe_mean(CompletionDelayDays),
      HighDelayPct = safe_mean(CompletionDelayDays > 30) * 100,
      .groups = "drop"
    ) %>%
    ungroup()

  report <- summary_tbl %>%
    mutate(
      EfficiencyScore = {
        delay_denom <- ifelse(is.na(AvgDelay), NA_real_, pmax(AvgDelay, 1))
        raw_score <- (MedianSavings / delay_denom) * 100
        minmax_0_100(raw_score)
      }
    ) %>%
    select(Region, MainIsland, TotalBudget, MedianSavings, AvgDelay, HighDelayPct, EfficiencyScore) %>%
    arrange(desc(EfficiencyScore))

  format_dataframe(report)
}

report_regional_efficiency <- function(df) {                  # backwards compatibility helper
  build_report1(df)
}
