# report1.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Regional Flood Mitigation Efficiency report.
# Contract  : report_regional_efficiency(df) -> tibble with columns
#   Region, MainIsland, TotalBudget, MedianSavings, AvgDelay,
#   HighDelayPct, EfficiencyScore (sorted by EfficiencyScore desc).
# Rubric    : Correctness (aggregations & min-max normalisation), Performance
#             (dplyr group summarise), Readability (formal comments), UX (stable
#             schema ordering).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load for CLI/tests
  library(dplyr)
})

report_regional_efficiency <- function(df) {                 # build report 1 summary
  if (!is.data.frame(df)) stop("report_regional_efficiency(): 'df' must be a data frame.")
  df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = safe_sum(ApprovedBudgetForContract),
      MedianSavings = safe_median(CostSavings),
      AvgDelay = safe_mean(CompletionDelayDays),
      HighDelayPct = 100 * safe_mean(CompletionDelayDays > 90),
      RawEfficiency = (MedianSavings / pmax(AvgDelay, 0.5)) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      EfficiencyScore = pmax(0, pmin(100, minmax_0_100(RawEfficiency)))
    ) %>%
    select(Region, MainIsland, TotalBudget, MedianSavings, AvgDelay, HighDelayPct, EfficiencyScore) %>%
    arrange(desc(EfficiencyScore), Region, MainIsland)
}

