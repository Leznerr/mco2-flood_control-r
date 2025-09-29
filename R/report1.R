# report1.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Regional Flood Mitigation Efficiency report.
# Contract  : report_regional_efficiency(df) -> tibble with columns
#   Region, MainIsland, TotalApprovedBudget, MedianSavings, AvgDelay,
#   Delay30Rate, EfficiencyScore (sorted by EfficiencyScore desc, Region, MainIsland).
# Rubric    : Correctness (aggregations & min-max normalisation), Performance
#             (dplyr group summarise), Readability (formal comments), UX (stable
#             schema ordering).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load for CLI/tests
  library(dplyr)
})

report_regional_efficiency <- function(df) {                 # build report 1 summary
  if (!is.data.frame(df)) stop("report_regional_efficiency(): 'df' must be a data frame.")
  summary <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalApprovedBudget = sum(ApprovedBudgetForContract, na.rm = TRUE),
      MedianSavings = safe_median(CostSavings),
      AvgDelay = safe_mean(CompletionDelayDays),
      Delay30Rate = 100 * safe_mean(CompletionDelayDays > 30),
      RawEfficiency = (MedianSavings / pmax(AvgDelay, 0.5)) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      EfficiencyScore = minmax_0_100(RawEfficiency)
    ) %>%
    select(Region, MainIsland, TotalApprovedBudget, MedianSavings, AvgDelay, Delay30Rate, EfficiencyScore) %>%
    arrange(desc(EfficiencyScore), Region, MainIsland)
  summary
}

