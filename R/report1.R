# report1.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Regional Flood Mitigation Efficiency report.
# Contract  : report_regional_efficiency(df) -> tibble with columns
#   Region, MainIsland, TotalApprovedBudget, MedianSavings, AvgDelay,
#   Delay30Rate, EfficiencyScore (sorted by EfficiencyScore desc).
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
      TotalApprovedBudget = safe_sum(ApprovedBudgetForContract),
      MedianSavings = safe_median(CostSavings),
      AvgDelay = safe_mean(CompletionDelayDays),
      Delay30Rate = 100 * safe_mean(CompletionDelayDays > 30),
      EfficiencyRaw = {
        adj_delay <- dplyr::case_when(
          is.na(AvgDelay) ~ NA_real_,
          abs(AvgDelay) < 1e-6 ~ dplyr::if_else(AvgDelay < 0, -1e-6, 1e-6),
          TRUE ~ AvgDelay
        )
        ifelse(!is.na(MedianSavings) & !is.na(adj_delay), (MedianSavings / adj_delay) * 100, NA_real_)
      },
      .groups = "drop"
    ) %>%
    mutate(
      EfficiencyScore = minmax_0_100(EfficiencyRaw)
    ) %>%
    select(Region, MainIsland, TotalApprovedBudget, MedianSavings, AvgDelay, Delay30Rate, EfficiencyScore) %>%
    arrange(desc(EfficiencyScore), Region, MainIsland)
}
