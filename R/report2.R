# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : report_contractor_ranking(df) -> tibble with columns
#   Contractor, NProjects, TotalCost, AvgDelay, TotalSavings, ReliabilityIndex,
#   RiskFlag. Pre-filters to contractors with ≥5 projects, keeps top 15 by cost,
#   and sorts by ReliabilityIndex desc then TotalCost desc.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

report_contractor_ranking <- function(df) {                  # build contractor leaderboard
  if (!is.data.frame(df)) stop("report_contractor_ranking(): 'df' must be a data frame.")
  summary <- df %>%
    group_by(Contractor) %>%
    summarise(
      NProjects = dplyr::n(),
      TotalCost = sum(ContractCost, na.rm = TRUE),
      AvgDelay = safe_mean(CompletionDelayDays),
      TotalSavings = sum(CostSavings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(NProjects >= 5) %>%
    arrange(desc(TotalCost), Contractor) %>%
    slice_head(n = 15) %>%
    mutate(
      ReliabilityIndex = pmin(100, (1 - (AvgDelay / 90)) * (TotalSavings / pmax(TotalCost, 1)) * 100),
      ReliabilityIndex = ifelse(is.finite(ReliabilityIndex), ReliabilityIndex, NA_real_),
      RiskFlag = ifelse(!is.na(ReliabilityIndex) & ReliabilityIndex < 50, "High Risk", "OK")
    ) %>%
    arrange(desc(ReliabilityIndex), desc(TotalCost), Contractor)
  select(summary, Contractor, NProjects, TotalCost, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag)
}

