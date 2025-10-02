# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : build_report2(df) -> tibble with columns Rank, Contractor,
#   TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

build_report2 <- function(df) {                              # build contractor leaderboard
  if (!is.data.frame(df)) stop("build_report2(): 'df' must be a data frame.")

  df %>%
    group_by(Contractor) %>%
    summarise(
      TotalCost = sum(ContractCost, na.rm = TRUE),
      NumProjects = dplyr::n(),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      TotalSavings = sum(CostSavings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(AvgDelay = ifelse(is.nan(AvgDelay), NA_real_, AvgDelay)) %>%
    filter(NumProjects >= 5) %>%
    arrange(desc(TotalCost), Contractor) %>%
    slice_head(n = 15) %>%
    mutate(
      ReliabilityIndex = {
        ri <- (1 - (AvgDelay / 90)) * (TotalSavings / pmax(TotalCost, 1)) * 100
        ri <- pmax(ri, 0)
        ri <- pmin(ri, 100)
        ri
      },
      RiskFlag = ifelse(ReliabilityIndex < 50, "High Risk", "Low Risk"),
      Rank = dplyr::row_number()
    ) %>%
    select(Rank, Contractor, TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag)
}
