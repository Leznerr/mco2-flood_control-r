# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : build_report2(df) -> tibble with columns Rank, Contractor,
#   TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # align with other report helpers
  library(dplyr)
})

build_report2 <- function(df) {                              # build contractor leaderboard
  if (!is.data.frame(df)) stop("build_report2(): 'df' must be a data frame.")

  df %>%
    dplyr::group_by(Contractor) %>%
    dplyr::summarise(
      TotalCost = sum(ContractCost, na.rm = TRUE),
      NumProjects = dplyr::n(),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      TotalSavings = sum(CostSavings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      AvgDelay = dplyr::na_if(AvgDelay, NaN),
      ReliabilityIndex = (1 - (AvgDelay / 90)) * (TotalSavings / pmax(TotalCost, 1)) * 100,
      ReliabilityIndex = pmax(pmin(ReliabilityIndex, 100), 0),
      ReliabilityIndex = dplyr::if_else(
        is.finite(ReliabilityIndex),
        ReliabilityIndex,
        NA_real_
      ),
      RiskFlag = dplyr::if_else(
        ReliabilityIndex < 50,
        "High Risk",
        "Low Risk",
        missing = NA_character_
      )
    ) %>%
    dplyr::filter(NumProjects >= 5) %>%
    dplyr::arrange(dplyr::desc(TotalCost), Contractor) %>%
    dplyr::slice_head(n = 15) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
    dplyr::select(
      Rank,
      Contractor,
      TotalCost,
      NumProjects,
      AvgDelay,
      TotalSavings,
      ReliabilityIndex,
      RiskFlag
    )
}

report_contractor_ranking <- function(df) {                  # backwards-compatible entry point
  build_report2(df)
}
