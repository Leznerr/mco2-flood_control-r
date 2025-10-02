# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : build_report2(df) -> tibble with columns Rank, Contractor,
#   TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag.
# ------------------------------------------------------------------------------

build_report2 <- function(df) {                              # build contractor leaderboard
  if (!is.data.frame(df)) stop("build_report2(): 'df' must be a data frame.")

  summary_tbl <- dplyr::summarise(
    dplyr::group_by(df, Contractor),
    TotalCost = sum(ContractCost, na.rm = TRUE),
    NumProjects = dplyr::n(),
    AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
    TotalSavings = sum(CostSavings, na.rm = TRUE)
  )
  summary_tbl <- dplyr::ungroup(summary_tbl)
  summary_tbl$AvgDelay[is.nan(summary_tbl$AvgDelay)] <- NA_real_

  eligible <- dplyr::filter(summary_tbl, NumProjects >= 5)
  ordered <- dplyr::arrange(eligible, dplyr::desc(TotalCost), Contractor)
  top15 <- dplyr::slice_head(ordered, n = 15)

  reliability_raw <- (1 - (top15$AvgDelay / 90)) * (top15$TotalSavings / pmax(top15$TotalCost, 1)) * 100
  reliability <- pmax(pmin(reliability_raw, 100), 0)
  reliability[!is.finite(reliability_raw)] <- NA_real_

  top15$ReliabilityIndex <- reliability
  top15$RiskFlag <- ifelse(top15$ReliabilityIndex < 50, "High Risk", "Low Risk")
  top15$Rank <- seq_len(nrow(top15))

  dplyr::select(
    top15,
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
