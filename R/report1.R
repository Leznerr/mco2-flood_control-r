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

required_helpers <- c(
  "minmax_0_100",
  "format_dataframe",
  "safe_sum",
  "safe_mean",
  "safe_median"
)

if (!all(vapply(required_helpers, exists, logical(1), mode = "function"))) {
  source("R/utils_format.R")
}

suppressPackageStartupMessages({
  library(dplyr)
})

build_report1 <- function(df) {
  if (!is.data.frame(df)) {
    stop("build_report1(): 'df' must be a data frame.")
  }

  summary_tbl <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = safe_sum(ApprovedBudgetForContract),
      MedianSavings = safe_median(CostSavings),
      AvgDelay = safe_mean(CompletionDelayDays),
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
        delay_denom <- ifelse(is.na(AvgDelay), NA_real_, pmax(AvgDelay, 1))
        raw_score <- (MedianSavings / delay_denom) * 100
        minmax_0_100(raw_score)
      }
    ) %>%
    arrange(desc(EfficiencyScore)) %>%
    select(
      Region,
      MainIsland,
      TotalBudget,
      MedianSavings,
      AvgDelay,
      HighDelayPct,
      EfficiencyScore
    )

  format_dataframe(summary_tbl)
}

report_regional_efficiency <- function(df) {
  build_report1(df)
}
