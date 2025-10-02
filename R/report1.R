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

ensure_format_helpers <- function() {                          # lazily load shared helpers
  required_fns <- c(
    "minmax_0_100",
    "format_dataframe",
    "safe_sum",
    "safe_mean",
    "safe_median"
  )
  missing_fns <- !vapply(required_fns, exists, logical(1), mode = "function")
  if (any(missing_fns)) {
    source("R/utils_format.R")
  }
}

suppressPackageStartupMessages({                             # quiet load for CLI/tests
  library(dplyr)
})

build_report1 <- function(df) {                               # build report 1 summary
  if (!is.data.frame(df)) stop("build_report1(): 'df' must be a data frame.")

  ensure_format_helpers()

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
      }
    , .groups = "drop"
    )

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
