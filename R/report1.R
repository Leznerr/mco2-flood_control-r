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

needed_helpers <- c("minmax_0_100", "format_dataframe")     # ensure shared helpers are loaded
if (!all(vapply(needed_helpers, exists, logical(1), mode = "function"))) {
  source("R/utils_format.R")
}

suppressPackageStartupMessages({                             # quiet load for CLI/tests
  library(dplyr)
})

build_report1 <- function(df) {                               # build report 1 summary
  if (!is.data.frame(df)) stop("build_report1(): 'df' must be a data frame.")

  na_sum <- function(x) {                                     # helper: NA when all missing
    values <- as.numeric(x)
    if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
  }

  na_mean <- function(x) {                                    # helper: NA-aware mean
    values <- as.numeric(x)
    if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
  }

  na_median <- function(x) {                                  # helper: NA-aware median
    values <- as.numeric(x)
    if (all(is.na(values))) NA_real_ else stats::median(values, na.rm = TRUE)
  }

  summary_tbl <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = na_sum(ApprovedBudgetForContract),
      MedianSavings = na_median(CostSavings),
      AvgDelay = na_mean(CompletionDelayDays),
      HighDelayPct = {
        delays <- as.numeric(CompletionDelayDays)
        if (all(is.na(delays))) {
          NA_real_
        } else {
          mean(delays > 30, na.rm = TRUE) * 100
        }
      }
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
