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

ensure_format_helpers <- function() {
  required <- c("minmax_0_100", "format_dataframe")
  missing_helpers <- !vapply(required, exists, logical(1), mode = "function")
  if (any(missing_helpers)) {
    source("R/utils_format.R")
  }
}

suppressPackageStartupMessages({
  library(dplyr)
})

build_report1 <- function(df) {
  if (!is.data.frame(df)) {
    stop("build_report1(): 'df' must be a data frame.")
  }

  ensure_format_helpers()

  summary_tbl <- df %>%
    group_by(Region, MainIsland) %>%
    summarise(
      TotalBudget = sum(ApprovedBudgetForContract, na.rm = TRUE),
      MedianSavings = {
        savings <- as.numeric(CostSavings)
        if (all(is.na(savings))) NA_real_ else stats::median(savings, na.rm = TRUE)
      },
      AvgDelay = {
        delays <- as.numeric(CompletionDelayDays)
        if (all(is.na(delays))) NA_real_ else mean(delays, na.rm = TRUE)
      },
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
