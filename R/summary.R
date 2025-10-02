# summary.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the summary payload for summary.json.
# Contract  : build_summary(df) -> named list with scalar metrics.
# Fields    :
#   - total_projects
#   - total_contractors
#   - total_provinces
#   - global_avg_delay
#   - total_savings
# Rubric    : Simplicity, Correctness (NA handling), Readability.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

build_summary <- function(df) {                              # assemble scalar metrics for JSON
  if (!is.data.frame(df)) stop("build_summary(): 'df' must be a data frame.")

  total_savings_raw <- sum(df$CostSavings, na.rm = TRUE)

  total_savings <- total_savings_raw
  if (!is.finite(total_savings_raw) || abs(total_savings_raw) > 1e13) {
    if (exists("log_warn", mode = "function")) {
      log_warn(
        "CostSavings sum implausible (%s) -> NA.",
        format(total_savings_raw, scientific = TRUE)
      )
    }
    total_savings <- NA_real_
  }

  list(
    total_projects = nrow(df),
    total_contractors = dplyr::n_distinct(df$Contractor),
    total_provinces = dplyr::n_distinct(df$Province),
    global_avg_delay = mean(df$CompletionDelayDays, na.rm = TRUE),
    total_savings = total_savings
  )
}

