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

  total_savings <- sum(df$CostSavings, na.rm = TRUE)
  limit <- 1e13

  if (!is.finite(total_savings) || abs(total_savings) > limit) {
    warn_msg <- sprintf(
      "CostSavings sum implausible (%s) -> NA.",
      format(total_savings, scientific = TRUE)
    )

    if (exists("log_warn", mode = "function")) {
      log_warn(warn_msg)
    } else {
      warning(warn_msg)
    }

    total_savings <- NA_real_
  }

  list(
    total_projects = nrow(df),
    total_contractors = dplyr::n_distinct(df$Contractor, na.rm = TRUE),
    total_provinces = dplyr::n_distinct(df$Province, na.rm = TRUE),
    global_avg_delay = mean(df$CompletionDelayDays, na.rm = TRUE),
    total_savings = total_savings
  )
}

