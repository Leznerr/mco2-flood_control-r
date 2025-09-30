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
  delays <- as.numeric(df$CompletionDelayDays)
  if (length(delays) == 0L || all(is.na(delays))) {
    global_avg_delay <- NA_real_
  } else {
    global_avg_delay <- round(mean(delays, na.rm = TRUE), 2)
  }

  savings_vec <- as.numeric(df$CostSavings)
  if (length(savings_vec) == 0L || all(is.na(savings_vec))) {
    total_savings <- NA_real_
  } else {
    total_savings <- sum(savings_vec, na.rm = TRUE)
    if (!is.finite(total_savings)) {
      if (exists("log_warn", mode = "function")) {
        log_warn("Summary: total_savings non-finite -> NA (value=%g).", total_savings)
      } else {
        message(sprintf("[WARN] Summary: total_savings non-finite -> NA (value=%g).", total_savings))
      }
      total_savings <- NA_real_
    } else if (abs(total_savings) > 1e13) {
      if (exists("log_warn", mode = "function")) {
        log_warn("Summary: total_savings=%g seems implausible; setting to NA and continuing.", total_savings)
      } else {
        message(sprintf("[WARN] Summary: total_savings=%g seems implausible; setting to NA and continuing.", total_savings))
      }
      total_savings <- NA_real_
    }
  }
  list(
    total_projects = nrow(df),
    total_contractors = dplyr::n_distinct(df$Contractor, na.rm = TRUE),
    total_provinces = dplyr::n_distinct(df$Province, na.rm = TRUE),
    global_avg_delay = global_avg_delay,
    total_savings = total_savings
  )
}

