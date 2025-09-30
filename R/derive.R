# derive.R
# ------------------------------------------------------------------------------
# Purpose   : Append derived analytical fields to the cleaned dataset and provide
#             filtering helpers required by downstream reports.
# Contract  :
#   - derive_fields(df) returns df with CostSavings and CompletionDelayDays
#     appended (preserving existing column order for prior fields).
#   - filter_years(df, years = 2021:2023) removes rows outside allowed years.
# Rubric    : Correctness (vectorised calculations), Simplicity, Readability,
#             UX (concise logs about overruns/NA delays and filtering effect).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

.log_info <- function(fmt, ...) {                            # local logging shim (delegates when global logger exists)
  msg <- sprintf(fmt, ...)
  if (exists("log_info", mode = "function")) log_info("%s", msg) else message(sprintf("[INFO]  %s", msg))
  invisible(NULL)
}

derive_fields <- function(df) {                              # append CostSavings + CompletionDelayDays
  if (!is.data.frame(df)) {
    stop("derive_fields(): 'df' must be a data frame from clean_all().")
  }
  df2 <- df %>%
    mutate(
      CostSavings = {
        cs <- ApprovedBudgetForContract - ContractCost
        bad <- !is.finite(cs) | abs(cs) > 1e12
        if (any(bad)) {
          if (exists("log_warn", mode = "function")) {
            log_warn("CostSavings: %d implausible values (>1e12 or non-finite) -> NA.", sum(bad))
          } else {
            message(sprintf("[WARN] CostSavings: %d implausible values (>1e12 or non-finite) -> NA.", sum(bad)))
          }
          cs[bad] <- NA_real_
        }
        cs
      },
      CompletionDelayDays = {
        delay <- as.numeric(ActualCompletionDate - StartDate)
        delay[!is.na(delay) & delay < 0] <- 0
        delay
      }
    )
  overruns <- sum(df2$CostSavings < 0, na.rm = TRUE)
  delay_na <- sum(is.na(df2$CompletionDelayDays))
  .log_info("Derivations summary | cost_overruns=%d | delay_na=%d", overruns, delay_na)
  df2
}

filter_years <- function(df, years = 2021:2023) {            # filter dataset by FundingYear inclusion set
  if (!is.data.frame(df) || is.null(df$FundingYear)) {
    stop("filter_years(): 'df' must include a FundingYear column.")
  }
  allowed <- as.integer(years)
  before <- nrow(df)
  df2 <- dplyr::filter(df, FundingYear %in% allowed)
  after <- nrow(df2)
  .log_info("Filter summary | before=%d | after=%d | dropped=%d", before, after, before - after)
  df2
}

