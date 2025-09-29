# validate.R
# ------------------------------------------------------------------------------
# Purpose   : Perform schema and invariant checks on the ingested dataset before
#             heavy transformations occur. Ensures required headers exist and
#             critical columns are well-formed.
# Contract  :
#   - validate_schema(df) stops with informative errors when the dataset is not
#     usable (missing headers, duplicates, empty, FundingYear not numeric-like).
#   - assert_year_filter(df, allowed_years) verifies FundingYear ∈ allowed set.
# Rubric    : Correctness (fail-fast), Simplicity, Readability (formal comments).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # ensures quiet load in tests/CLI
  library(readr)
})

.required_cols <- c(
  "Region", "MainIsland", "Province", "FundingYear", "TypeOfWork",
  "StartDate", "ActualCompletionDate", "ApprovedBudgetForContract",
  "ContractCost", "Contractor", "Latitude", "Longitude"
)

validate_schema <- function(df) {                            # schema guard for ingested table
  if (!is.data.frame(df)) {
    stop("validate_schema(): 'df' must be a data frame from ingest_csv().")
  }
  if (nrow(df) == 0L) {
    stop("validate_schema(): dataset contains zero rows.")
  }
  headers <- names(df)
  dup <- headers[duplicated(headers)]
  if (length(dup) > 0L) {
    stop(sprintf("validate_schema(): duplicated column names: %s.", paste(unique(dup), collapse = ", ")))
  }
  missing_cols <- setdiff(.required_cols, headers)
  if (length(missing_cols) > 0L) {
    stop(sprintf("validate_schema(): missing required columns: %s.", paste(missing_cols, collapse = ", ")))
  }
  fy <- df[["FundingYear"]]
  parsed <- suppressWarnings(readr::parse_number(fy))
  coercible <- is.na(fy) | (!is.na(parsed) & abs(parsed - round(parsed)) < 1e-6)
  if (any(!coercible)) {
    stop("validate_schema(): FundingYear values must be coercible to integers.")
  }
  invisible(TRUE)
}

assert_year_filter <- function(df, allowed_years = 2021:2023) {  # ensure filtered dataset has expected FundingYear
  if (!is.data.frame(df) || is.null(df$FundingYear)) {
    stop("assert_year_filter(): 'df' must have a FundingYear column.")
  }
  allowed_years <- as.integer(allowed_years)
  fy <- unique(df$FundingYear)
  invalid <- fy[is.na(fy) | !(fy %in% allowed_years)]
  if (length(invalid) > 0L) {
    stop(sprintf("assert_year_filter(): unexpected FundingYear values: %s.", paste(sort(invalid), collapse = ", ")))
  }
  invisible(TRUE)
}

