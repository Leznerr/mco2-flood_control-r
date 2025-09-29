# validate.R
# ------------------------------------------------------------------------------
# Purpose : Schema/type/invariant checks for the DPWH flood-control CSV.
# Contract: validate_schema(df) -> invisible(TRUE) or stop() on violations
#           assert_year_filter(df, allowed_years=2021:2023) -> invisible(TRUE)
# Notes   : Accepts either {Latitude,Longitude} OR {ProjectLatitude,ProjectLongitude}.
# ------------------------------------------------------------------------------

.has_pair <- function(nms, pairs) {
  any(apply(pairs, 1L, function(p) all(p %in% nms)))
}

validate_schema <- function(df) {
  if (missing(df) || !is.data.frame(df)) {
    stop("validate_schema(): 'df' must be a data.frame/tibble from ingest_csv().")
  }
  nms <- names(df)
  if (length(nms) == 0L) stop("validate_schema(): dataframe has zero columns.")
  if (nrow(df) == 0L)   stop("validate_schema(): dataframe has zero rows (no data).")

  dups <- nms[duplicated(nms)]
  if (length(dups) > 0L) {
    stop(sprintf("validate_schema(): duplicated column names: %s.", paste(sort(unique(dups)), collapse = ', ')))
  }

  required_strict <- c(
    "Region","MainIsland","Province","FundingYear","TypeOfWork",
    "StartDate","ActualCompletionDate","ApprovedBudgetForContract",
    "ContractCost","Contractor"
  )
  missing_strict <- setdiff(required_strict, nms)
  if (length(missing_strict) > 0L) {
    stop(sprintf("validate_schema(): missing required columns: %s.", paste(missing_strict, collapse = ", ")))
  }

  latlon_pairs <- rbind(
    c("Latitude","Longitude"),
    c("ProjectLatitude","ProjectLongitude")
  )
  if (!.has_pair(nms, latlon_pairs)) {
    stop("validate_schema(): missing coordinates; expected either {Latitude,Longitude} or {ProjectLatitude,ProjectLongitude}.")
  }

  invisible(TRUE)
}

assert_year_filter <- function(df, allowed_years = 2021:2023) {
  if (missing(df) || !is.data.frame(df)) {
    stop("assert_year_filter(): 'df' must be a data.frame/tibble.")
  }
  if (!"FundingYear" %in% names(df)) {
    stop("assert_year_filter(): 'FundingYear' column is missing.")
  }
  vals <- unique(stats::na.omit(df$FundingYear))
  bad  <- setdiff(vals, allowed_years)
  if (length(bad) > 0L) {
    stop(sprintf("assert_year_filter(): found disallowed FundingYear values: %s; allowed: %s.",
                 paste(sort(bad), collapse = ", "), paste(allowed_years, collapse = ", ")))
  }
  invisible(TRUE)
}
