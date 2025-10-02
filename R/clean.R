# clean.R 
# ------------------------------------------------------------------------------
# Purpose   : Clean and standardize the ingested DPWH flood-control dataset.
#             - Robust date parsing (multiple formats) to Date.
#             - Coerce money-like fields to numeric (strip "Php", commas, etc.).
#             - Ensure FundingYear is integer.
#             - Ensure Latitude/Longitude are numeric and plausibly bounded.
#             - Normalize key text fields (trim, title-case where sensible).
#             - Conservatively impute geocoordinates ONLY when BOTH are missing,
#               using province-level means (no single-coordinate imputation).
# Contract  : clean_all(df_raw) -> df_clean (same columns, standardized values).
# Notes     : No derived analytics here (see derive.R). No filtering here.
# Rubric    : Correctness (robust parsing, conservative imputation), Readability
#             (formal comments), Simplicity (single responsibility), UX (logs).
# ------------------------------------------------------------------------------

# ----------------------------- Dependencies -----------------------------------
suppressPackageStartupMessages({                                           # suppress package startup banners for clean logs
  library(dplyr)                                                           # verbs for mutate, group_by, summarise, joins
  library(stringr)                                                         # string normalization (trim, case)
  library(lubridate)                                                       # robust date parsing utilities
  library(readr)                                                           # parse_number for flexible numeric parsing
})

# ----------------------------- Logging utilities ------------------------------
# These wrappers integrate with project-wide loggers if present; otherwise they
# fallback to standardized console messages.

.log_info <- function(fmt, ...) {                                          # define informational logger (printf-style)
  msg <- sprintf(fmt, ...)                                                 # format message once
  if (exists("log_info", mode = "function")) log_info("%s", msg)            # delegate to project logger if available
  else message(sprintf("[INFO]  %s", msg))                                 # otherwise print standardized INFO line
  return(invisible(NULL))
}

.log_warn <- function(fmt, ...) {                                          # define warning logger (printf-style)
  msg <- sprintf(fmt, ...)                                                 # format message once
  if (exists("log_warn", mode = "function")) log_warn("%s", msg)            # delegate to project logger if available
  else message(sprintf("[WARN]  %s", msg))                                 # otherwise print standardized WARN line
  return(invisible(NULL))
}

# ----------------------------- Helper functions -------------------------------

.parse_date_any <- function(x) {                                           # parse ISO first, then fallback to %m/%d/%Y
  if (inherits(x, "Date")) {                                              # already a Date object -> return as-is
    return(x)
  }
  tokens_na <- c("", "NA", "N/A", "null", "NULL")                     # tokens treated as missing
  chr <- trimws(as.character(x))                                           # normalize whitespace around values
  chr[chr %in% tokens_na] <- NA_character_                                 # set NA tokens explicitly to NA
  primary <- suppressWarnings(lubridate::ymd(chr))                         # ISO-8601 first pass (YYYY-MM-DD)
  need_fallback <- is.na(primary) & !is.na(chr)                            # identify entries needing fallback parse
  if (any(need_fallback)) {                                                # attempt %m/%d/%Y fallback where needed
    fallback <- suppressWarnings(lubridate::mdy(chr[need_fallback]))
    primary[need_fallback] <- fallback
  }
  primary                                                                  # return Date vector (lubridate::ymd yields Date)
}

# ---- Strict money parser (Php text -> numeric pesos) -------------------------
.parse_money_safely <- function(x) {
  chr <- trimws(as.character(x))
  chr <- gsub("(?i)php\\s*", "", chr, perl = TRUE)
  num <- readr::parse_number(
    chr,
    na = c("", "NA", "N/A", "null", "NULL"),
    locale = readr::locale(grouping_mark = ",", decimal_mark = ".")
  )
  bx <- grepl("(?i)\\bB\\b", chr)
  mx <- grepl("(?i)\\bM\\b", chr)
  num[bx] <- num[bx] * 1e9
  num[mx] <- num[mx] * 1e6
  bad <- (!is.finite(num)) | (abs(num) > 1e12)
  bad_idx <- which(bad %in% TRUE)
  if (length(bad_idx) > 0L) {
    if (exists("log_warn", mode = "function")) {
      log_warn("Money parse: %d implausible -> NA", length(bad_idx))
    } else {
      message(sprintf("[WARN] Money parse: %d implausible -> NA", length(bad_idx)))
    }
    num[bad_idx] <- NA_real_
  }
  num
}

# ---- Safe integer coercion that accepts numeric/character/factor -------------
.as_integer_safely <- function(x) {
  if (is.integer(x)) return(x)
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA_real_
    return(as.integer(round(x)))
  }
  if (is.factor(x)) return(.as_integer_safely(as.character(x)))
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  na_tokens <- c("", "NA", "N/A", "null", "NULL")
  x_chr[x_chr %in% na_tokens] <- NA_character_
  readr::parse_integer(x_chr, na = na_tokens, trim_ws = TRUE, locale = readr::locale())
}

.as_numeric_safely <- function(x) {                                        # coerce to numeric, keep NA on failure
  suppressWarnings(as.numeric(x))                                          # base numeric conversion (robust and fast)
}

.title_case_squish <- function(x) {                                        # normalize whitespace and apply title case
  stringr::str_to_title(stringr::str_squish(x))                            # compress internal spaces; Title Case
}

.squish_only <- function(x) {                                              # normalize whitespace without changing case
  stringr::str_squish(x)                                                   # trim edges and compress internal spaces
}

.bound_latlon <- function(lat, lon) {                                      # bound-check geocoordinates for plausibility
  lat_ok <- ifelse(is.na(lat), NA_real_, ifelse(lat < -90 | lat > 90, NA_real_, lat))  # invalid latitudes -> NA
  lon_ok <- ifelse(is.na(lon), NA_real_, ifelse(lon < -180 | lon > 180, NA_real_, lon))# invalid longitudes -> NA
  list(lat = lat_ok, lon = lon_ok)                                         # return list (used inside mutate)
}

# ----------------------------- Public API -------------------------------------

#' Clean and standardize the ingested dataset.
#'
#' @param df data.frame/tibble from ingest_csv() and validate_schema().
#' @return data.frame/tibble with standardized types, normalized text, and
#'         conservative province-mean geolocation imputation.
clean_all <- function(df) {                                                # define main cleaning function
  # ---- Preconditions ----------------------------------------------------------
  if (missing(df) || !is.data.frame(df)) {                                  # basic contract validation
    stop("clean_all(): 'df' must be a data.frame/tibble.")                  # fail fast on incorrect inputs
  }

  # ---- Canonicalize coordinate column names (accept synonyms) ----------------
  # Keep this block ahead of type coercions so downstream NA tracking sees the
  # canonical Latitude/Longitude headers regardless of input synonyms.
  # If Latitude/Longitude are absent but ProjectLatitude/ProjectLongitude exist,
  # rename the latter to the canonical names so all downstream steps use one pair.
  if (!('Latitude' %in% names(df)) && all(c('ProjectLatitude','ProjectLongitude') %in% names(df))) {
    if (all(c('Latitude','Longitude') %in% names(df))) {
      if (exists('log_warn', mode = 'function')) {
        log_warn('Both coordinate pairs present; using canonical Latitude/Longitude; ignoring ProjectLatitude/ProjectLongitude.')
      }
    } else {
      names(df)[match('ProjectLatitude', names(df))]  <- 'Latitude'
      names(df)[match('ProjectLongitude', names(df))] <- 'Longitude'
    }
  }

  orig_cols <- names(df)                                                    # capture canonicalized column order for restoration
  na_tokens <- c("", "NA", "N/A", "null", "NULL")                       # shared NA tokens for parsing checks
  money_cols <- c("ApprovedBudgetForContract", "ContractCost")            # columns requiring currency parsing
  money_inputs <- lapply(money_cols, function(col) {                        # cache normalized originals for anomaly counts
    vals <- if (col %in% names(df)) df[[col]] else rep(NA, nrow(df))
    chr <- trimws(as.character(vals))
    chr[chr %in% na_tokens] <- NA_character_
    chr
  })
  names(money_inputs) <- money_cols

  # ---- Type coercions & text normalization (idempotent) ----------------------
  df1 <- df %>%                                                            # start a new pipeline to avoid surprise mutation
    mutate(
      across(where(is.character), stringr::str_squish),                    # trim whitespace and collapse runs for all text
      StartDate            = .parse_date_any(StartDate),                    # parse StartDate to Date (ISO + m/d/Y fallback)
      ActualCompletionDate = .parse_date_any(ActualCompletionDate),         # parse ActualCompletionDate similarly
      ApprovedBudgetForContract = .parse_money_safely(.data$ApprovedBudgetForContract), # numeric ABC via strict parser
      ContractCost              = .parse_money_safely(.data$ContractCost),  # numeric contract cost
      FundingYear = .as_integer_safely(.data$FundingYear),                  # integer or NA if unparsable
      Latitude  = .as_numeric_safely(Latitude),                             # numeric latitude or NA
      Longitude = .as_numeric_safely(Longitude),                            # numeric longitude or NA
      Region     = .title_case_squish(Region),                              # normalize Region naming
      MainIsland = .title_case_squish(MainIsland),                          # normalize MainIsland
      Province   = .title_case_squish(Province),                            # normalize Province naming
      Contractor = .title_case_squish(Contractor),                          # normalize Contractor naming
      TypeOfWork = .squish_only(TypeOfWork)                                 # preserve original case for TypeOfWork
    ) %>%
    {
      range_formatter <- function(v) {
        if (all(is.na(v))) return(c("NA", "NA"))
        min_v <- suppressWarnings(min(v, na.rm = TRUE))
        max_v <- suppressWarnings(max(v, na.rm = TRUE))
        min_str <- if (is.finite(min_v)) format(min_v, scientific = FALSE) else "NA"
        max_str <- if (is.finite(max_v)) format(max_v, scientific = FALSE) else "NA"
        c(min_str, max_str)
      }
      anomalies <- vapply(names(money_inputs), function(col) {
        inputs <- money_inputs[[col]]
        observed <- !is.na(inputs)
        sum(is.na(.[[col]]) & observed)
      }, integer(1))
      budget_range <- range_formatter(.$ApprovedBudgetForContract)
      cost_range   <- range_formatter(.$ContractCost)
      .log_info(
        "Money parse anomalies: %s",
        paste(sprintf("%s=%d", names(anomalies), anomalies), collapse = ", ")
      )
      .log_info(
        "Money ranges | Budget: [%s, %s] | Cost: [%s, %s]",
        budget_range[1], budget_range[2], cost_range[1], cost_range[2]
      )
      b <- .bound_latlon(.$Latitude, .$Longitude)                           # compute bounded lat/lon
      mutate(., Latitude = b$lat, Longitude = b$lon)                        # replace with bounded versions
    }

  # ---- Province-level mean geolocation (for conservative imputation) ---------
  prov_means <- df1 %>%                                                    # compute province means using available coordinates
    filter(!is.na(Province)) %>%
    group_by(Province) %>%
    summarise(
      mean_lat = if (all(is.na(Latitude))) NA_real_ else mean(Latitude, na.rm = TRUE),
      mean_lon = if (all(is.na(Longitude))) NA_real_ else mean(Longitude, na.rm = TRUE),
      .groups  = "drop"
    )

  # ---- Impute coordinates when missing, using province means ----------------
  df2 <- df1 %>%                                                           # continue cleaning pipeline
    left_join(prov_means, by = "Province") %>%                             # attach province-level means
    mutate(
      Latitude  = ifelse(is.na(Latitude)  & !is.na(mean_lat), mean_lat, Latitude),
      Longitude = ifelse(is.na(Longitude) & !is.na(mean_lon), mean_lon, Longitude)
    ) %>%
    select(-mean_lat, -mean_lon)                                           # drop helper columns to restore schema

  remaining_geo_na <- sum(is.na(df2$Latitude) | is.na(df2$Longitude))
  .log_info("Post-imputation geo gaps (any missing coordinate): %d", remaining_geo_na)

  df3 <- df2 %>%
    filter(
      !is.na(StartDate),
      !is.na(ActualCompletionDate),
      !is.na(ApprovedBudgetForContract),
      !is.na(ContractCost)
    )
  dropped_core <- nrow(df2) - nrow(df3)
  .log_info("Rows dropped after core field enforcement: %d", dropped_core)

  # ---- Postconditions: column integrity (order and set) ----------------------
  if (!identical(names(df3), orig_cols)) {                                  # ensure we didn't change names/order inadvertently
    .log_warn("Column order/set changed during cleaning; restoring original header order.") # warn for developer visibility
    df3 <- df3[, orig_cols, drop = FALSE]                                   # restore original column order strictly
  }

  # ---- Return cleaned dataframe ----------------------------------------------
  return(df3)                                                               # yield standardized, conservatively-imputed table
}

#' Filter a dataframe to a specific window of funding years.
#'
#' @param df data.frame/tibble containing a FundingYear column.
#' @param years integer/numeric vector of years to keep (defaults to 2021:2023).
#' @return subset of `df` restricted to the specified FundingYear values.
filter_window <- function(df, years = 2021:2023) {
  if (!is.data.frame(df)) {
    stop("filter_window(): 'df' must be a data.frame/tibble.")
  }
  if (!("FundingYear" %in% names(df))) {
    stop("filter_window(): 'df' must contain a 'FundingYear' column.")
  }
  if (!(is.numeric(years) || is.integer(years))) {
    stop("filter_window(): 'years' must be numeric or integer.")
  }

  years <- unique(as.integer(years))
  years <- years[!is.na(years)]

  if (length(years) == 0) {
    return(df[0, , drop = FALSE])
  }

  df %>%
    dplyr::filter(.data$FundingYear %in% years)
}
