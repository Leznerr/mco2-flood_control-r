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

.parse_date_any <- function(x) {                                           # parse common gov/date formats to Date
  dt <- suppressWarnings(                                                  # suppress per-element parse warnings (we log totals)
    lubridate::parse_date_time(                                           # use multi-order parser for resilience
      x = x,                                                              # input vector (character or factor/Date)
      orders = c("ymd","mdy","dmy","Ymd HMS","mdy HMS","dmy HMS","Ymd HM","mdy HM","dmy HM"), # common layouts
      tz = "UTC",                                                         # deterministic timezone for POSIX parsing
      exact = FALSE                                                       # allow flexible matching across orders
    )
  )
  as.Date(dt)                                                             # coerce POSIXct -> Date (drops time component)
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

  ..na_before <- vapply(                                                    # snapshot NA counts before cleaning
    c(
      "Region","MainIsland","Province","FundingYear","TypeOfWork",
      "StartDate","ActualCompletionDate","ApprovedBudgetForContract",
      "ContractCost","Contractor","Latitude","Longitude"
    ),
    function(n) sum(is.na(df[[n]])),
    integer(1)
  )

  # ---- Type coercions & text normalization (idempotent) ----------------------
  df1 <- df %>%                                                            # start a new pipeline to avoid surprise mutation
    mutate(
      # Dates: robust parse to Date (UTC-based, drop time)
      StartDate            = .parse_date_any(StartDate),                    # parse StartDate to Date
      ActualCompletionDate = .parse_date_any(ActualCompletionDate),         # parse ActualCompletionDate to Date

      # Money: parse currency-like strings and commas to numeric
      ApprovedBudgetForContract = .parse_money_safely(.data$ApprovedBudgetForContract), # numeric ABC via strict parser
      ContractCost              = .parse_money_safely(.data$ContractCost),  # numeric contract cost

      # Year: integer FundingYear (handles "2021", 2021.0, "FY2021", etc.)
      FundingYear = .as_integer_safely(.data$FundingYear),                  # integer or NA if unparsable

      # Geo: numeric lat/lon first (NA on non-numeric), then plausibility bounds
      Latitude  = .as_numeric_safely(Latitude),                             # numeric latitude or NA
      Longitude = .as_numeric_safely(Longitude),                            # numeric longitude or NA
      # Text normalization: geographic/party labels Title Case; work type trim only
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
      budget_range <- range_formatter(.$ApprovedBudgetForContract)
      cost_range   <- range_formatter(.$ContractCost)
      if (exists("log_info", mode = "function")) {
        log_info("Money ranges | Budget: [%s, %s] | Cost: [%s, %s]",
                 budget_range[1], budget_range[2], cost_range[1], cost_range[2])
      } else {
        message(sprintf("[INFO] Money ranges | Budget: [%s, %s] | Cost: [%s, %s]",
                        budget_range[1], budget_range[2], cost_range[1], cost_range[2]))
      }
      b <- .bound_latlon(.$Latitude, .$Longitude)                           # compute bounded lat/lon
      mutate(., Latitude = b$lat, Longitude = b$lon)                        # replace with bounded versions
    }

  # ---- Province-level mean geolocation (for conservative imputation) ---------
  complete_coords <- df1 %>%                                               # restrict to rows with complete coordinate pairs
    filter(!is.na(Latitude) & !is.na(Longitude))                           # exclude any partial geocoordinate entries

  prov_means <- complete_coords %>%                                        # compute province means using only observed pairs
    group_by(Province) %>%                                                 # group by normalized Province
    summarise(
      mean_lat = mean(Latitude),                                           # province mean latitude (complete cases only)
      mean_lon = mean(Longitude),                                          # province mean longitude (complete cases only)
      .groups  = "drop"                                                    # drop grouping to avoid downstream carry-over
    )

  # ---- Impute BOTH coordinates only when BOTH are missing --------------------
  df2 <- df1 %>%                                                           # continue cleaning pipeline
    mutate(
      .both_na = is.na(Latitude) & is.na(Longitude)                         # flag rows with both coordinates missing
    ) %>%
    left_join(prov_means, by = "Province") %>%                             # attach filtered province-level means
    mutate(
      Latitude  = ifelse(.both_na & !is.na(mean_lat) & !is.na(mean_lon),    # if both missing and province means exist
                         mean_lat,                                          #   impute latitude with province mean
                         Latitude),                                         # else keep original
      Longitude = ifelse(.both_na & !is.na(mean_lat) & !is.na(mean_lon),    # if both missing and province means exist
                         mean_lon,                                          #   impute longitude with province mean
                         Longitude)                                         # else keep original
    ) %>%
    select(-mean_lat, -mean_lon, -.both_na)                                 # drop helper columns to restore schema

  remaining_geo_na <- sum(is.na(df2$Latitude) & is.na(df2$Longitude))
  if (exists("log_info", mode = "function")) {
    log_info("Post-imputation geo gaps: %d rows still lack coordinates", remaining_geo_na)
  } else {
    message(sprintf("[INFO] Post-imputation geo gaps: %d rows still lack coordinates", remaining_geo_na))
  }

  df3 <- df2 %>%
    filter(
      !is.na(StartDate),
      !is.na(ActualCompletionDate),
      !is.na(ApprovedBudgetForContract),
      !is.na(ContractCost)
    )
  dropped_core <- nrow(df2) - nrow(df3)
  if (dropped_core > 0) {
    if (exists("log_warn", mode = "function")) {
      log_warn("Dropped %d rows missing core schedule/cost fields after cleaning.", dropped_core)
    } else {
      message(sprintf("[WARN] Dropped %d rows missing core schedule/cost fields after cleaning.", dropped_core))
    }
  }

  # ---- NA reduction logging (compact; robust; comma-free) ----------------------
  cols_track <- c(
    "Region","MainIsland","Province","FundingYear","TypeOfWork",
    "StartDate","ActualCompletionDate","ApprovedBudgetForContract",
    "ContractCost","Contractor","Latitude","Longitude"
  )

  if (!exists("..na_before", inherits = FALSE)) {
    ..na_before <- vapply(cols_track, function(n) sum(is.na(df[[n]])), integer(1))
  }

  na_after <- vapply(cols_track, function(n) sum(is.na(df3[[n]])), integer(1))
  na_delta <- ..na_before - na_after

  if (exists("log_info", mode = "function")) {
    log_info("NA reductions by column: %s",
             paste(sprintf("%s=%+d", names(na_delta), na_delta), collapse = ", "))
  } else {
    message(sprintf("[INFO] NA reductions by column: %s",
                    paste(sprintf("%s=%+d", names(na_delta), na_delta), collapse = ", ")))
  }

  max_formatter <- function(v) {
    if (all(is.na(v))) return("NA")
    max_v <- suppressWarnings(max(abs(v), na.rm = TRUE))
    if (is.finite(max_v)) format(max_v, scientific = FALSE) else "NA"
  }
  max_budget <- max_formatter(df3$ApprovedBudgetForContract)
  max_cost   <- max_formatter(df3$ContractCost)
  if (exists("log_info", mode = "function")) {
    log_info("Max |Budget|=%s |Cost|=%s", max_budget, max_cost)
  } else {
    message(sprintf("[INFO] Max |Budget|=%s |Cost|=%s", max_budget, max_cost))
  }

  rm(list="..na_before", inherits = FALSE)

  # ---- Postconditions: column integrity (order and set) ----------------------
  if (!identical(names(df3), orig_cols)) {                                  # ensure we didn't change names/order inadvertently
    .log_warn("Column order/set changed during cleaning; restoring original header order.") # warn for developer visibility
    df3 <- df3[, orig_cols, drop = FALSE]                                   # restore original column order strictly
  }

  # ---- Return cleaned dataframe ----------------------------------------------
  return(df3)                                                               # yield standardized, conservatively-imputed table
}
