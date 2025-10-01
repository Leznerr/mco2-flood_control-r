# interactive.R
# ------------------------------------------------------------------------------
# Purpose   : Provide the interactive console preview used when the CLI is run
#             with --interactive. The helper prints each report heading followed
#             by a small preview of the corresponding data frame.
# ------------------------------------------------------------------------------

if (!exists("REPORT_PREVIEW_HEADINGS", inherits = TRUE)) {
  if (file.exists("constants.R")) {
    source("constants.R", chdir = TRUE)
  } else if (file.exists(file.path("R", "constants.R"))) {
    source(file.path("R", "constants.R"), chdir = TRUE)
  }
}

.run_interactive_spec <- function(reports, preview_rows = 5L) {
  if (!is.list(reports)) {
    stop(".run_interactive_spec(): 'reports' must be a named list of data frames.")
  }
  expected_keys <- REPORT_PREVIEW_ORDER
  missing <- setdiff(expected_keys, names(reports))
  if (length(missing) > 0L) {
    stop(sprintf(".run_interactive_spec(): missing report(s): %s", paste(missing, collapse = ", ")))
  }

  cat(REPORT_PREVIEW_TITLE, "\n", sep = "")
  cat(REPORT_PREVIEW_RULE, "\n\n", sep = "")

  for (key in expected_keys) {
    heading <- REPORT_PREVIEW_HEADINGS[[key]]
    cat(heading, "\n", sep = "")
    data <- reports[[key]]
    if (is.data.frame(data) && nrow(data) > 0L) {
      utils::print(utils::head(data, preview_rows))
    } else if (is.data.frame(data)) {
      utils::print(data)
    } else {
      cat("[No data available]\n")
    }
    cat("\n")
  }

  invisible(NULL)
}

