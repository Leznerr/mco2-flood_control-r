# utils_cli.R
# ------------------------------------------------------------------------------
# Purpose   : Provide CLI helpers for argument parsing and interactive menu
#             handling. The menu helpers intentionally rely only on base R so
#             that they work consistently across platforms and terminals.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(optparse)
})

build_cli <- function() {
  option_list <- list(
    make_option(c("-i", "--input"), type = "character", metavar = "FILE",
                help = "Path to the DPWH flood-control CSV dataset."),
    make_option(c("-o", "--outdir"), type = "character", default = "outputs",
                metavar = "DIR", help = "Directory for generated reports [default %default]."),
    make_option("--interactive", action = "store_true", default = FALSE,
                help = "Enable interactive preview of report outputs.")
  )
  OptionParser(option_list = option_list, usage = "%prog --input <file> [--outdir <dir>]")
}

`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

validate_cli_args <- function(args) {
  if (is.null(args$input) || is.na(args$input) || !nzchar(args$input)) {
    stop("CLI: --input <file> is required.")
  }
  if (!file.exists(args$input)) {
    stop(sprintf("CLI: input file not found -> %s", args$input))
  }
  outdir <- args$outdir %||% "outputs"
  if (!is.null(outdir) && (is.na(outdir) || !nzchar(outdir))) {
    stop("CLI: --outdir must be a non-empty string when provided.")
  }
  if (file.exists(outdir) && !dir.exists(outdir)) {
    stop(sprintf("CLI: --outdir path exists but is not a directory -> %s", outdir))
  }
  if (!dir.exists(outdir)) {
    ok <- dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop(sprintf("CLI: unable to create --outdir '%s'.", outdir))
    }
  }
  invisible(args)
}

normalize_cli_paths <- function(args) {
  args$input <- normalizePath(args$input, winslash = "/", mustWork = FALSE)
  args$outdir <- normalizePath(args$outdir %||% "outputs", winslash = "/", mustWork = FALSE)
  args
}

.read_cli_line <- function() {
  line <- tryCatch(readLines(con = stdin(), n = 1, warn = FALSE), error = function(...) character(0))
  if (length(line) == 0L) {
    line <- tryCatch(readline(), error = function(...) character(0))
  }
  if (length(line) == 0L) {
    return(NA_character_)
  }
  value <- line[[1L]]
  if (is.na(value)) {
    return(NA_character_)
  }
  value
}

print_menu <- function() {
  cat("Select Language Implementation:\n")
  cat("[1] Load the file\n")
  cat("[2] Generate Reports\n\n")
  cat("Enter choice: ")
  flush.console()
}

read_choice <- function() {
  input <- .read_cli_line()
  if (is.na(input)) {
    return(NA_integer_)
  }
  choice <- trimws(input)
  if (nzchar(choice)) {
    cat(sprintf("Enter choice: %s\n", choice))
    flush.console()
  }
  if (identical(choice, "1") || identical(choice, "2")) {
    return(as.integer(choice))
  }
  NA_integer_
}

prompt_back_to_menu <- function() {
  cat("Back to Report Selection (Y/N): ")
  flush.console()
  input <- .read_cli_line()
  if (is.na(input)) {
    return(FALSE)
  }
  tolower(trimws(input)) == "y"
}
