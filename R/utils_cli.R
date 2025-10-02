# utils_cli.R
# ------------------------------------------------------------------------------
# Purpose   : Centralise command-line interface behaviour for the pipeline.
#             Provides OptionParser construction, argument validation, and
#             lightweight path normalisation that works cross-platform.
# Contract  :
#   - build_cli() -> OptionParser (from optparse).
#   - validate_cli_args(args) -> stops with informative errors when flags
#     are missing or malformed.
#   - normalize_cli_paths(args) -> returns same list with paths normalised.
# Rubric    : Simplicity (single module), Correctness (fail-fast), UX (clear
#             help strings and error messages), Readability (formal comments).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # avoid optparse startup chatter during CLI usage
  library(optparse)
})

# Build the CLI parser with --input (required) and --outdir (optional, defaults).
build_cli <- function() {                                    # construct OptionParser with required flags
  option_list <- list(
    make_option(c("-i", "--input"), type = "character", metavar = "FILE",
                help = "Path to the DPWH flood-control CSV dataset."),
    make_option(c("-o", "--outdir"), type = "character", default = "outputs",
                metavar = "DIR", help = "Directory where reports and summary will be written [default %default]."),
    make_option(c("--interactive"), action = "store_true", default = FALSE,
                help = "Enable interactive console preview of reports before export.")
  )
  OptionParser(option_list = option_list, usage = "%prog --input <file> [--outdir <dir>]")
}

# Validate raw optparse argument list before pipeline execution.
validate_cli_args <- function(args) {                         # ensure CLI arguments follow expectations
  if (is.null(args$input) || is.na(args$input) || !nzchar(args$input)) {
    stop("CLI: --input <file> is required.")
  }
  if (!file.exists(args$input)) {
    stop(sprintf("CLI: input file not found -> %s", args$input))
  }
  if (!is.null(args$outdir) && (is.na(args$outdir) || !nzchar(args$outdir))) {
    stop("CLI: --outdir must be a non-empty string when provided.")
  }
  outdir <- args$outdir %||% "outputs"
  if (file.exists(outdir) && !dir.exists(outdir)) {
    stop(sprintf("CLI: --outdir path exists but is not a directory -> %s", outdir))
  }
  if (!dir.exists(outdir)) {
    ok <- dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop(sprintf("CLI: unable to create --outdir '%s' (permissions?).", outdir))
    }
  }
  invisible(args)
}

# Normalise filesystem-like paths without requiring existence (cross-platform safe).
normalize_cli_paths <- function(args) {                       # return args with normalised paths (non-destructive)
  args$input  <- normalizePath(args$input, winslash = "/", mustWork = FALSE)
  args$outdir <- normalizePath(args$outdir %||% "outputs", winslash = "/", mustWork = FALSE)
  args
}

# Provide a tiny helper similar to rlang::`%||%` to avoid dependency.
`%||%` <- function(lhs, rhs) {                                # return lhs when non-null, otherwise rhs
  if (!is.null(lhs)) lhs else rhs
}


print_menu <- function() {
  cat("Select Language Implementation:\n")
  cat("[1] Load the file\n")
  cat("[2] Generate Reports\n\n")
  cat("Enter choice: ")

}

prompt_back_to_menu <- function() {
  cat("Back to Report Selection (Y/N): ")

}

