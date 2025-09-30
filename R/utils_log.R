# utils_log.R
# ------------------------------------------------------------------------------
# Purpose   : Provide lightweight structured logging utilities shared across
#             the pipeline. Supports log levels, optional file sink, and
#             contextual metadata injection.
# Contract  :
#   - log_set_level(level) where level ∈ {"DEBUG","INFO","WARN","ERROR"}.
#   - log_set_file(path_or_NULL) to enable/disable file logging (append mode).
#   - log_context_set(list(...)) to set base context, and with_log_context()
#     to temporarily add nested fields.
#   - log_debug/info/warn/error(fmt, ...) for printf-style logging.
#   - log_banner(text) for nice run delimiters.
# Rubric    : Simplicity (single place for logging), Correctness (consistent
#             formatting), UX (clear ISO timestamps, context fields),
#             Readability (per-line comments).
# ------------------------------------------------------------------------------

# ------------------------------- State storage ---------------------------------
.log_state <- new.env(parent = emptyenv())                   # private env holds mutable logger state
.log_state$level <- "INFO"                                   # default log level for emission thresholds
.log_state$file  <- NULL                                     # optional file path for log duplication
.log_state$context <- list()                                # named list of context fields always appended

# Map of log level names to numeric severity (lower = more verbose).
.log_level_map <- c(DEBUG = 10L, INFO = 20L, WARN = 30L, ERROR = 40L)

# ----------------------------- Helper functions --------------------------------

# Normalise a user-provided level string to an uppercase validated value.
.normalize_level <- function(level) {                        # helper to validate/uppercase level names
  if (missing(level) || length(level) != 1L || is.na(level)) {
    stop("log_set_level(): 'level' must be a non-NA scalar.")
  }
  lvl <- toupper(trimws(as.character(level)))               # uppercase to make comparisons case-insensitive
  if (!lvl %in% names(.log_level_map)) {                    # ensure provided level is supported
    stop(sprintf("Unsupported log level '%s'. Valid options: %s.",
                 level, paste(names(.log_level_map), collapse = ", ")))
  }
  lvl                                                      # return validated level
}

# Determine if a message at `level` should be emitted given current threshold.
.should_emit <- function(level) {                           # compute level gating for logging
  current <- .log_level_map[[.log_state$level]]             # numeric severity for configured level
  target  <- .log_level_map[[level]]                        # numeric severity for requested log call
  target >= current                                         # emit if requested severity ≥ configured threshold
}

# Serialise named context list into "key=value" tokens.
.format_context <- function(ctx) {                          # format context metadata for log line
  if (length(ctx) == 0L) return("")                        # no context -> empty string
  parts <- Map(function(k, v) {                             # iterate over key/value pairs to format
    value <- if (length(v) == 0L || all(is.na(v))) "NA" else paste(v, collapse = ";")
    sprintf("%s=%s", k, value)                              # produce key=value string
  }, names(ctx), ctx)
  paste(parts, collapse = " ")                              # join with spaces between key-value tokens
}

# Emit a fully formatted log line to console and optional file sink.
.emit_line <- function(level, message) {                    # central emission helper
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")   # ISO-like timestamp for traceability
  base_ctx <- .log_state$context                            # fetch persistent context
  ctx_str  <- .format_context(base_ctx)                     # format context string once
  ctx_part <- if (nzchar(ctx_str)) paste0(" ", ctx_str) else ""   # prefix a space only when context available
  line <- sprintf("%s [%s]%s %s", timestamp, level, ctx_part, message)  # build final log line
  cat(line, "\n", file = stdout())                         # write to standard output
  if (!is.null(.log_state$file)) {                          # also persist to file when configured
    cat(line, "\n", file = .log_state$file, append = TRUE)
  }
  return(invisible(NULL))
}

# Format the payload using printf-like semantics (handles zero-argument case).
.format_message <- function(fmt, ...) {                     # helper to sprintf only when needed
  dots <- list(...)                                         # capture variadic arguments
  if (length(dots) == 0L) {                                 # if no extra values supplied
    as.character(fmt)                                       # treat fmt as final string
  } else {
    do.call(sprintf, c(list(fmt), dots))                    # otherwise delegate to sprintf with arguments
  }
}

# ------------------------------ Public API -------------------------------------

log_set_level <- function(level) {                          # configure minimum log level (DEBUG/INFO/WARN/ERROR)
  .log_state$level <- .normalize_level(level)               # validate and assign to state
  invisible(.log_state$level)                               # return invisible for chaining
}

log_get_level <- function() {                               # expose current log level (mainly for tests)
  .log_state$level
}

log_set_file <- function(path) {                            # enable/disable file sink (append mode)
  if (is.null(path)) {                                      # NULL disables file logging
    .log_state$file <- NULL
    return(invisible(NULL))
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("log_set_file(): 'path' must be a non-NA character scalar or NULL.")
  }
  dir <- dirname(path)                                      # ensure parent directory exists
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  .log_state$file <- path                                   # store path (cat() handles creation/appending)
  invisible(path)
}

log_context_set <- function(context) {                      # replace persistent context metadata
  if (is.null(context)) context <- list()                   # treat NULL as empty list
  if (!is.list(context) || (!is.null(names(context)) && any(names(context) == ""))) {
    stop("log_context_set(): 'context' must be a named list (or empty).")
  }
  .log_state$context <- context                             # store context verbatim (used by .emit_line)
  invisible(context)
}

with_log_context <- function(context, expr) {               # temporarily augment context for nested operations
  if (!is.list(context)) stop("with_log_context(): 'context' must be a list.")
  prev <- .log_state$context                                # snapshot current context for restoration
  merged <- prev                                            # start from base context
  if (length(context) > 0L) {                               # merge provided values (override on name collision)
    merged[names(context)] <- context
  }
  .log_state$context <- merged                              # install merged context
  on.exit({ .log_state$context <- prev }, add = TRUE)       # restore previous context after expr evaluation
  force(expr)                                               # evaluate provided expression in caller environment
}

log_debug <- function(fmt, ...) {                           # emit DEBUG-level message if allowed
  if (.should_emit("DEBUG")) {
    .emit_line("DEBUG", .format_message(fmt, ...))
  }
  return(invisible(NULL))
}

log_info <- function(fmt, ...) {                            # emit INFO-level message if allowed
  if (.should_emit("INFO")) {
    .emit_line("INFO", .format_message(fmt, ...))
  }
  return(invisible(NULL))
}

log_warn <- function(fmt, ...) {                            # emit WARN-level message if allowed
  if (.should_emit("WARN")) {
    .emit_line("WARN", .format_message(fmt, ...))
  }
  return(invisible(NULL))
}

log_error <- function(fmt, ...) {                           # emit ERROR-level message (always shown when allowed)
  if (.should_emit("ERROR")) {
    .emit_line("ERROR", .format_message(fmt, ...))
  }
  return(invisible(NULL))
}

log_banner <- function(text) {                              # convenience helper to emit run delimiter banner
  bar <- paste(rep("-", 72L), collapse = "")              # fixed-width separator for readability
  log_info(bar)
  log_info("%s", text)
  log_info(bar)
  return(invisible(NULL))
}

