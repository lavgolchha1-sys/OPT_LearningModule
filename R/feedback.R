# Google Sheets feedback logging + 30-day retention
library(googlesheets4)
library(dplyr)

FEEDBACK_RETENTION_DAYS <- 30

init_feedback_sheet <- function() {
  auth_google_sheets()
  existing_sheets <- tryCatch(sheet_names(SPREADSHEET_ID), error = function(e) character(0))
  if (!"Feedback" %in% existing_sheets) {
    sheet_add(SPREADSHEET_ID, sheet = "Feedback")
    header <- data.frame(
      timestamp = character(0), title = character(0), url = character(0),
      source = character(0), score = integer(0), justification = character(0),
      feedback = character(0), stringsAsFactors = FALSE
    )
    sheet_write(header, ss = SPREADSHEET_ID, sheet = "Feedback")
    message("Created Feedback sheet")
  }
}

log_feedback <- function(title, url, source, score, justification, feedback) {
  auth_google_sheets()
  row <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    title = title, url = url, source = source,
    score = as.integer(score), justification = justification,
    feedback = feedback, stringsAsFactors = FALSE
  )
  tryCatch({
    sheet_append(SPREADSHEET_ID, data = row, sheet = "Feedback")
    TRUE
  }, error = function(e) {
    message("Error logging feedback: ", e$message)
    FALSE
  })
}

# Prune feedback rows older than FEEDBACK_RETENTION_DAYS.
# Called by the scheduled GitHub Actions job (not from the Shiny app).
prune_old_feedback <- function() {
  auth_google_sheets()
  tryCatch({
    df <- read_sheet(SPREADSHEET_ID, sheet = "Feedback", col_types = "ccccncc")
    if (nrow(df) == 0) return(invisible(0))
    df$timestamp_parsed <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    cutoff <- Sys.time() - as.difftime(FEEDBACK_RETENTION_DAYS, units = "days")
    keep <- df[!is.na(df$timestamp_parsed) & df$timestamp_parsed >= cutoff, ]
    keep$timestamp_parsed <- NULL
    removed <- nrow(df) - nrow(keep)
    if (removed > 0) {
      sheet_write(keep, ss = SPREADSHEET_ID, sheet = "Feedback")
      message("Pruned ", removed, " feedback row(s) older than ", FEEDBACK_RETENTION_DAYS, " days")
    }
    invisible(removed)
  }, error = function(e) {
    message("Error pruning feedback: ", e$message)
    invisible(-1)
  })
}

# Compute precision over the last `days_back` days for monitoring alerts.
# Returns list(precision = numeric, total = int, useful = int) or NULL if insufficient data.
compute_precision <- function(days_back = 3, min_items = 5) {
  auth_google_sheets()
  tryCatch({
    df <- read_sheet(SPREADSHEET_ID, sheet = "Feedback", col_types = "ccccncc")
    if (nrow(df) < min_items) return(NULL)
    df$timestamp_parsed <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    cutoff <- Sys.time() - as.difftime(days_back, units = "days")
    recent <- df[!is.na(df$timestamp_parsed) & df$timestamp_parsed >= cutoff, ]
    if (nrow(recent) < min_items) return(NULL)
    useful <- sum(recent$feedback == "useful", na.rm = TRUE)
    list(
      precision = useful / nrow(recent),
      total = nrow(recent),
      useful = useful,
      window_days = days_back
    )
  }, error = function(e) {
    message("Error computing precision: ", e$message)
    NULL
  })
}
