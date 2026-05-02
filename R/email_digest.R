# Email digest pipeline: fetch -> score -> render HTML -> send via Resend
# Designed to be called from GitHub Actions (Rscript R/email_digest.R [mode])
#
# Modes:
#   "digest"     - daily digest email (default)
#   "onboarding" - one-time welcome email
#   "alert"      - precision <60% alert
#
# Reads all secrets from env vars. Does not depend on Shiny.

library(httr2)
library(jsonlite)
library(dplyr)

source("R/config.R")
source("R/feedback.R")
source("R/fetch_rss.R")
source("R/fetch_youtube.R")
source("R/fetch_github.R")
source("R/dedup.R")
source("R/prompt.R")
source("R/score_items.R")

RESEND_API_KEY <- ${{ secrets.RESEND_API_KEY }}
RESEND_FROM     <- "OPT Digest <onboarding@resend.dev>"
DIGEST_TO       <- ${{ secrets.DIGEST_EMAIL }}
RESEND_API_URL  <- "https://api.resend.com/emails"

TOP_N <- 15
MUST_READ_THRESHOLD <- 8
FILTERED_THRESHOLD <- 5
PRECISION_ALERT_THRESHOLD <- 0.60
PRECISION_WINDOW_DAYS <- 3

# ---------------------------------------------------------------- send via Resend
send_email <- function(subject, html, to = DIGEST_TO) {
  if (!nzchar(RESEND_API_KEY)) stop("RESEND_API_KEY not set")
  if (!nzchar(to)) stop("Recipient email not set (DIGEST_EMAIL env var)")
  body <- list(from = RESEND_FROM, to = list(to), subject = subject, html = html)
  resp <- request(RESEND_API_URL) |>
    req_headers(
      Authorization = paste("Bearer", RESEND_API_KEY),
      `Content-Type` = "application/json"
    ) |>
    req_body_raw(toJSON(body, auto_unbox = TRUE)) |>
    req_timeout(30) |>
    req_perform()
  if (resp_status(resp) >= 400) {
    stop("Resend API error: ", resp_body_string(resp))
  }
  message("\u2713 Email sent: ", subject)
  invisible(TRUE)
}

# ---------------------------------------------------------------- HTML helpers
html_escape <- function(s) {
  if (is.null(s) || is.na(s)) return("")
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s
}

# Single inline-styled email shell. Email clients hate external CSS.
email_shell <- function(title, body_html) {
  paste0('<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>', html_escape(title), '</title></head>',
    '<body style="margin:0;padding:0;background:#0e1014;',
    'font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,sans-serif;',
    'color:#e8e9ed;line-height:1.55;">',
    '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" ',
    'style="background:#0e1014;padding:32px 16px;"><tr><td align="center">',
    '<table role="presentation" width="600" cellpadding="0" cellspacing="0" border="0" ',
    'style="max-width:600px;width:100%;">',
    body_html,
    '</table></td></tr></table></body></html>')
}

email_header <- function() {
  paste0(
    '<tr><td style="padding-bottom:24px;border-bottom:1px solid rgba(255,255,255,0.08);">',
    '<div style="font-size:18px;font-weight:600;color:#e8e9ed;">',
    '<span style="color:#8b8fff;">OPT</span> Learning Feed</div>',
    '<div style="font-size:13px;color:#8a8d96;margin-top:4px;">',
    format(Sys.Date(), "%A, %B %d, %Y"),
    '</div></td></tr>'
  )
}

email_footer <- function(extra = "") {
  paste0(
    '<tr><td style="padding:32px 0 0;border-top:1px solid rgba(255,255,255,0.08);',
    'margin-top:24px;text-align:center;font-size:12px;color:#5a5d66;">',
    extra,
    'OPT \u2014 Update goals via the Shiny app to refine results.',
    '</td></tr>'
  )
}

render_item_row <- function(row) {
  score <- as.integer(row$score)
  badge_color <- if (score >= 8) c("#6acc8c","rgba(106,204,140,0.10)")
                 else if (score >= 6) c("#d9a949","rgba(217,169,73,0.10)")
                 else c("#8a8d96","rgba(138,141,150,0.10)")
  type_label <- switch(as.character(row$item_type),
                       video = "video", repo = "repo", "article")
  paste0(
    '<tr><td style="padding:14px 16px;background:#15171c;',
    'border:1px solid rgba(255,255,255,0.06);border-radius:12px;margin-bottom:8px;">',
    '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0">',
    '<tr><td valign="top" style="padding-right:12px;">',
    '<a href="', html_escape(row$url), '" target="_blank" ',
    'style="font-size:14px;font-weight:500;color:#e8e9ed;text-decoration:none;line-height:1.45;">',
    html_escape(row$title), '</a>',
    '<div style="font-size:12px;color:#5a5d66;margin-top:4px;">',
    type_label, ' \u00b7 ', html_escape(row$source), '</div>',
    '</td><td valign="top" align="right" style="white-space:nowrap;">',
    '<span style="font-family:SF Mono,Menlo,monospace;font-size:11px;font-weight:500;',
    'padding:3px 8px;border-radius:4px;background:', badge_color[2], ';color:', badge_color[1], ';">',
    score, '/10</span>',
    '</td></tr>',
    '<tr><td colspan="2" style="font-size:13px;color:#8a8d96;padding-top:8px;line-height:1.5;">',
    '\u2192 ', html_escape(row$justification),
    '</td></tr></table></td></tr>',
    '<tr><td style="height:8px;"></td></tr>'
  )
}

section_heading <- function(label, color, count) {
  paste0(
    '<tr><td style="padding:24px 0 12px;font-size:11px;font-weight:500;',
    'text-transform:uppercase;letter-spacing:1px;color:#8a8d96;">',
    '<span style="display:inline-block;width:6px;height:6px;border-radius:50%;',
    'background:', color, ';margin-right:8px;vertical-align:middle;"></span>',
    label, ' \u00b7 ', count,
    '</td></tr>'
  )
}

# ---------------------------------------------------------------- digest
generate_and_send_digest <- function() {
  message("[", Sys.time(), "] Starting digest generation")
  config <- read_config()

  # Fetch
  rss_items <- tryCatch(fetch_rss(),     error = function(e) { message("RSS error: ", e$message); data.frame() })
  yt_items  <- tryCatch(fetch_youtube(), error = function(e) { message("YT error: ", e$message); data.frame() })
  gh_items  <- tryCatch(fetch_github_trending(), error = function(e) { message("GH error: ", e$message); data.frame() })
  items <- bind_rows(rss_items, yt_items, gh_items)
  if (nrow(items) == 0) {
    message("\u26A0 No items fetched, skipping email")
    return(invisible(FALSE))
  }
  items <- deduplicate(items)

  # Score
  scored <- score_items(items, config)
  scored <- scored[order(-scored$score), ]
  scored <- scored[scored$score >= FILTERED_THRESHOLD, ]
  if (nrow(scored) > TOP_N) scored <- scored[1:TOP_N, ]
  if (nrow(scored) == 0) {
    message("\u26A0 No items passed threshold, skipping email")
    return(invisible(FALSE))
  }

  must_read  <- scored[scored$score >= MUST_READ_THRESHOLD, ]
  worth_time <- scored[scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD, ]

  body <- email_header()
  body <- paste0(body,
    '<tr><td style="padding:16px 0;font-size:12px;color:#5a5d66;">',
    nrow(scored), ' items \u00b7 ', length(unique(scored$source)), ' sources',
    '</td></tr>')

  if (nrow(must_read) > 0) {
    body <- paste0(body, section_heading("Must read", "#e8676b", nrow(must_read)))
    for (i in seq_len(nrow(must_read))) {
      body <- paste0(body, render_item_row(must_read[i, ]))
    }
  }
  if (nrow(worth_time) > 0) {
    body <- paste0(body, section_heading("Worth your time", "#d9a949", nrow(worth_time)))
    for (i in seq_len(nrow(worth_time))) {
      body <- paste0(body, render_item_row(worth_time[i, ]))
    }
  }
  body <- paste0(body, email_footer())

  subject <- paste0(
    if (nrow(must_read) > 0) paste0(nrow(must_read), " must-reads + ") else "",
    nrow(scored), " items today \u00b7 OPT"
  )
  send_email(subject, email_shell("OPT Digest", body))

  # Retention + monitoring after each successful send
  tryCatch(prune_old_feedback(), error = function(e) message("Prune failed: ", e$message))
  tryCatch(check_precision_alert(), error = function(e) message("Precision check failed: ", e$message))

  invisible(TRUE)
}

# ---------------------------------------------------------------- onboarding
send_onboarding_email <- function() {
  config <- tryCatch(read_config(), error = function(e) DEFAULT_CONFIG)
  body <- email_header()
  body <- paste0(body,
    '<tr><td style="padding:24px 0 16px;font-size:15px;color:#e8e9ed;line-height:1.6;">',
    'Welcome to OPT \u2014 your AI-curated learning feed.',
    '</td></tr>',
    '<tr><td style="padding:0 0 16px;font-size:14px;color:#8a8d96;line-height:1.6;">',
    'Starting tomorrow at 6 AM, you\u2019ll get a daily digest of 10\u201315 items ',
    'pulled from your RSS, YouTube, and GitHub Trending sources, scored against ',
    'your learning goals.',
    '</td></tr>',
    '<tr><td style="padding:16px;background:#15171c;border:1px solid rgba(255,255,255,0.06);',
    'border-radius:12px;font-size:13px;color:#e8e9ed;">',
    '<div style="font-size:11px;text-transform:uppercase;letter-spacing:1px;color:#8a8d96;margin-bottom:12px;">Your current context</div>',
    '<div style="margin-bottom:8px;"><strong style="color:#8b8fff;">Goals:</strong> ', html_escape(config$learning_goals), '</div>',
    '<div style="margin-bottom:8px;"><strong style="color:#8b8fff;">Project:</strong> ', html_escape(config$project_context), '</div>',
    '<div><strong style="color:#8b8fff;">Style:</strong> ', html_escape(config$learning_style), ' \u00b7 ',
    '<strong style="color:#8b8fff;">Depth:</strong> ', html_escape(config$depth_preference), '</div>',
    '</td></tr>',
    '<tr><td style="padding:24px 0 0;font-size:13px;color:#5a5d66;line-height:1.6;">',
    'Update these any time in the OPT Shiny app. The first digest arrives tomorrow morning.',
    '</td></tr>')
  body <- paste0(body, email_footer())
  send_email("Welcome to OPT \u2014 your first digest arrives tomorrow", email_shell("Welcome to OPT", body))
}

# ---------------------------------------------------------------- precision alert
check_precision_alert <- function() {
  result <- compute_precision(days_back = PRECISION_WINDOW_DAYS, min_items = 5)
  if (is.null(result)) {
    message("Precision check: insufficient data, skipping")
    return(invisible(FALSE))
  }
  message("Precision (last ", result$window_days, "d): ",
          round(result$precision * 100), "% (", result$useful, "/", result$total, ")")
  if (result$precision < PRECISION_ALERT_THRESHOLD) {
    send_precision_alert(result)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

send_precision_alert <- function(result) {
  body <- email_header()
  body <- paste0(body,
    '<tr><td style="padding:24px 0 16px;font-size:15px;color:#e8676b;font-weight:500;">',
    'Heads up: your digest precision dropped',
    '</td></tr>',
    '<tr><td style="padding:0 0 16px;font-size:14px;color:#8a8d96;line-height:1.6;">',
    'Over the last ', result$window_days, ' days, only ',
    round(result$precision * 100), '% of items you rated were marked useful (',
    result$useful, ' of ', result$total, ').',
    '</td></tr>',
    '<tr><td style="padding:16px;background:#15171c;border:1px solid rgba(255,255,255,0.06);',
    'border-radius:12px;font-size:13px;color:#8a8d96;line-height:1.7;">',
    '<strong style="color:#e8e9ed;">Likely fixes:</strong><br>',
    '\u2022 Open the OPT app and check whether your learning goals still reflect what you\u2019re building<br>',
    '\u2022 Update skill levels if you\u2019ve advanced past beginner in any area<br>',
    '\u2022 Tighten the project context with a concrete deliverable',
    '</td></tr>')
  body <- paste0(body, email_footer())
  send_email("OPT precision dropped \u2014 review your context", email_shell("Precision Alert", body))
}

# ---------------------------------------------------------------- entrypoint
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  mode <- if (length(args) > 0) args[1] else "digest"
  switch(mode,
    digest     = generate_and_send_digest(),
    onboarding = send_onboarding_email(),
    alert      = check_precision_alert(),
    stop("Unknown mode: ", mode))
}

if (!interactive()) {
  tryCatch(main(), error = function(e) {
    message("FATAL: ", e$message)
    quit(status = 1)
  })
}
