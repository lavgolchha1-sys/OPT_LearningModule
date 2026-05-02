# Score content items using ellmer + Gemini
library(ellmer)
library(jsonlite)
library(dplyr)

GEMINI_API_KEY <- (function() {
  key <- ${{ secrets.GEMINI_API_KEY }}
  if (nchar(key) > 0) key else "AIzaSyDmR5wKiIL-BhBNUtOjzGqxE2uv7Jt5rtI"
})()
GEMINI_MODEL   <- Sys.getenv("GEMINI_MODEL", "gemini-2.5-flash")

if (!nzchar(GEMINI_API_KEY)) {
  warning("GEMINI_API_KEY env var not set. Scoring will fail.")
}

score_items <- function(items_df, config) {
  if (nrow(items_df) == 0) return(items_df)
  prompt <- build_scoring_prompt(items_df, config)
  tryCatch({
    chat <- chat_google_gemini(model = GEMINI_MODEL, api_key = GEMINI_API_KEY)
    response <- chat$chat(prompt)
    scores <- parse_scores(response, nrow(items_df))
    items_df$score <- scores$score
    items_df$justification <- scores$justification
    items_df
  }, error = function(e) {
    message("  \u26A0 Scoring error: ", e$message, " \u2014 dropping items")
    items_df$score <- 0L  # Was 5L — dropping to 0 so they fail the >=5 filter
    items_df$justification <- paste("Could not score:", e$message)
    items_df
  })
}

parse_scores <- function(response_text, n_items) {
  text <- trimws(response_text)
  text <- sub("^\\s*```[a-zA-Z]*\\s*\\n?", "", text)
  text <- sub("\\n?\\s*```\\s*$", "", text)
  text <- trimws(text)
  json_match <- regmatches(text, regexpr("\\[\\s*\\{[\\s\\S]*\\}\\s*\\]", text, perl = TRUE))
  if (length(json_match) == 1) text <- json_match
  scores_list <- fromJSON(text, simplifyDataFrame = TRUE)
  if (!is.data.frame(scores_list)) scores_list <- bind_rows(scores_list)
  result <- data.frame(
    score = rep(5L, n_items),
    justification = rep("Item was not scored by Gemini.", n_items),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(scores_list))) {
    idx <- scores_list$index[i] + 1
    if (!is.na(idx) && idx >= 1 && idx <= n_items) {
      result$score[idx] <- max(0L, min(10L, as.integer(scores_list$score[i])))
      result$justification[idx] <- as.character(scores_list$justification[i])
    }
  }
  result
}
