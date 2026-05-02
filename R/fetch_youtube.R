# Fetch recent videos from YouTube channels via Data API v3
library(httr2)
library(jsonlite)
library(rlang)  # for %||%

YOUTUBE_API_KEY    <- "AIzaSyCn-M1HZCi3zxHBKXxgQ0cm5Ssg8jlPA8s"
YOUTUBE_SEARCH_URL <- "https://www.googleapis.com/youtube/v3/search"
LOOKBACK_HOURS_YT  <- 168  # 7 days

YOUTUBE_CHANNELS <- list(
  list(channel_id = "UCLKPca3kwwd-B59HNr-_lvA", name = "AI Engineer"),
  list(channel_id = "UCXUPKJO5MZQN11PqgIvyuvQ", name = "Andrej Karpathy"),
  list(channel_id = "UCXl4i9dYBrFOabk0xGmbkRA", name = "Dwarkesh Podcast")
)

fetch_youtube <- function(channels = YOUTUBE_CHANNELS, api_key = YOUTUBE_API_KEY,
                          lookback_hours = LOOKBACK_HOURS_YT) {
  if (is.null(api_key) || api_key == "") {
    message("  \u26A0 YouTube API key not set \u2014 skipping YouTube sources")
    return(data.frame())
  }
  cutoff <- format(Sys.time() - as.difftime(lookback_hours, units = "hours"),
                   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  items <- list()
  for (ch in channels) {
    tryCatch({
      message("  Fetching YouTube: ", ch$name, "...")
      resp <- request(YOUTUBE_SEARCH_URL) |>
        req_url_query(
          key = api_key, channelId = ch$channel_id,
          order = "date", type = "video", maxResults = 20,
          part = "snippet", publishedAfter = cutoff
        ) |>
        req_timeout(15) |>
        req_perform()
      data <- resp_body_json(resp)
      videos <- data$items %||% list()
      count <- 0
      for (v in videos) {
        video_id <- v$id$videoId
        if (is.null(video_id)) next
        snippet <- v$snippet
        title <- snippet$title %||% "Untitled"
        desc <- snippet$description %||% ""
        if (nchar(desc) > 1000) desc <- paste0(substr(desc, 1, 1000), "...")
        pub_raw <- snippet$publishedAt %||% ""
        published <- tryCatch(as.POSIXct(pub_raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                              error = function(e) Sys.time())
        url <- paste0("https://youtube.com/watch?v=", video_id)
        items[[length(items) + 1]] <- data.frame(
          title = title, source = paste0(ch$name, " (YouTube)"),
          url = url, text = desc, published_at = published,
          item_type = "video", stringsAsFactors = FALSE
        )
        count <- count + 1
      }
      message("  \u2713 ", ch$name, ": ", count, " recent video(s)")
    }, error = function(e) {
      message("  \u26A0 Error fetching ", ch$name, ": ", e$message, " \u2014 skipping")
    })
  }
  if (length(items) == 0) return(data.frame())
  dplyr::bind_rows(items)
}
