# Simple deduplication by URL
library(dplyr)

deduplicate <- function(items) {
  if (nrow(items) == 0) return(items)
  items$url_normalized <- tolower(trimws(gsub("/$", "", items$url)))
  deduped <- items[!duplicated(items$url_normalized), ]
  deduped$url_normalized <- NULL
  removed <- nrow(items) - nrow(deduped)
  if (removed > 0) message("  Removed ", removed, " duplicate(s)")
  deduped
}
