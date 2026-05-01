# Build scoring prompt for Gemini
build_scoring_prompt <- function(items_df, config) {
  skills <- get_skill_levels(config)
  skill_lines <- paste(
    paste0("  - ", names(skills), ": ", unlist(skills)),
    collapse = "\n"
  )
  items_block <- build_items_block(items_df)
  paste0(
    "You are a personalized learning content curator.\n\n",
    "USER CONTEXT:\n",
    "- Goals: ", config$learning_goals, "\n",
    "- Project: ", config$project_context, "\n",
    "- Skill Levels:\n", skill_lines, "\n",
    "- Learning Style: ", config$learning_style, "\n",
    "- Depth Preference: ", config$depth_preference, "\n\n",
    "CONTENT ITEMS TO SCORE:\n", items_block, "\n\n",
    "For EACH item, score relevance 0-10 and write exactly one sentence explaining ",
    "why it's relevant (or not) to this user's goals. Use \"you/your\" to make it personal.\n\n",
    "Scoring weights:\n",
    "- Relevance to goals & project (50%)\n",
    "- Skill level alignment (20%)\n",
    "- Learning style match (20%)\n",
    "- Actionability (10%)\n\n",
    "Rules:\n",
    "- Borderline items (score 5-6): include them, let user decide\n",
    "- Don't penalize recent content\n",
    "- Be honest \u2014 score 0-2 if truly irrelevant\n\n",
    "CRITICAL: Respond with ONLY a raw JSON array. No markdown, no commentary, no explanation.\n",
    "Example of the EXACT format required:\n",
    "[{\"index\": 0, \"score\": 8, \"justification\": \"One sentence here.\"}]"
  )
}

build_items_block <- function(items_df) {
  lines <- vapply(seq_len(nrow(items_df)), function(i) {
    row <- items_df[i, ]
    text_preview <- if (nchar(row$text) > 300) paste0(substr(row$text, 1, 300), "...") else row$text
    paste0(
      "[", i - 1, "] Title: ", row$title, "\n",
      "    Source: ", row$source, "\n",
      "    Type: ", row$item_type, "\n",
      "    Description: ", text_preview
    )
  }, character(1))
  paste(lines, collapse = "\n\n")
}
