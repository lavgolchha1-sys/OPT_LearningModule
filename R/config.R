# Google Sheets config read/write
library(googlesheets4)
library(gargle)

# --- Configuration via environment variables ---
SPREADSHEET_ID       <- "1_wwAk_3njHXef5WNVDb7KcF0ZSItwKxeua27jSspzMA"
SERVICE_ACCOUNT_JSON <- "service-account.json"

if (!nzchar(SPREADSHEET_ID)) {
  stop("GOOGLE_SHEET_ID env var not set. Add it to .env or your shell environment.")
}

DEFAULT_CONFIG <- list(
  learning_goals = "Build AI agents using Claude API and MCP. Understand tool calling, agentic workflows, and multi-step reasoning patterns.",
  project_context = "Building a research agent with web search and summarization. Deadline: end of March 2026.",
  skill_MCP = "beginner",
  skill_RAG = "intermediate",
  skill_LLM_fundamentals = "intermediate",
  skill_production_deployment = "beginner",
  learning_style = "build_first",
  depth_preference = "mixed"
)

auth_google_sheets <- function() {
  if (!file.exists(SERVICE_ACCOUNT_JSON)) {
    stop("Service account file not found at: ", SERVICE_ACCOUNT_JSON)
  }
  gs4_auth(path = SERVICE_ACCOUNT_JSON)
}

init_config_sheet <- function() {
  auth_google_sheets()
  existing_sheets <- tryCatch(sheet_names(SPREADSHEET_ID), error = function(e) {
    stop("Cannot access spreadsheet '", SPREADSHEET_ID,
         "'. Verify the ID is correct AND the service account email has Editor access. Original error: ", e$message)
  })
  if (!"Config" %in% existing_sheets) {
    sheet_add(SPREADSHEET_ID, sheet = "Config")
    df <- data.frame(field = names(DEFAULT_CONFIG), value = unlist(DEFAULT_CONFIG), stringsAsFactors = FALSE)
    sheet_write(df, ss = SPREADSHEET_ID, sheet = "Config")
    message("Created Config sheet with defaults")
  }
}

read_config <- function() {
  auth_google_sheets()
  tryCatch({
    df <- read_sheet(SPREADSHEET_ID, sheet = "Config", col_types = "cc")
    if (nrow(df) == 0) return(DEFAULT_CONFIG)
    config <- as.list(setNames(df$value, df$field))
    for (nm in names(DEFAULT_CONFIG)) {
      if (is.null(config[[nm]]) || is.na(config[[nm]])) config[[nm]] <- DEFAULT_CONFIG[[nm]]
    }
    config
  }, error = function(e) {
    message("Error reading config: ", e$message)
    DEFAULT_CONFIG
  })
}

write_config <- function(config) {
  auth_google_sheets()
  df <- data.frame(field = names(config), value = unlist(config), stringsAsFactors = FALSE)
  tryCatch({
    sheet_write(df, ss = SPREADSHEET_ID, sheet = "Config")
    TRUE
  }, error = function(e) {
    message("Error writing config: ", e$message)
    FALSE
  })
}

get_skill_levels <- function(config) {
  skill_keys <- grep("^skill_", names(config), value = TRUE)
  skills <- setNames(unlist(config[skill_keys]), gsub("^skill_", "", skill_keys))
  as.list(skills)
}
