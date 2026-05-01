library(shiny)
library(dplyr)
library(htmltools)

source("R/config.R")
source("R/feedback.R")
source("R/fetch_rss.R")
source("R/fetch_youtube.R")
source("R/fetch_github.R")
source("R/dedup.R")
source("R/prompt.R")
source("R/score_items.R")

TOP_N <- 15
MUST_READ_THRESHOLD <- 8
FILTERED_THRESHOLD <- 5

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css")
  ),
  # Header
  tags$div(class = "opt-header",
           tags$h1(tags$span("OPT"), " \u2014 Learning Feed"),
           tags$p("AI-curated content from your sources, scored for your goals.")
  ),
  # Generate button
  fluidRow(column(12,
                  actionButton("generate_btn", "Generate Digest", class = "btn-generate",
                               icon = icon("bolt"))
  )),
  tags$div(style = "height:16px"),
  # Collapsible settings
  fluidRow(column(12,
                  actionButton("settings_toggle", class = "settings-toggle",
                               label = tagList("Settings & Preferences", tags$span(class = "caret", "\u25BC"))),
                  tags$div(id = "settings_panel", class = "settings-panel",
                           # Row 1: goals + context
                           tags$div(class = "settings-row",
                                    tags$div(
                                      textAreaInput("learning_goals", "Learning Goals", rows = 3, width = "100%"),
                                    ),
                                    tags$div(
                                      textAreaInput("project_context", "Project Context", rows = 3, width = "100%"),
                                    )
                           ),
                           tags$hr(class = "settings-divider"),
                           # Row 2: style + depth
                           tags$div(class = "settings-row",
                                    selectInput("learning_style", "Learning Style",
                                                choices = c("Build First" = "build_first", "Theory First" = "theory_first",
                                                            "Video Learner" = "video_learner"), width = "100%"),
                                    selectInput("depth_preference", "Depth Preference",
                                                choices = c("Quick Wins" = "quick_wins", "Deep Dives" = "deep_dives",
                                                            "Mixed" = "mixed"), width = "100%")
                           ),
                           tags$hr(class = "settings-divider"),
                           # Row 3: skill levels
                           tags$div(class = "settings-row-3",
                                    selectInput("skill_MCP", "MCP",
                                                choices = c("beginner", "intermediate", "advanced"), width = "100%"),
                                    selectInput("skill_RAG", "RAG",
                                                choices = c("beginner", "intermediate", "advanced"), width = "100%"),
                                    selectInput("skill_LLM_fundamentals", "LLM Fundamentals",
                                                choices = c("beginner", "intermediate", "advanced"), width = "100%"),
                                    selectInput("skill_production_deployment", "Production Deployment",
                                                choices = c("beginner", "intermediate", "advanced"), width = "100%")
                           ),
                           tags$div(style = "height: 8px"),
                           fluidRow(
                             column(6, actionButton("save_settings", "Save Settings", class = "btn-save", icon = icon("check"))),
                             column(6, tags$div(class = "text-end", style = "padding-top:8px; font-size:12px; color:#5a5c66;",
                                                icon("clock"), " Scheduling \u2014 coming soon"
                             ))
                           )
                  )
  )),
  # Feed content
  fluidRow(column(12, uiOutput("digest_ui"))),
  # Content viewer modal
  tags$div(id = "content-modal", class = "content-modal", onclick = "if(event.target===this)closeModal()",
           tags$div(class = "modal-container", id = "modal-container",
                    tags$div(class = "modal-header",
                             tags$span(id = "modal-title", class = "modal-title", ""),
                             tags$div(class = "modal-actions",
                                      tags$a(id = "modal-external", href = "#", target = "_blank", class = "btn-modal-action",
                                             icon("arrow-up-right-from-square"), " Open in source"),
                                      tags$button(class = "btn-modal-close", onclick = "closeModal()",
                                                  icon("xmark"))
                             )
                    ),
                    tags$div(id = "modal-body", class = "modal-body")
           )
  ),
  # Footer
  tags$div(class = "opt-footer", "OPT \u2014 Update your goals in Settings to refine results."),
  # JS for collapsible settings
  tags$script(HTML("
    $(document).on('click', '#settings_toggle', function() {
      $('#settings_panel').toggleClass('show');
      $(this).toggleClass('open');
    });
    Shiny.addCustomMessageHandler('vote', function(msg) {
      var btn = document.getElementById(msg.id);
      if (btn) btn.classList.add(msg.cls);
    });
    function getYouTubeId(url) {
      var m = url.match(/(?:youtube\\.com\\/watch\\?v=|youtu\\.be\\/)([a-zA-Z0-9_-]{11})/);
      return m ? m[1] : null;
    }
    function openModal(url, title, source) {
      var modal = document.getElementById('content-modal');
      var container = document.getElementById('modal-container');
      var body = document.getElementById('modal-body');
      var titleEl = document.getElementById('modal-title');
      var extLink = document.getElementById('modal-external');
      titleEl.textContent = title;
      extLink.href = url;
      container.className = 'modal-container';
      var ytId = getYouTubeId(url);
      if (ytId) {
        container.classList.add('modal-video');
        body.innerHTML = '<iframe src=\"https://www.youtube.com/embed/' + ytId +
          '?autoplay=1&rel=0\" class=\"yt-embed\" ' +
          'allow=\"autoplay; encrypted-media; picture-in-picture\" allowfullscreen></iframe>';
      } else {
        container.classList.add('modal-article');
        var icon = url.indexOf('github.com') !== -1 ? '\uD83D\uDCE6' :
                   url.indexOf('substack.com') !== -1 ? '\u2709\uFE0F' : '\uD83D\uDCF0';
        body.innerHTML = '<div class=\"article-preview\">' +
          '<div class=\"preview-icon\">' + icon + '</div>' +
          '<div class=\"preview-source\">' + (source || '') + '</div>' +
          '<a href=\"' + url + '\" target=\"_blank\" class=\"btn-read-article\">' +
          '<i class=\"fa-solid fa-book-open\"></i> Read Article</a></div>';
      }
      modal.classList.add('open');
      document.body.style.overflow = 'hidden';
    }
    function closeModal() {
      var modal = document.getElementById('content-modal');
      document.getElementById('modal-body').innerHTML = '';
      document.getElementById('modal-container').className = 'modal-container';
      modal.classList.remove('open');
      document.body.style.overflow = '';
    }
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') closeModal();
    });
  "))
)

# --- Server ---
server <- function(input, output, session) {
  rv <- reactiveValues(scored = NULL, feedback_given = list())
  
  # Load config on startup
  observe({
    tryCatch({
      init_config_sheet()
      init_feedback_sheet()
      cfg <- read_config()
      updateTextAreaInput(session, "learning_goals", value = cfg$learning_goals)
      updateTextAreaInput(session, "project_context", value = cfg$project_context)
      updateSelectInput(session, "learning_style", selected = cfg$learning_style)
      updateSelectInput(session, "depth_preference", selected = cfg$depth_preference)
      updateSelectInput(session, "skill_MCP", selected = cfg$skill_MCP)
      updateSelectInput(session, "skill_RAG", selected = cfg$skill_RAG)
      updateSelectInput(session, "skill_LLM_fundamentals", selected = cfg$skill_LLM_fundamentals)
      updateSelectInput(session, "skill_production_deployment", selected = cfg$skill_production_deployment)
    }, error = function(e) {
      showNotification(paste("Could not load settings:", e$message), type = "warning")
    })
  }) |> bindEvent(TRUE)
  
  # Save settings
  observeEvent(input$save_settings, {
    cfg <- list(
      learning_goals = input$learning_goals,
      project_context = input$project_context,
      learning_style = input$learning_style,
      depth_preference = input$depth_preference,
      skill_MCP = input$skill_MCP,
      skill_RAG = input$skill_RAG,
      skill_LLM_fundamentals = input$skill_LLM_fundamentals,
      skill_production_deployment = input$skill_production_deployment
    )
    ok <- write_config(cfg)
    if (ok) showNotification("Settings saved!", type = "message")
    else showNotification("Failed to save settings.", type = "error")
  })
  
  # Generate digest
  observeEvent(input$generate_btn, {
    config <- list(
      learning_goals = input$learning_goals,
      project_context = input$project_context,
      learning_style = input$learning_style,
      depth_preference = input$depth_preference,
      skill_MCP = input$skill_MCP,
      skill_RAG = input$skill_RAG,
      skill_LLM_fundamentals = input$skill_LLM_fundamentals,
      skill_production_deployment = input$skill_production_deployment
    )
    rv$scored <- NULL
    rv$feedback_given <- list()
    withProgress(message = "Generating digest...", value = 0, {
      setProgress(0.1, detail = "Fetching RSS feeds...")
      rss_items <- tryCatch(fetch_rss(), error = function(e) {
        showNotification(paste("RSS error:", e$message), type = "warning"); data.frame()
      })
      setProgress(0.3, detail = "Fetching YouTube...")
      yt_items <- tryCatch(fetch_youtube(), error = function(e) {
        showNotification(paste("YouTube error:", e$message), type = "warning"); data.frame()
      })
      setProgress(0.4, detail = "Fetching GitHub Trending...")
      gh_items <- tryCatch(fetch_github_trending(), error = function(e) {
        showNotification(paste("GitHub error:", e$message), type = "warning"); data.frame()
      })
      items <- dplyr::bind_rows(rss_items, yt_items, gh_items)
      if (nrow(items) == 0) { showNotification("No content found.", type = "warning"); return() }
      setProgress(0.5, detail = paste0(nrow(items), " items. Deduplicating..."))
      items <- deduplicate(items)
      setProgress(0.6, detail = "Scoring with Gemini...")
      scored <- score_items(items, config)
      setProgress(0.9, detail = "Ranking...")
      scored <- scored[order(-scored$score), ]
      scored <- scored[scored$score >= FILTERED_THRESHOLD, ]
      if (nrow(scored) > TOP_N) scored <- scored[1:TOP_N, ]
      rv$scored <- scored
      setProgress(1, detail = "Done!")
    })
  })
  
  # Render digest
  output$digest_ui <- renderUI({
    scored <- rv$scored
    if (is.null(scored)) {
      return(tags$div(class = "empty-state",
                      tags$div(class = "icon", "\U0001F4E1"),
                      tags$h3("No digest yet"),
                      tags$p("Hit Generate Digest to fetch your personalized feed.")
      ))
    }
    if (nrow(scored) == 0) {
      return(tags$div(class = "empty-state",
                      tags$div(class = "icon", "\U0001F50D"),
                      tags$h3("Nothing passed the filter"),
                      tags$p("Try updating your learning goals in Settings.")
      ))
    }
    total <- nrow(scored)
    must_read <- scored[scored$score >= MUST_READ_THRESHOLD, ]
    worth_time <- scored[scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD, ]
    n_sources <- length(unique(scored$source))
    cards <- list(
      tags$div(class = "stats-bar",
               tags$span(icon("layer-group"), paste0(total, " items")),
               tags$span(icon("database"), paste0(n_sources, " sources")),
               tags$span(icon("calendar"), format(Sys.Date(), "%b %d, %Y"))
      )
    )
    if (nrow(must_read) > 0) {
      cards <- c(cards, list(tags$div(class = "section-label must-read",
                                      paste0("\U0001F525 MUST READ (", nrow(must_read), ")")
      )))
      for (i in seq_len(nrow(must_read))) {
        cards <- c(cards, list(build_item_card(must_read[i, ], paste0("mr_", i))))
      }
    }
    if (nrow(worth_time) > 0) {
      cards <- c(cards, list(tags$div(class = "section-label worth-time",
                                      paste0("\U0001F4DA WORTH YOUR TIME (", nrow(worth_time), ")")
      )))
      for (i in seq_len(nrow(worth_time))) {
        cards <- c(cards, list(build_item_card(worth_time[i, ], paste0("wt_", i))))
      }
    }
    tagList(cards)
  })
  
  # Feedback observers
  observe({
    scored <- rv$scored
    if (is.null(scored)) return()
    all_ids <- c(
      if (any(scored$score >= MUST_READ_THRESHOLD))
        paste0("mr_", seq_len(sum(scored$score >= MUST_READ_THRESHOLD))),
      if (any(scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD))
        paste0("wt_", seq_len(sum(scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD)))
    )
    for (id in all_ids) {
      local({
        local_id <- id
        observeEvent(input[[paste0("up_", local_id)]], {
          handle_feedback(local_id, "useful", rv, session)
        }, ignoreInit = TRUE, once = TRUE)
        observeEvent(input[[paste0("down_", local_id)]], {
          handle_feedback(local_id, "not_useful", rv, session)
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  })
}

# --- Build item card ---
build_item_card <- function(row, card_id) {
  score <- row$score
  score_class <- if (score >= 8) "score-high" else if (score >= 6) "score-mid" else "score-low"
  type_icon <- switch(as.character(row$item_type),
                      video = "\U0001F4FA", repo = "\U0001F4E6", tweet = "\U0001F426", "\U0001F4F0")
  # Escape single quotes in title for JS
  safe_title <- gsub("'", "\\\\'", row$title)
  safe_source <- gsub("'", "\\\\'", row$source)
  safe_url <- row$url
  tags$div(class = "item-card",
           tags$div(class = "item-top",
                    tags$div(class = "item-title-wrap",
                             tags$a(class = "item-title", href = "#",
                                    onclick = paste0("event.preventDefault(); openModal('", safe_url, "','", safe_title, "','", safe_source, "')"),
                                    row$title),
                             tags$div(class = "item-meta",
                                      tags$span(class = "source-icon", type_icon),
                                      tags$span(row$source)
                             )
                    ),
                    tags$div(style = "display:flex; align-items:center; gap:8px;",
                             tags$span(class = paste("score-badge", score_class), paste0(score, "/10")),
                             tags$div(class = "feedback-wrap",
                                      actionButton(paste0("up_", card_id), label = NULL, icon = icon("thumbs-up"),
                                                   class = "btn-feedback"),
                                      actionButton(paste0("down_", card_id), label = NULL, icon = icon("thumbs-down"),
                                                   class = "btn-feedback")
                             )
                    )
           ),
           tags$div(class = "item-why", paste0("\u2192 ", row$justification))
  )
}

# --- Handle feedback ---
handle_feedback <- function(card_id, feedback_type, rv, session) {
  if (!is.null(rv$feedback_given[[card_id]])) return()
  scored <- rv$scored
  if (is.null(scored)) return()
  parts <- strsplit(card_id, "_")[[1]]
  prefix <- parts[1]
  idx <- as.integer(parts[2])
  if (prefix == "mr") {
    subset <- scored[scored$score >= MUST_READ_THRESHOLD, ]
  } else {
    subset <- scored[scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD, ]
  }
  if (idx > nrow(subset)) return()
  row <- subset[idx, ]
  log_feedback(row$title, row$url, row$source, row$score, row$justification, feedback_type)
  rv$feedback_given[[card_id]] <- feedback_type
  # Highlight the clicked button via JS
  btn_id <- if (feedback_type == "useful") paste0("up_", card_id) else paste0("down_", card_id)
  vote_class <- if (feedback_type == "useful") "voted-up" else "voted-down"
  session$sendCustomMessage("vote", list(id = btn_id, cls = vote_class))
  showNotification(
    if (feedback_type == "useful") "\u2705 Marked useful" else "\u274C Marked not useful",
    type = "message", duration = 2
  )
}

shinyApp(ui, server)