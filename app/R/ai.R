# ANCHOR Studio - drafting back-ends.
# Mode "api":    Anthropic Messages API with the researcher's own key
# Mode "prompt": copy-and-paste prompts for claude.ai (no key needed)
# Mode "ollama": a local model served by Ollama on this computer
# Mode "manual": no AI; the researcher writes every phase

AI_MODES <- c(
  "Copy-and-paste prompts (Claude.ai, no key)" = "prompt",
  "Claude API key (drafts inside the app)" = "api",
  "Local AI with Ollama (offline)" = "ollama",
  "Manual only (no AI)" = "manual"
)

build_prompt <- function(phase, article_text, context, answers, include_text = TRUE, max_chars = 120000, lang = "en") {
  ans <- if (length(answers)) paste0("- ", names(answers), ": ", vapply(answers, function(x) paste(x, collapse = "; "), ""), collapse = "\n") else "- (none yet)"
  art <- if (include_text && nzchar(article_text)) {
    paste0("\n\n=== ARTICLE TEXT (extracted from the uploaded PDF) ===\n", substr(article_text, 1, max_chars),
           if (nchar(article_text) > max_chars) "\n[... text truncated ...]" else "")
  } else {
    "\n\n(The article PDF is attached to this conversation; use it as the only source.)"
  }
  paste0(
    phase$prompt,
    "\n\n=== RESEARCHER'S ANSWERS FOR THIS PHASE ===\n", ans,
    "\n\n=== APPROVED EARLIER PHASES ===\n", if (nzchar(context)) context else "(none yet)",
    art, QUESTION_TAIL, lang_line(lang)
  )
}

# drafts in German or Arabic: the instructions stay in English, the answer comes in the chosen language
LANG_NAME <- c(de = "German", ar = "Arabic")
lang_line <- function(lang) {
  if (is.null(lang) || !lang %in% names(LANG_NAME)) return("")
  sprintf("\n\nWrite the draft in %s. Keep the exact line 'QUESTIONS FOR THE RESEARCHER:' in English so the app can find your questions; write the questions themselves in %s.", LANG_NAME[[lang]], LANG_NAME[[lang]])
}

ai_draft <- function(mode, prompt, settings) {
  if (mode == "api") {
    key <- settings$api_key
    if (!nzchar(key)) stop("No API key set. Add one in Settings, or switch to copy-and-paste prompts.")
    req <- httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(`x-api-key` = key, `anthropic-version` = "2023-06-01", `content-type` = "application/json") |>
      httr2::req_body_json(list(
        model = settings$api_model, max_tokens = 8000,
        system = "You are a careful research assistant. Use only the supplied sources, never invent figures, and flag uncertainty.",
        messages = list(list(role = "user", content = prompt))
      )) |>
      httr2::req_timeout(300) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp)
    if (httr2::resp_status(resp) >= 400) stop(paste("Claude API error:", body$error$message %||% httr2::resp_status(resp)))
    return(paste(vapply(body$content, function(b) b$text %||% "", ""), collapse = "\n"))
  }
  if (mode == "ollama") {
    req <- httr2::request(paste0(settings$ollama_url, "/api/generate")) |>
      httr2::req_body_json(list(model = settings$ollama_model, prompt = prompt, stream = FALSE)) |>
      httr2::req_timeout(900) |>
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- tryCatch(httr2::req_perform(req), error = function(e)
      stop("Could not reach Ollama at ", settings$ollama_url, ". Is Ollama running? (", conditionMessage(e), ")"))
    body <- httr2::resp_body_json(resp)
    if (!is.null(body$error)) stop("Ollama error: ", body$error)
    return(body$response %||% "")
  }
  stop("This mode does not draft inside the app.")
}

# Pull the model's questions out of a draft
extract_ai_questions <- function(txt) {
  if (!nzchar(txt)) return(character(0))
  m <- regexpr("QUESTIONS FOR THE RESEARCHER:", txt, fixed = TRUE)
  if (m < 0) return(character(0))
  tail_txt <- substr(txt, m + attr(m, "match.length"), nchar(txt))
  if (grepl("^\\s*none", tail_txt, ignore.case = TRUE)) return(character(0))
  lines <- trimws(strsplit(tail_txt, "\n")[[1]])
  lines <- lines[grepl("^([0-9]+[.)]|[-*])\\s+", lines)]
  q <- sub("^([0-9]+[.)]|[-*])\\s+", "", lines)
  head(q[nzchar(q)], 5)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
