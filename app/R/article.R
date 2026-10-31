# ANCHOR Studio - reading the uploaded article

read_article <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  txt <- switch(ext,
    pdf = paste(pdftools::pdf_text(path), collapse = "\n\f\n"),
    txt = paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    docx = {
      if (!requireNamespace("officer", quietly = TRUE)) stop("Reading .docx needs the 'officer' package.")
      d <- officer::docx_summary(officer::read_docx(path))
      paste(d$text[!is.na(d$text)], collapse = "\n")
    },
    stop("Please upload the article as PDF, DOCX or TXT.")
  )
  info <- list(file = name, text = txt, pages = NA_integer_)
  if (ext == "pdf") {
    info$pages <- tryCatch(pdftools::pdf_info(path)$pages, error = function(e) NA_integer_)
    meta <- tryCatch(pdftools::pdf_info(path)$keys, error = function(e) list())
    info$meta_title <- meta$Title %||% ""
  }
  info$doi <- detect_doi(txt)
  info$title_guess <- guess_title(txt, info$meta_title %||% "")
  info$abstract <- extract_abstract(txt)
  info$references <- extract_references(txt)
  info$words <- length(strsplit(txt, "\\s+")[[1]])
  info$scanned <- nchar(gsub("\\s", "", txt)) < 500
  info
}

detect_doi <- function(txt) {
  txt <- gsub("\\x{2010}|\\x{2011}|\\x{2012}|\\x{2013}|\\x{2212}", "-", txt, perl = TRUE)  # PDF dashes inside DOIs
  m <- regmatches(txt, regexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", txt))
  if (length(m)) sub("[.,;)]+$", "", m[1]) else ""
}

guess_title <- function(txt, meta_title) {
  if (nzchar(meta_title) && nchar(meta_title) > 10 && !grepl("^untitled|\\.docx?$|microsoft|_|\\.\\.", meta_title, ignore.case = TRUE)) return(meta_title)
  blocks <- strsplit(substr(txt, 1, 4000), "\n\\s*\n")[[1]]
  for (b in head(blocks, 8)) {
    b1 <- trimws(gsub("\\s*\n\\s*", " ", b))
    if (!nzchar(b1) || grepl("doi|http|\u00a9|copyright|received|accepted|published|licen[cs]e|abstract|^#|\\(\\d{4}\\)\\s*\\d+:", b1, ignore.case = TRUE)) next
    if (grepl("@", b1) || (grepl("&| and ", b1) && grepl("\\d", b1) && nchar(b1) < 120)) next
    if (nchar(b1) >= 20 && nchar(b1) <= 250) return(b1)
  }
  ""
}

extract_abstract <- function(txt) {
  m <- regexpr("(?is)abstract\\s*[:.]?\\s*(.{200,2500}?)(\\n\\s*\\n|keywords|jel|1\\s+introduction|introduction)", txt, perl = TRUE)
  if (m < 0) return("")
  s <- regmatches(txt, m)
  s <- sub("(?is)^abstract\\s*[:.]?\\s*", "", s, perl = TRUE)
  s <- sub("(?is)(\\n\\s*\\n|keywords|jel|1\\s+introduction|introduction)$", "", s, perl = TRUE)
  trimws(gsub("\\s+", " ", s))
}

# Split the reference section into individual references (author-year style)
extract_references <- function(txt) {
  pos <- gregexpr("(?im)^\\s*(references|bibliography|literature cited)\\s*$", txt, perl = TRUE)[[1]]
  if (pos[1] < 0) return(character(0))
  ref <- substr(txt, max(pos), nchar(txt))
  ref <- sub("(?im)^\\s*(references|bibliography|literature cited)\\s*$", "", ref, perl = TRUE)
  ref <- gsub("\f", "\n", ref)
  lines <- trimws(strsplit(ref, "\n")[[1]])
  lines <- lines[nzchar(lines)]
  # a new reference starts with "Surname, X." or "Surname X," pattern
  start <- grepl("^[A-Z][A-Za-z'`\\-]+(,| )\\s*([A-Z]\\.|[A-Z][a-z]+,)", lines)
  if (!any(start)) return(character(0))
  grp <- cumsum(start)
  refs <- tapply(lines, grp, paste, collapse = " ")
  refs <- refs[names(refs) != "0"]
  refs <- gsub("-\\s+([a-z])", "\\1", refs)
  refs <- refs[nchar(refs) > 30 & nchar(refs) < 800 & grepl("(19|20)\\d{2}", refs)]
  unname(refs)
}

# Turn the article's own reference list into a small bibliometrix-style data
# frame (authors, year, title, source), so some analyses can run before any
# database export is available.
refs_to_M <- function(refs) {
  if (!length(refs)) return(NULL)
  rows <- lapply(refs, function(r) {
    r <- gsub("\u2013|\u2014", "-", r)
    yr_m <- regexpr("\\(?(19|20)\\d{2}[a-z]?\\)?", r)
    yr <- regmatches(r, regexpr("(19|20)\\d{2}", r))
    au_part <- substr(r, 1, yr_m - 1)
    # surnames = capitalised words that are not initials or joining words
    words <- regmatches(au_part, gregexpr("[A-Z][a-z'\\-]+(?:[A-Z][a-z]+)?|[A-Z]{2,}(?![a-z.])", au_part, perl = TRUE))[[1]]
    surn <- toupper(setdiff(words, c("And", "Et", "Al", "Eds", "Ed", "Van", "Von", "De", "Der")))
    after <- trimws(sub("^[).,:\\s]+", "", substr(r, yr_m + attr(yr_m, "match.length"), nchar(r))))
    # source = last capitalised phrase before a volume number or page range
    srcm <- regmatches(after, gregexpr("([A-Z][A-Za-z&:'\\- ]{3,}?)(?=,?\\s+\\d+\\s*(\\(|:|,|$))", after, perl = TRUE))[[1]]
    so <- if (length(srcm)) trimws(srcm[length(srcm)]) else ""
    ti <- if (nzchar(so)) trimws(sub(paste0("[.?!]?\\s*", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", so), ".*$"), "", after)) else trimws(sub("([.?!])\\s.*$", "\\1", after))
    if (!nzchar(so)) so <- if (grepl("working paper", after, ignore.case = TRUE)) "WORKING PAPER" else trimws(sub(",.*$", "", sub("^[^.?!]*[.?!]\\s*", "", after)))
    data.frame(AU = paste(head(surn, 10), collapse = ";"), PY = suppressWarnings(as.integer(yr)),
               TI = toupper(ti), SO = toupper(so), DT = "ARTICLE", TC = NA_integer_, DB = "ARTICLE_REFS",
               DE = NA_character_, ID = NA_character_, CR = NA_character_, C1 = NA_character_,
               stringsAsFactors = FALSE)
  })
  M <- do.call(rbind, rows)
  M <- M[!is.na(M$PY), ]
  M$SR <- paste0(sub(";.*$", "", M$AU), ", ", M$PY, ", ", M$SO)
  rownames(M) <- make.unique(M$SR)
  M
}
