# ANCHOR Studio - phase definitions
# Each phase: what it needs, what it produces, the checks for its gate,
# the questions the researcher must answer before the gate opens, and the
# AI prompt used to draft it.

PHASES <- list(
  list(
    id = "A", name = "Anchor the article",
    question = "What does the article claim, and how?",
    input = "The uploaded article (PDF) and your set-up answers.",
    actions = "Extract question, theory, hypotheses, section headings, data, sample, models, variables and key estimates.",
    output = "Article-at-a-glance table and a section-by-section summary.",
    controls = c("Q1", "Q2"),
    checks = c(
      "Article-at-a-glance table and section summary are complete",
      "Every figure was checked against the uploaded file",
      "GATE: version of the article stated; no number taken from a press summary"
    ),
    questions = list(
      list(id = "A_version_ok", label = "Is the uploaded file the published version of record?", type = "radio",
           choices = c("Yes, version of record", "No, a working paper or preprint (figures must be rechecked later)")),
      list(id = "A_focus", label = "Which parts of the article matter most for your own project?", type = "text"),
      list(id = "A_confirm", label = "Did you compare the key numbers in the draft with the tables in the PDF?", type = "radio",
           choices = c("Yes, all key numbers", "Partly; open items noted below"))
    ),
    prompt = paste(
      "You are assisting a researcher with phase A (Anchor the article) of the ANCHOR framework.",
      "Using ONLY the article text supplied, extract: (1) citation and DOI; (2) research question(s);",
      "(3) theory and theoretical lenses; (4) every hypothesis, numbered as in the article; (5) section headings;",
      "(6) data sources, sample period, sample size and unit of analysis; (7) models or equations and the coefficient",
      "that tests each hypothesis; (8) dependent, independent, moderating and control variables with definitions;",
      "(9) key estimates with t-statistics or p-values and the table they come from; (10) the authors' stated limitations.",
      "Give an 'Article at a glance' table first, then a section-by-section summary. Quote table numbers.",
      "Never invent a number: write 'not reported' when the text does not give it.",
      sep = " ")
  ),
  list(
    id = "N", name = "Navigate the literature",
    question = "Which studies surround it?",
    input = "Phase A summary, the article's reference list, and your database exports.",
    actions = "PRISMA 2020 protocol, Boolean search string, database exports (Scopus, Web of Science, ...), de-duplication, screening.",
    output = "Protocol, eligibility table, PRISMA flow with real counts, core corpus.",
    controls = c("Q3"),
    checks = c(
      "Protocol, search string and eligibility table are written",
      "Database exports imported in the Bibliometrics tab (or explicitly postponed)",
      "GATE: PRISMA counts are real (from the imported data) or marked 'pending', never estimated"
    ),
    questions = list(
      list(id = "N_db", label = "Which databases will you search?", type = "checkbox",
           choices = c("Scopus", "Web of Science", "Dimensions", "PubMed", "Lens", "OpenAlex", "Other")),
      list(id = "N_window", label = "Publication window (e.g. 2000-2026)", type = "text"),
      list(id = "N_types", label = "Document types to include", type = "checkbox",
           choices = c("Journal articles", "Reviews", "Conference papers", "Book chapters", "Working papers")),
      list(id = "N_lang", label = "Languages to include", type = "text"),
      list(id = "N_criteria", label = "Inclusion and exclusion criteria (one per line)", type = "textarea"),
      list(id = "N_fallback", label = "If database access is not available yet, how should we proceed?", type = "radio",
           choices = c("Wait: do not pass this gate without exports", "Continue with citation snowballing and mark counts pending"))
    ),
    prompt = paste(
      "Phase N (Navigate the literature) of the ANCHOR framework.",
      "Based on the focal article summary below, write a PRISMA 2020 review protocol: review question, databases,",
      "a Boolean search string for Scopus (TITLE-ABS-KEY) and one for Web of Science (TS=), inclusion and exclusion",
      "criteria as a table, and a screening plan. Then list candidate core studies from the article's reference list",
      "(backward snowballing) with a one-line reason each. Do NOT estimate database hit counts: write 'pending'.",
      "Respect the researcher's answers given below.",
      sep = " ")
  ),
  list(
    id = "C", name = "Code and synthesise",
    question = "What does the evidence show?",
    input = "Core corpus and the bibliometric results.",
    actions = "Code each study (outlet, stream, theory, data, method, finding); appraise quality 0-2 on five criteria; synthesise by stream; read the bibliometric maps.",
    output = "Extraction matrix, quality profile, synthesis by stream, theory table, selected bibliometric figures.",
    controls = c("Q4", "Q6"),
    checks = c(
      "Extraction matrix and quality scores are complete",
      "Bibliometric figures chosen for the report (Bibliometrics tab, 'Add to report')",
      "GATE: every claim points to a coded study; research streams do not overlap"
    ),
    questions = list(
      list(id = "C_quality", label = "Quality appraisal scheme", type = "radio",
           choices = c("0-2 on theory, construct validity, identification, robustness, transparency", "Journal ranking only", "Other (describe in notes)")),
      list(id = "C_organise", label = "Organise the synthesis by", type = "radio",
           choices = c("Research stream", "Theory", "Method", "Chronology")),
      list(id = "C_streams", label = "Streams or themes you already expect (one per line; leave empty to let the maps decide)", type = "textarea")
    ),
    prompt = paste(
      "Phase C (Code and synthesise) of the ANCHOR framework.",
      "Using the focal article summary, the protocol and the bibliometric summary below, produce: (1) an extraction",
      "matrix (study, outlet, stream, theory, data and sample, method, key finding); (2) a quality appraisal 0-2 on the",
      "researcher's chosen scheme; (3) a synthesis organised as the researcher chose; (4) a table mapping each theory to",
      "its prediction for the focal setting; (5) a short reading of the bibliometric results (most productive sources,",
      "authors, themes and clusters). Only include studies you can name with author and year; mark uncertain items.",
      sep = " ")
  ),
  list(
    id = "H", name = "Hypothesise gaps",
    question = "What should be tested next?",
    input = "Synthesis and the focal article's limitations.",
    actions = "Classify gaps by type; turn each into a direction and a testable hypothesis with an expected sign.",
    output = "Gap table, research directions, numbered hypotheses.",
    controls = c("Q6"),
    checks = c(
      "Gap table with gap types is complete",
      "Each direction has a testable hypothesis with an expected sign",
      "GATE: every hypothesis names its gap, theory and direction; numbering does not clash with the article's"
    ),
    questions = list(
      list(id = "H_types", label = "Gap types to prioritise", type = "checkbox",
           choices = c("Data", "Temporal", "Contextual", "Conceptual", "Theoretical", "Methodological", "Outcome", "Identification")),
      list(id = "H_n", label = "How many future hypotheses do you want?", type = "radio", choices = c("3", "5", "6", "8", "10")),
      list(id = "H_prefix", label = "Label for your hypotheses (to avoid clashing with the article's)", type = "radio",
           choices = c("F1, F2, ...", "RH1, RH2, ...", "P1, P2, ..."))
    ),
    prompt = paste(
      "Phase H (Hypothesise gaps) of the ANCHOR framework.",
      "Identify research gaps in the focal article and the synthesis, classified by the gap types the researcher",
      "prioritised. For each gap give: gap type, evidence for it, a research direction, and a testable hypothesis with",
      "its expected sign and theoretical basis. Produce exactly the number of hypotheses requested and use the",
      "researcher's label prefix. Clearly mark that these are the researcher's proposals, not the authors'.",
      sep = " ")
  ),
  list(
    id = "O", name = "Operationalise the design",
    question = "How, and with which variables?",
    input = "Your hypotheses.",
    actions = "Paradigm, sample, data sources, equations, identification strategy, robustness tests, variable table with precedents, conceptual framework.",
    output = "Design section, equations, variable table, framework diagram.",
    controls = c("Q4"),
    checks = c(
      "Sample, data sources and equations are written",
      "Variable table with a precedent for every variable",
      "GATE: every hypothesis maps to one coefficient; every variable has a source in the literature"
    ),
    questions = list(
      list(id = "O_paradigm", label = "Research paradigm", type = "radio",
           choices = c("Quantitative archival", "Experimental", "Survey", "Qualitative", "Mixed methods")),
      list(id = "O_setting", label = "Setting, sample and period you can actually access", type = "textarea"),
      list(id = "O_ident", label = "Preferred identification strategy", type = "radio",
           choices = c("Fixed-effects panel", "Difference-in-differences", "Instrumental variables", "Event study", "Regression discontinuity", "Let the draft propose"))
    ),
    prompt = paste(
      "Phase O (Operationalise the design) of the ANCHOR framework.",
      "Design research that tests the approved hypotheses in the researcher's setting: paradigm, sample, data sources,",
      "regression equations (one coefficient per hypothesis, with expected sign), identification strategy, robustness",
      "tests, and a variable table (role, name, definition, measure, data source, precedent study). Describe the",
      "conceptual framework as boxes and arrows in text.",
      sep = " ")
  ),
  list(
    id = "R", name = "Report and record",
    question = "How is it shared and disclosed?",
    input = "All approved phases, bibliometric figures and the decision log.",
    actions = "Assemble the report, AI-use disclosure, APA 7 references and the decision log; export.",
    output = "Report (HTML and Word), bibliometric figures, prompt and decision log.",
    controls = c("Q5", "Q6", "Q7", "Q8"),
    checks = c(
      "Report previewed and exported",
      "AI-use disclosure reviewed",
      "GATE: figures agree across outputs and with the source; you sign off as author"
    ),
    questions = list(
      list(id = "R_audience", label = "Main audience of the report", type = "radio",
           choices = c("Specialists (supervisor, reviewers)", "Non-specialists (students, practitioners)")),
      list(id = "R_outputs", label = "Outputs to export", type = "checkbox",
           choices = c("Report (HTML)", "Report (Word)", "Bibliometric data (Excel, Biblioshiny-compatible)", "Project file")),
      list(id = "R_signoff", label = "Your sign-off statement (name and date)", type = "text")
    ),
    prompt = paste(
      "Phase R (Report and record) of the ANCHOR framework.",
      "Write an executive summary (250 words) of the whole review for the stated audience, followed by a list of the",
      "five most important take-aways and a short limitations paragraph. Use only the approved phase outputs below.",
      sep = " ")
  )
)
names(PHASES) <- vapply(PHASES, `[[`, "", "id")

CONTROLS <- c(
  Q1 = "Primary-source numbers: take every statistic from the published article; recheck working-paper figures.",
  Q2 = "Version labelling: state whether the published or a working-paper version was used.",
  Q3 = "No invented counts: database counts are real or marked 'pending'.",
  Q4 = "Point-in-time data: prefer data as available at the time; note revisions.",
  Q5 = "Cross-output consistency: after a correction, update every output that repeats it.",
  Q6 = "Paraphrase and cite: new words; at most one short quote per source; cite every claim.",
  Q7 = "AI-use disclosure: prompts verbatim; tool and version named; APA 7 citation.",
  Q8 = "Human sign-off: you check citations and figures; responsibility stays with you."
)

SETUP_QUESTIONS <- list(
  list(id = "S_name", label = "Your name", type = "text"),
  list(id = "S_inst", label = "Institution or course", type = "text"),
  list(id = "S_purpose", label = "Purpose of this review", type = "radio",
       choices = c("Thesis or seminar paper", "Research proposal", "Journal article", "Teaching material")),
  list(id = "S_policy", label = "Have you read your institution's rules on using generative AI?", type = "radio",
       choices = c("Yes", "Not yet (please do before submitting)"))
)

# Instruction appended to every AI draft so the app can show the model's own
# questions to the researcher before the gate.
QUESTION_TAIL <- paste(
  "\n\nEnd your answer with a section that starts exactly with the line 'QUESTIONS FOR THE RESEARCHER:'",
  "followed by up to five numbered questions you need the researcher to decide or confirm before this phase can be",
  "approved. If you have none, write 'QUESTIONS FOR THE RESEARCHER: none'."
)
