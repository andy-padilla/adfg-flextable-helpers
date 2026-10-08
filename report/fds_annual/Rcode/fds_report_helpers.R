# Canonical helper loader for the FDS annual report.
# This file centralizes helper sourcing so report child files can call one
# entrypoint without changing existing helper implementations.

# ---- Consolidated report helpers (migrated from report/Rcode/template_report_helpers.R) ----

prepare_report_cover_docx <- function(title, year, authors, date) {
  invisible(list(title = title, year = year, authors = authors, date = date))
}

adfg_xml_escape <- function(text) {
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text
}

adfg_emit_openxml_block <- function(xml, mark_pagebreak = FALSE) {
  if (isTRUE(mark_pagebreak)) {
    state <- adfg_get_pagebreak_state()
    state$last_was_pagebreak <- TRUE
  } else {
    adfg_reset_pagebreak_state()
  }
  cat("```{=openxml}\n", xml, "\n```\n", sep = "")
}

adfg_get_pagebreak_state <- function() {
  state <- get0(".adfg_pagebreak_state", envir = .GlobalEnv, inherits = FALSE)
  if (is.null(state) || !is.environment(state)) {
    state <- new.env(parent = emptyenv())
    state$last_was_pagebreak <- FALSE
    assign(".adfg_pagebreak_state", state, envir = .GlobalEnv)
  }
  state
}

adfg_reset_pagebreak_state <- function() {
  state <- adfg_get_pagebreak_state()
  state$last_was_pagebreak <- FALSE
  invisible(NULL)
}

adfg_emit_docx_pagebreak_block <- function() {
  if (!identical(knitr::pandoc_to(), "docx")) {
    return(invisible(NULL))
  }

  adfg_emit_openxml_block(
    paste0(
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
      "<w:r><w:br w:type=\"page\"/></w:r>",
      "</w:p>"
    ),
    mark_pagebreak = TRUE
  )
  invisible(NULL)
}

adfg_pagebreak_guardrail <- function() {
  state <- adfg_get_pagebreak_state()
  if (isTRUE(state$last_was_pagebreak)) {
    return(invisible(NULL))
  }

  state$last_was_pagebreak <- TRUE

  if (knitr::is_latex_output()) {
    cat("\n\\newpage\n")
    return(invisible(NULL))
  }
  if (knitr::is_html_output()) {
    cat("\n<div style=\"page-break-after: always;\"></div>\n")
    return(invisible(NULL))
  }
  if (identical(knitr::pandoc_to(), "docx")) {
    adfg_emit_docx_pagebreak_block()
    return(invisible(NULL))
  }

  cat("\n\\newpage\n")
  invisible(NULL)
}

word_pagebreak <- function() {
  adfg_pagebreak_guardrail()
}

adfg_section_title_page <- function(title) {
  title <- as.character(title)
  if (!identical(knitr::pandoc_to(), "docx")) {
    cat(paste0("\n\n# ", title, "\n\n"))
    return(invisible(NULL))
  }

  text_xml <- adfg_xml_escape(title)
  adfg_emit_openxml_block(
    paste0(
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:pPr><w:pStyle w:val=\"Normal\"/></w:pPr><w:r><w:t xml:space=\"preserve\"> </w:t></w:r></w:p>",
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:pPr><w:pStyle w:val=\"Heading1\"/><w:spacing w:before=\"4000\"/></w:pPr><w:r><w:t xml:space=\"preserve\">",
      text_xml,
      "</w:t><w:br w:type=\"page\"/></w:r></w:p>"
    )
  )
  invisible(NULL)
}

adfg_normalize_year_ranges <- function(text) {
  text <- as.character(text)
  text <- gsub("([0-9]{4})-([0-9]{4})", "\\1–\\2", text, perl = TRUE)
  text <- gsub("([0-9]{4})-([Pp]resent)", "\\1–\\2", text, perl = TRUE)
  text
}

adfg_emit_seq_caption <- function(seq_id, seq_num, prefix_label, caption_text, style = "Caption") {
  prefix_xml <- adfg_xml_escape(prefix_label)
  caption_xml <- adfg_xml_escape(adfg_normalize_year_ranges(caption_text))
  style_xml <- adfg_xml_escape(style)
  seq_xml <- adfg_xml_escape(seq_id)

  seq_instr <- if (!is.na(suppressWarnings(as.integer(seq_num)))) {
    paste0(" SEQ ", seq_xml, " \\* ARABIC \\r ", as.integer(seq_num), " ")
  } else {
    paste0(" SEQ ", seq_xml, " \\* ARABIC ")
  }

  cat(
    "```{=openxml}\n",
    "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
    "<w:pPr><w:pStyle w:val=\"", style_xml, "\"/></w:pPr>",
    "<w:r><w:t xml:space=\"preserve\">", prefix_xml, "</w:t></w:r>",
    "<w:r><w:fldChar w:fldCharType=\"begin\"/></w:r>",
    "<w:r><w:instrText xml:space=\"preserve\">", seq_instr, "</w:instrText></w:r>",
    "<w:r><w:fldChar w:fldCharType=\"separate\"/></w:r>",
    "<w:r><w:t>", as.character(ifelse(is.na(suppressWarnings(as.integer(seq_num))), "1", as.integer(seq_num))), "</w:t></w:r>",
    "<w:r><w:fldChar w:fldCharType=\"end\"/></w:r>",
    "<w:r><w:t xml:space=\"preserve\">.\u2013", caption_xml, "</w:t></w:r>",
    "</w:p>\n",
    "```\n\n",
    sep = ""
  )
  invisible(NULL)
}

adfg_table_caption <- function(prefix, caption_text) {
  prefix_str <- as.character(prefix)
  caption_text <- adfg_normalize_year_ranges(caption_text)
  if (grepl("^[A-Za-z]", prefix_str)) {
    parts <- strsplit(prefix_str, "-", fixed = TRUE)[[1]]
    appendix_letter <- if (length(parts) > 0) parts[[1]] else "A"
    appendix_num <- suppressWarnings(as.integer(if (length(parts) > 1) parts[[2]] else NA_integer_))
    appendix_label <- paste0("Appendix ", appendix_letter)
    if (identical(knitr::pandoc_to(), "docx")) {
      adfg_emit_seq_caption(
        seq_id = "Appendix_A",
        seq_num = appendix_num,
        prefix_label = appendix_label,
        caption_text = caption_text,
        style = "Caption"
      )
    } else {
      cat(paste0(appendix_label, ".\u2013", caption_text, "\n\n"))
    }
    return(invisible(NULL))
  }
  paste0("Table ", prefix_str, ".\u2013", caption_text)
}

adfg_figure_caption <- function(prefix, caption_text) {
  prefix_str <- as.character(prefix)
  caption_text <- adfg_normalize_year_ranges(caption_text)
  caption_text <- sub("^[[:space:][:punct:]]*[\u2013\u2014-]+[[:space:]]*", "", caption_text, perl = TRUE)
  if (grepl("^[0-9]+$", prefix_str)) {
    return(paste0("Figure ", prefix_str, ".\u2013", caption_text))
  }
  num_fmt <- gsub("-", "–", prefix_str, fixed = TRUE)
  paste0("Appendix ", num_fmt, ".\u2013", caption_text)
}

adfg_figure_caption_block <- function(prefix, caption_text) {
  prefix_str <- as.character(prefix)
  caption_text <- adfg_normalize_year_ranges(caption_text)

  if (grepl("^[0-9]+$", prefix_str)) {
    caption_clean <- as.character(caption_text)
    caption_clean <- sub(
      paste0("^Figure(?:\\s+|–)", prefix_str, "(?:\\s*[–—]|\\.)?\\s*"),
      "",
      caption_clean,
      perl = TRUE
    )
    caption_clean <- sub("^[[:space:][:punct:]]*[\u2013\u2014-]+[[:space:]]*", "", caption_clean, perl = TRUE)

    if (identical(knitr::pandoc_to(), "docx")) {
      adfg_emit_seq_caption(
        seq_id = "Figure",
        seq_num = suppressWarnings(as.integer(prefix_str)),
        prefix_label = "Figure ",
        caption_text = caption_clean,
        style = "Caption"
      )
    } else {
      cat(adfg_figure_caption(prefix_str, caption_clean), "\n\n", sep = "")
    }
    return(invisible(NULL))
  }

  if (grepl("^[A-Za-z]", prefix_str)) {
    parts <- strsplit(prefix_str, "-", fixed = TRUE)[[1]]
    appendix_letter <- if (length(parts) > 0) parts[[1]] else "A"
    appendix_num <- suppressWarnings(as.integer(if (length(parts) > 1) parts[[2]] else NA_integer_))
    appendix_label <- paste0("Appendix ", appendix_letter)
    if (identical(knitr::pandoc_to(), "docx")) {
      adfg_emit_seq_caption(
        seq_id = "Appendix_A",
        seq_num = appendix_num,
        prefix_label = appendix_label,
        caption_text = caption_text,
        style = "Caption"
      )
    } else {
      cat(paste0(appendix_label, ".\u2013", caption_text, "\n\n"))
    }
    return(invisible(NULL))
  }

  cat(adfg_figure_caption(prefix_str, caption_text), "\n\n", sep = "")
  invisible(NULL)
}

adfg_output_appendix_caption <- function(prefix, caption_text) {
  adfg_figure_caption_block(prefix, caption_text)
}

adfg_appendix_caption_from_text <- function(text) {
  line <- adfg_normalize_year_ranges(as.character(text))
  m <- regexec("^\\s*Appendix\\s+([A-Za-z])\\s*([0-9]+)\\.?[-–]?\\s*(.*)$", line, perl = TRUE)
  parts <- regmatches(line, m)[[1]]
  if (length(parts) == 4) {
    prefix <- paste0(parts[[2]], "-", parts[[3]])
    caption <- trimws(parts[[4]])
    return(adfg_figure_caption_block(prefix, caption))
  }

  cat("::: {custom-style=\"Caption\"}\n", line, "\n:::\n\n", sep = "")
  invisible(NULL)
}

adfg_output_continuation_caption <- function(prefix, caption_text = NULL) {
  prefix_str <- as.character(prefix)
  num_fmt <- gsub("-", "–", prefix_str, fixed = TRUE)
  if (is.null(caption_text)) {
    caption_text <- "continued"
  }
  caption_text <- adfg_normalize_year_ranges(caption_text)
  cat("::: {custom-style=\"Caption\"}\nAppendix ", num_fmt, ".\u2013", caption_text, "\n:::\n\n", sep = "")
  invisible(NULL)
}

adfg_emit_table_continued <- function(text = "-continued-", pagebreak = FALSE) {
  if (isTRUE(pagebreak) && identical(knitr::pandoc_to(), "docx")) {
    adfg_emit_openxml_block(
      paste0(
        "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
        "<w:pPr><w:pStyle w:val=\"Table-Continued\"/></w:pPr>",
        "<w:r><w:t xml:space=\"preserve\">", adfg_xml_escape(text), "</w:t>",
        "<w:br w:type=\"page\"/></w:r></w:p>"
      ),
      mark_pagebreak = TRUE
    )
    return(invisible(NULL))
  }
  cat("::: {custom-style=\"Table-Continued\"}\n", text, "\n:::\n\n", sep = "")
  if (isTRUE(pagebreak)) word_pagebreak()
  invisible(NULL)
}

adfg_table_emit_continued_rule <- function(has_split,
                                           table_num,
                                           action = c("marker", "pagebreak", "marker_pagebreak", "caption"),
                                           page = 2,
                                           total_pages = 2,
                                           marker_text = "-continued-",
                                           marker_style = "Table-Continued",
                                           caption_style = "Caption",
                                           caption_bottom_border = FALSE) {
  if (!isTRUE(has_split)) return(invisible(FALSE))

  action <- match.arg(action)
  style_fn <- get0("adfg_style_paragraph", mode = "function", inherits = TRUE)
  pagebreak_fn <- get0("word_pagebreak", mode = "function", inherits = TRUE)

  if (is.null(style_fn)) {
    style_fn <- function(text, style = "Normal") {
      cat(paste0(text, "\n\n"))
    }
  }
  if (is.null(pagebreak_fn)) {
    pagebreak_fn <- function() {
      cat("\n\n")
    }
  }

  # Keep split-table flow in the same section so orientation is preserved.
  in_section_pagebreak_fn <- function() {
    if (identical(knitr::pandoc_to(), "docx")) {
      adfg_emit_docx_pagebreak_block()
      return(invisible(NULL))
    }
    pagebreak_fn()
  }

  emit_helper_result <- function(x) {
    if (inherits(x, "knit_asis")) {
      cat(as.character(x), sep = "")
    } else if (is.character(x) && length(x) > 0) {
      cat(x, sep = "")
    }
    invisible(NULL)
  }

  if (identical(action, "marker")) {
    emit_helper_result(style_fn(marker_text, marker_style))
    return(invisible(TRUE))
  }

  if (identical(action, "marker_pagebreak")) {
    if (identical(knitr::pandoc_to(), "docx")) {
      cat(
        "```{=openxml}\n",
        "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
        "<w:pPr><w:pStyle w:val=\"", adfg_xml_escape(marker_style), "\"/></w:pPr>",
        "<w:r><w:t xml:space=\"preserve\">", adfg_xml_escape(marker_text), "</w:t>",
        "<w:br w:type=\"page\"/></w:r></w:p>\n",
        "```\n\n",
        sep = ""
      )
    } else {
      emit_helper_result(style_fn(marker_text, marker_style))
      emit_helper_result(in_section_pagebreak_fn())
    }
    return(invisible(TRUE))
  }

  if (identical(action, "pagebreak")) {
    emit_helper_result(in_section_pagebreak_fn())
    return(invisible(TRUE))
  }

  caption_prefix <- if (grepl("^Appendix\\s", table_num)) "" else "Table "
  caption_text <- paste0(caption_prefix, table_num, ".–Page ", page, " of ", total_pages, ".")
  if (isTRUE(caption_bottom_border) && identical(knitr::pandoc_to(), "docx")) {
    cat(
      "```{=openxml}\n",
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
      "<w:pPr><w:pStyle w:val=\"", adfg_xml_escape(caption_style), "\"/>",
      "<w:pBdr><w:bottom w:val=\"single\" w:sz=\"8\" w:space=\"1\" w:color=\"auto\"/></w:pBdr></w:pPr>",
      "<w:r><w:t xml:space=\"preserve\">", adfg_xml_escape(caption_text), "</w:t></w:r></w:p>\n",
      "```\n\n",
      sep = ""
    )
    return(invisible(TRUE))
  }

  emit_helper_result(style_fn(
    caption_text,
    caption_style
  ))
  invisible(TRUE)
}

adfg_theme <- function(ft) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    return(ft)
  }

  ft |>
    flextable::theme_booktabs() |>
    flextable::autofit()
}

adfg_inline_italic <- function(text) {
  paste0("*", as.character(text), "*")
}

word_pagebreak <- function() {
  if (isTRUE(knitr::is_html_output())) {
    return(invisible(NULL))
  }
  if (identical(knitr::pandoc_to(), "docx")) {
    return(knitr::asis_output(paste0(
      "```{=openxml}\n",
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:r><w:br w:type=\"page\"/></w:r></w:p>\n",
      "```\n\n"
    )))
  }

  knitr::asis_output("\\newpage\n")
}

if (!exists(".fds_helper_state", envir = .GlobalEnv, inherits = FALSE)) {
  .fds_helper_state <- new.env(parent = emptyenv())
  .fds_helper_state$loaded_paths <- character(0)
  assign(".fds_helper_state", .fds_helper_state, envir = .GlobalEnv)
}

fds_locate_repo_root <- function(start = getwd()) {
  cur <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    marker <- file.path(cur, "report", "fds_annual", "FDS_subsistence.Rmd")
    if (file.exists(marker)) {
      return(cur)
    }

    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  stop(
    "Unable to locate repository root for FDS annual report helper loading.",
    call. = FALSE
  )
}

fds_source_once <- function(repo_root, rel_path, envir = parent.frame(), strict = TRUE, verbose = FALSE) {
  target <- normalizePath(file.path(repo_root, rel_path), winslash = "/", mustWork = FALSE)
  if (!file.exists(target)) {
    msg <- paste0("Required helper file not found: ", rel_path)
    if (isTRUE(strict)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
    return(invisible(FALSE))
  }

  loaded <- get(".fds_helper_state", envir = .GlobalEnv, inherits = FALSE)$loaded_paths
  if (target %in% loaded) {
    return(invisible(TRUE))
  }

  if (isTRUE(verbose)) {
    message("Sourcing helper: ", target)
  }

  source(target, local = envir)

  st <- get(".fds_helper_state", envir = .GlobalEnv, inherits = FALSE)
  st$loaded_paths <- c(st$loaded_paths, target)
  assign(".fds_helper_state", st, envir = .GlobalEnv)

  invisible(TRUE)
}

fds_source_abs_once <- function(source_path, envir = parent.frame(), strict = TRUE, verbose = FALSE) {
  target <- normalizePath(source_path, winslash = "/", mustWork = FALSE)
  if (!file.exists(target)) {
    msg <- paste0("Required helper file not found: ", source_path)
    if (isTRUE(strict)) {
      stop(msg, call. = FALSE)
    }
    warning(msg, call. = FALSE)
    return(invisible(FALSE))
  }

  loaded <- get(".fds_helper_state", envir = .GlobalEnv, inherits = FALSE)$loaded_paths
  if (target %in% loaded) {
    return(invisible(TRUE))
  }

  if (isTRUE(verbose)) {
    message("Sourcing helper: ", target)
  }

  source(target, local = envir)

  st <- get(".fds_helper_state", envir = .GlobalEnv, inherits = FALSE)
  st$loaded_paths <- c(st$loaded_paths, target)
  assign(".fds_helper_state", st, envir = .GlobalEnv)

  invisible(TRUE)
}

fds_load_report_helpers <- function(load_template = TRUE,
                                    load_flextable = TRUE,
                                    load_figures = FALSE,
                                    template_helpers_path = NULL,
                                    strict = TRUE,
                                    verbose = FALSE) {
  repo_root <- fds_locate_repo_root()
  options(adfg.repo_root = repo_root)

  if (isTRUE(load_template)) {
    if (!is.null(template_helpers_path) && nzchar(as.character(template_helpers_path))) {
      fds_source_abs_once(
        source_path = as.character(template_helpers_path),
        envir = parent.frame(),
        strict = strict,
        verbose = verbose
      )
    }
  }

  if (isTRUE(load_flextable)) {
    fds_source_once(
      repo_root,
      rel_path = file.path("functions", "adfg_flextable_helpers.R"),
      envir = parent.frame(),
      strict = strict,
      verbose = verbose
    )
  }

  if (isTRUE(load_figures)) {
    figure_pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "scales", "viridis", "here")
    missing_pkgs <- figure_pkgs[!vapply(figure_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_pkgs) > 0) {
      stop(
        paste0("Missing required package(s) for figure helpers: ", paste(missing_pkgs, collapse = ", ")),
        call. = FALSE
      )
    }

    for (pkg in figure_pkgs) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }

    fds_source_once(
      repo_root,
      rel_path = file.path("report", "Rcode", "figure_builders", "fds_report_figures.R"),
      envir = parent.frame(),
      strict = strict,
      verbose = verbose
    )
  }

  invisible(repo_root)
}
