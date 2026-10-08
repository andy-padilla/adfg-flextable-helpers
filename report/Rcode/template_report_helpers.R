# Minimal helper set used by the report template.

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

adfg_normalize_year_ranges <- function(text) {
	text <- as.character(text)
	text <- gsub("([0-9]{4})-([0-9]{4})", "\\1\u2013\\2", text, perl = TRUE)
	text <- gsub("([0-9]{4})-([Pp]resent)", "\\1\u2013\\2", text, perl = TRUE)
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
		"<w:r><w:t xml:space=\"preserve\">.–", caption_xml, "</w:t></w:r>",
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
		# Appendix table: emit a SEQ caption so List of Appendices fields can index it.
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
			cat(paste0(appendix_label, ".–", caption_text, "\n\n"))
		}
		return(invisible(NULL))
	}
	# Regular table: return string for use in flextable::set_caption() etc.
	paste0("Table ", prefix_str, ".–", caption_text)
}

adfg_figure_caption <- function(prefix, caption_text) {
	prefix_str <- as.character(prefix)
	caption_text <- adfg_normalize_year_ranges(caption_text)
	# Guardrail: if caller text already starts with dash punctuation, remove it.
	caption_text <- sub("^[[:space:][:punct:]]*[\u2013\u2014-]+[[:space:]]*", "", caption_text, perl = TRUE)
	if (grepl("^[0-9]+$", prefix_str)) {
		return(paste0("Figure ", prefix_str, ".–", caption_text))
	}
	# Appendix figure label text
	num_fmt <- gsub("-", "\u2013", prefix_str, fixed = TRUE)
	paste0("Appendix ", num_fmt, ".–", caption_text)
}

# Figure caption emitter that mirrors adfg_table_caption() call style.
adfg_figure_caption_block <- function(prefix, caption_text) {
	prefix_str <- as.character(prefix)
	caption_text <- adfg_normalize_year_ranges(caption_text)

	if (grepl("^[0-9]+$", prefix_str)) {
		caption_clean <- as.character(caption_text)
		caption_clean <- sub(
			paste0("^Figure(?:\\s+|\u2013)", prefix_str, "(?:\\s*[\u2013\u2014]|\\.)?\\s*"),
			"",
			caption_clean,
			perl = TRUE
		)
		# Guardrail: remove any leftover leading dash punctuation so we never get ".--" in caption output.
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
			cat(paste0(appendix_label, ".–", caption_text, "\n\n"))
		}
		return(invisible(NULL))
	}

	cat(adfg_figure_caption(prefix_str, caption_text), "\n\n", sep = "")
	invisible(NULL)
}

# Output appendix caption as separate block (use in results='asis' chunk BEFORE table/figure)
adfg_output_appendix_caption <- function(prefix, caption_text) {
	adfg_figure_caption_block(prefix, caption_text)
}

adfg_appendix_caption_from_text <- function(text) {
	line <- adfg_normalize_year_ranges(as.character(text))
	m <- regexec("^\\s*Appendix\\s+([A-Za-z])\\s*([0-9]+)\\.?[\u2013-]?\\s*(.*)$", line, perl = TRUE)
	parts <- regmatches(line, m)[[1]]
	if (length(parts) == 4) {
		prefix <- paste0(parts[[2]], "-", parts[[3]])
		caption <- trimws(parts[[4]])
		return(adfg_figure_caption_block(prefix, caption))
	}

	cat("::: {custom-style=\"Caption\"}\n", line, "\n:::\n\n", sep = "")
	invisible(NULL)
}

# Output continuation caption for split tables
adfg_output_continuation_caption <- function(prefix, caption_text = NULL) {
	prefix_str <- as.character(prefix)
	num_fmt <- gsub("-", "\u2013", prefix_str, fixed = TRUE)
	if (is.null(caption_text)) {
		caption_text <- "continued"
	}
	caption_text <- adfg_normalize_year_ranges(caption_text)
	cat("::: {custom-style=\"Caption\"}\nAppendix ", num_fmt, ".–", caption_text, "\n:::\n\n", sep = "")
	invisible(NULL)
}

adfg_emit_table_continued <- function(text = "-continued-") {
	cat("::: {custom-style=\"Table-Continued\"}\n", text, "\n:::\n\n", sep = "")
	invisible(NULL)
}

adfg_theme <- function(ft) {
	if (!requireNamespace("flextable", quietly = TRUE)) {
		return(ft)
	}

	ft |>
		flextable::theme_booktabs() |>
		flextable::autofit()
}

word_pagebreak <- function() {
	if (isTRUE(knitr::is_html_output())) {
		return(invisible(NULL))
	}
	if (identical(knitr::pandoc_to(), "docx")) {
		return(knitr::asis_output(paste0(
			"```{=openxml}\n",
			"<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\"><w:pPr><w:sectPr><w:type w:val=\"nextPage\"/></w:sectPr></w:pPr></w:p>\n",
			"```\n\n"
		)))
	}

	knitr::asis_output("\\newpage\n")
}
