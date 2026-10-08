adfg_ft_rule_border <- function(width = 0.5) {
  officer::fp_border(width = width)
}

adfg_table_footnote_style <- function() {
  # Centralized style name so all table note/footnote sections stay consistent.
  getOption("adfg.table_footnote_style", "Table-Footnote")
}

adfg_sentence_case <- function(text,
                               proper_nouns = character(0),
                               preserve_tokens = c("CI", "NA", "ND", "FNSB", "ADF&G", "U.S.", "5D", "5D down", "5D up")) {
  if (length(text) == 0) return(text)

  # Escape regex metacharacters safely for dynamic gsub patterns.
  escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\\\1", as.character(x), perl = TRUE)
  }

  proper_nouns <- as.character(proper_nouns)
  proper_nouns <- proper_nouns[!is.na(proper_nouns) & nzchar(trimws(proper_nouns))]
  proper_lookup <- setNames(proper_nouns, tolower(proper_nouns))

  preserve_tokens <- unique(c(as.character(preserve_tokens), proper_nouns))
  preserve_tokens <- preserve_tokens[!is.na(preserve_tokens) & nzchar(trimws(preserve_tokens))]

  fix_line <- function(line_text) {
    if (is.na(line_text) || !nzchar(trimws(line_text))) return(line_text)

    line_text <- as.character(line_text)
    original_tokens <- unlist(strsplit(line_text, "\\s+"), use.names = FALSE)
    original_tokens <- original_tokens[nzchar(original_tokens)]
    original_caps <- original_tokens[grepl("^[A-Z0-9&./-]{2,}$", original_tokens)]

    normalized <- tolower(line_text)
    first_alpha <- regexpr("[a-z]", normalized)
    if (first_alpha[[1]] > 0) {
      pos <- first_alpha[[1]]
      substr(normalized, pos, pos) <- toupper(substr(normalized, pos, pos))
    }

    # Re-apply known proper nouns by exact word-boundary matching.
    if (length(proper_lookup) > 0) {
      for (k in names(proper_lookup)) {
        normalized <- gsub(
          paste0("\\b", escape_regex(k), "\\b"),
          proper_lookup[[k]],
          normalized,
          ignore.case = TRUE,
          perl = TRUE
        )
      }
    }

    # Preserve acronyms/initialisms from configured list and source text.
    keep <- unique(c(preserve_tokens, original_caps))
    keep <- keep[!is.na(keep) & nzchar(keep)]
    if (length(keep) > 0) {
      for (tok in keep) {
        escaped <- escape_regex(tok)
        normalized <- gsub(
          paste0("\\b", tolower(escaped), "\\b"),
          tok,
          normalized,
          ignore.case = TRUE,
          perl = TRUE
        )
      }
    }

    normalized
  }

  vapply(
    as.character(text),
    function(x) {
      lines <- strsplit(x, "\\n", fixed = FALSE)[[1]]
      paste(vapply(lines, fix_line, character(1)), collapse = "\n")
    },
    character(1)
  )
}

adfg_normalize_header_labels <- function(labels,
                                         proper_nouns = character(0),
                                         preserve_tokens = c("CI", "NA", "ND", "FNSB", "ADF&G", "U.S.", "5D", "5D down", "5D up")) {
  if (is.null(labels) || length(labels) == 0) return(labels)

  out <- adfg_sentence_case(
    text = as.character(labels),
    proper_nouns = proper_nouns,
    preserve_tokens = preserve_tokens
  )

  if (!is.null(names(labels))) {
    names(out) <- names(labels)
  }
  out
}

adfg_ft_clamp_font_size <- function(size, min_size = 9, max_size = 10) {
  size <- suppressWarnings(as.numeric(size))
  if (is.na(size)) size <- min_size
  max(min_size, min(max_size, size))
}

adfg_index_to_marker <- function(i) {
  i <- suppressWarnings(as.integer(i))
  if (is.na(i) || i <= 0L) return("")

  alpha <- letters
  out <- ""
  while (i > 0L) {
    rem <- (i - 1L) %% 26L + 1L
    out <- paste0(alpha[[rem]], out)
    i <- (i - rem) %/% 26L
  }
  out
}

adfg_note_registry <- function(start_index = 1L) {
  start_index <- suppressWarnings(as.integer(start_index))
  if (is.na(start_index) || start_index < 1L) start_index <- 1L

  note_env <- new.env(parent = emptyenv())
  note_order <- character(0)

  normalize_note <- function(note) {
    if (length(note) == 0) return(NA_character_)
    n <- trimws(as.character(note[[1]]))
    if (!nzchar(n) || is.na(n)) return(NA_character_)
    gsub("\\s+", " ", n)
  }

  next_available_marker <- function() {
    used <- unname(unlist(as.list(note_env), use.names = FALSE))
    i <- start_index
    repeat {
      m <- adfg_index_to_marker(i)
      if (!(m %in% used)) return(m)
      i <- i + 1L
    }
  }

  register <- function(note, marker = NULL) {
    n <- normalize_note(note)
    if (is.na(n) || !nzchar(n)) return(character(0))

    if (!exists(n, envir = note_env, inherits = FALSE)) {
      m <- if (is.null(marker) || !nzchar(as.character(marker[[1]]))) {
        next_available_marker()
      } else {
        as.character(marker[[1]])
      }
      assign(n, m, envir = note_env)
      note_order <<- c(note_order, n)
    }

    get(n, envir = note_env, inherits = FALSE)
  }

  register_many <- function(notes) {
    if (length(notes) == 0) return(character(0))
    out <- character(0)
    for (note in notes) {
      m <- register(note)
      if (length(m) > 0) out <- c(out, m)
    }
    unique(out)
  }

  marker_for <- function(note) {
    n <- normalize_note(note)
    if (is.na(n) || !exists(n, envir = note_env, inherits = FALSE)) return(NA_character_)
    get(n, envir = note_env, inherits = FALSE)
  }

  marker_map <- function() {
    if (length(note_order) == 0) return(NULL)
    setNames(
      as.list(vapply(note_order, function(n) get(n, envir = note_env, inherits = FALSE), character(1))),
      note_order
    )
  }

  list(
    register = register,
    register_many = register_many,
    marker_for = marker_for,
    note_order = function() note_order,
    marker_map = marker_map
  )
}

adfg_ft_apply_typography_guardrails <- function(ft,
                                                fontname = "Times New Roman",
                                                font_size = 10,
                                                line_space = 1.0,
                                                padding = 2,
                                                min_font_size = 9,
                                                max_font_size = 10) {
  clamped_size <- adfg_ft_clamp_font_size(font_size, min_font_size, max_font_size)
  ft <- flextable::font(ft, fontname = fontname, part = "all")
  ft <- flextable::fontsize(ft, size = clamped_size, part = "all")
  ft <- flextable::line_spacing(ft, space = line_space, part = "all")
  flextable::padding(ft, padding = padding, part = "all")
}

adfg_ft_apply_row_height_guardrails <- function(ft,
                                                body_height = NULL,
                                                header_height = NULL) {
  if (!is.null(body_height)) {
    ft <- flextable::height(ft, height = body_height, part = "body")
  }
  if (!is.null(header_height)) {
    ft <- flextable::height(ft, height = header_height, part = "header")
  }
  ft
}

adfg_ft_fit_within_width_guardrails <- function(ft,
                                                max_total_width = 6.5,
                                                min_scale = 0.5) {
  # Always let Word scale table to available text width on the page.
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)

  dims <- tryCatch(flextable::dim(ft), error = function(e) NULL)
  if (is.null(dims) || is.null(dims$widths)) return(ft)

  widths <- suppressWarnings(as.numeric(dims$widths))
  widths <- widths[is.finite(widths)]
  if (length(widths) == 0) return(ft)

  current_total <- sum(widths)
  if (!is.finite(current_total) || current_total <= 0 || current_total <= max_total_width) {
    return(ft)
  }

  scale_factor <- max(min_scale, max_total_width / current_total)
  flextable::width(ft, j = dims$col_keys, width = widths * scale_factor)
}

adfg_ft_total_width <- function(ft) {
  dims <- tryCatch(flextable::dim(ft), error = function(e) NULL)
  if (is.null(dims) || is.null(dims$widths)) return(NA_real_)

  widths <- suppressWarnings(as.numeric(dims$widths))
  widths <- widths[is.finite(widths)]
  if (length(widths) == 0) return(NA_real_)

  sum(widths)
}

adfg_ft_exceeds_max_width <- function(ft, max_total_width = 6.5) {
  ft_check <- tryCatch(
    flextable::set_table_properties(ft, layout = "autofit", width = 1),
    error = function(e) ft
  )

  total_width <- adfg_ft_total_width(ft_check)
  is.finite(total_width) && total_width > max_total_width
}

adfg_ft_apply_header_alignment_map <- function(ft, alignment_map = list()) {
  if (length(alignment_map) == 0) return(ft)

  for (spec in alignment_map) {
    if (is.null(spec$cols) || is.null(spec$row) || is.null(spec$align)) next
    ft <- flextable::align(
      ft,
      j = spec$cols,
      align = spec$align,
      part = "header",
      i = spec$row
    )
  }

  ft
}

adfg_ft_apply_header_rule_map <- function(ft,
                                          rule_map = list(),
                                          border = adfg_ft_rule_border(),
                                          add_top = TRUE,
                                          add_bottom = TRUE) {
  if (isTRUE(add_top)) {
    ft <- flextable::hline_top(ft, border = border, part = "header")
  }

  if (length(rule_map) > 0) {
    for (spec in rule_map) {
      if (is.null(spec$row) || is.null(spec$cols)) next
      ft <- flextable::hline(
        ft,
        i = spec$row,
        j = spec$cols,
        border = border,
        part = "header"
      )
    }
  }

  if (isTRUE(add_bottom)) {
    ft <- flextable::hline_bottom(ft, border = border, part = "header")
  }

  ft
}

adfg_ft_apply_accessibility <- function(ft,
                                        word_title = "Data table",
                                        word_description = "Tabular data with labeled columns and rows.") {
  if (is.null(ft) || !inherits(ft, "flextable")) {
    stop("Accessibility guardrail requires a flextable object.", call. = FALSE)
  }

  col_keys <- tryCatch(as.character(ft$col_keys), error = function(e) character(0))
  if (length(col_keys) == 0 || anyNA(col_keys) || any(!nzchar(trimws(col_keys)))) {
    stop("Accessibility guardrail requires non-empty table column keys.", call. = FALSE)
  }

  header_rows <- tryCatch(nrow(ft$header$dataset), error = function(e) 0L)
  if (is.na(header_rows) || header_rows < 1L) {
    stop("Accessibility guardrail requires at least one header row.", call. = FALSE)
  }

  if (is.null(word_title) || !nzchar(trimws(as.character(word_title[[1]])))) {
    word_title <- "Data table"
  }
  if (is.null(word_description) || !nzchar(trimws(as.character(word_description[[1]])))) {
    word_description <- "Tabular data with labeled columns and rows."
  }

  flextable::set_table_properties(
    ft,
    opts_word = list(repeat_headers = TRUE),
    word_title = as.character(word_title[[1]]),
    word_description = as.character(word_description[[1]])
  )
}

adfg_ft_apply_table_guardrails <- function(ft,
                                           left_cols = NULL,
                                           right_cols = NULL,
                                           header_center_cols = NULL,
                                           header_center_rows = 1L,
                                           header_rule_rows = header_center_rows,
                                           header_rule_cols = header_center_cols,
                                           group_boundary_cols = NULL,
                                           border = adfg_ft_rule_border(),
                                           fontname = "Times New Roman",
                                           font_size = 10,
                                           line_space = 1.0,
                                           padding = 2,
                                           min_font_size = 9,
                                           max_font_size = 10,
                                           body_height = NULL,
                                           header_height = NULL,
                                           enforce_max_width = TRUE,
                                           max_total_width = 6.5,
                                           prefer_max_font_size = TRUE,
                                           header_alignment_map = list(),
                                           header_rule_map = list(),
                                           add_header_top = TRUE,
                                           add_header_bottom = TRUE,
                                           word_title = "Data table",
                                           word_description = "Tabular data with labeled columns and rows.") {
  requested_size <- adfg_ft_clamp_font_size(font_size, min_font_size, max_font_size)
  preferred_size <- if (isTRUE(prefer_max_font_size)) {
    adfg_ft_clamp_font_size(max_font_size, min_font_size, max_font_size)
  } else {
    requested_size
  }

  ft <- adfg_ft_apply_typography_guardrails(
    ft,
    fontname = fontname,
    font_size = preferred_size,
    line_space = line_space,
    padding = padding,
    min_font_size = min_font_size,
    max_font_size = max_font_size
  )

  ft <- adfg_ft_apply_row_height_guardrails(
    ft,
    body_height = body_height,
    header_height = header_height
  )

  ft <- adfg_ft_apply_standard_rules(
    ft,
    left_cols = left_cols,
    right_cols = right_cols,
    header_center_cols = header_center_cols,
    header_center_rows = header_center_rows,
    header_rule_rows = header_rule_rows,
    header_rule_cols = header_rule_cols,
    group_boundary_cols = group_boundary_cols,
    border = border
  )

  if (length(header_alignment_map) > 0) {
    ft <- adfg_ft_apply_header_alignment_map(ft, alignment_map = header_alignment_map)
  }

  if (length(header_rule_map) > 0) {
    ft <- adfg_ft_apply_header_rule_map(
      ft,
      rule_map = header_rule_map,
      border = border,
      add_top = add_header_top,
      add_bottom = add_header_bottom
    )
  }

  if (isTRUE(enforce_max_width) &&
      preferred_size > min_font_size &&
      adfg_ft_exceeds_max_width(ft, max_total_width = max_total_width)) {
    ft <- adfg_ft_apply_typography_guardrails(
      ft,
      fontname = fontname,
      font_size = min_font_size,
      line_space = line_space,
      padding = padding,
      min_font_size = min_font_size,
      max_font_size = max_font_size
    )
  }

  if (isTRUE(enforce_max_width)) {
    ft <- adfg_ft_fit_within_width_guardrails(
      ft,
      max_total_width = max_total_width
    )
  }

  adfg_ft_apply_accessibility(
    ft,
    word_title = word_title,
    word_description = word_description
  )
}

adfg_ft_apply_standard_rules <- function(ft,
                                         left_cols = NULL,
                                         right_cols = NULL,
                                         header_center_cols = NULL,
                                         header_center_rows = 1L,
                                         header_rule_rows = header_center_rows,
                                         header_rule_cols = header_center_cols,
                                         group_boundary_cols = NULL,
                                         border = adfg_ft_rule_border()) {
  ft <- flextable::border_remove(ft)

  if (length(left_cols) > 0) {
    ft <- flextable::align(ft, j = left_cols, align = "left", part = "all")
  }
  if (length(right_cols) > 0) {
    ft <- flextable::align(ft, j = right_cols, align = "right", part = "all")
  }
  if (length(header_center_cols) > 0) {
    for (header_row in unique(header_center_rows)) {
      ft <- flextable::align(ft, j = header_center_cols, align = "center", part = "header", i = header_row)
    }
  }

  ft <- flextable::hline_top(ft, border = border, part = "header")
  if (length(header_rule_cols) > 0) {
    for (header_row in unique(header_rule_rows)) {
      ft <- flextable::hline(ft, i = header_row, j = header_rule_cols, border = border, part = "header")
    }
  }
  ft <- flextable::hline_bottom(ft, border = border, part = "header")

  # Global report rule: no vertical borders in tables.
  # Keep horizontal rules only, even when group boundary columns are provided.

  ft
}

adfg_ft_apply_context_borders <- function(ft,
                                          row_labels,
                                          numeric_cols,
                                          total_pattern = "\\b(total|subtotal)\\b",
                                          subtotal_pattern = "\\bsubtotal\\b",
                                          full_row_border_pattern = NULL,
                                          full_row_border_exclude_pattern = NULL,
                                          full_row_border_cols = NULL,
                                          border = adfg_ft_rule_border()) {
  row_labels_chr <- tolower(trimws(as.character(row_labels)))

  rule_rows <- grep(total_pattern, row_labels_chr)
  if (length(rule_rows) == 0) {
    return(ft)
  }

  subtotal_rows <- grep(subtotal_pattern, row_labels_chr)
  subtotals_with_subtotal_below <- subtotal_rows[(subtotal_rows + 1L) %in% subtotal_rows]
  rule_rows_for_bottom <- setdiff(rule_rows, subtotals_with_subtotal_below)

  if (length(rule_rows_for_bottom) > 0) {
    ft <- flextable::hline(ft, i = rule_rows_for_bottom, border = border, part = "body")
  }

  preceding_rows <- unique(pmax(rule_rows - 1L, 1L))
  preceding_subtotal_rows <- unique(pmax(subtotal_rows - 1L, 1L))
  preceding_subtotal_rows <- setdiff(preceding_subtotal_rows, subtotal_rows)
  preceding_non_subtotal_rows <- setdiff(preceding_rows, preceding_subtotal_rows)

  if (length(preceding_subtotal_rows) > 0 && length(numeric_cols) > 0) {
    ft <- flextable::hline(
      ft,
      i = preceding_subtotal_rows,
      j = numeric_cols,
      border = border,
      part = "body"
    )
  }

  if (length(preceding_non_subtotal_rows) > 0) {
    if (length(numeric_cols) > 0) {
      ft <- flextable::hline(
        ft,
        i = preceding_non_subtotal_rows,
        j = numeric_cols,
        border = border,
        part = "body"
      )
    } else {
      ft <- flextable::hline(
        ft,
        i = preceding_non_subtotal_rows,
        border = border,
        part = "body"
      )
    }
  }

  # Optional rule: enforce full top+bottom borders across selected rows.
  if (!is.null(full_row_border_pattern) && nzchar(as.character(full_row_border_pattern[[1]]))) {
    full_rows <- grep(as.character(full_row_border_pattern[[1]]), row_labels_chr)

    if (!is.null(full_row_border_exclude_pattern) && nzchar(as.character(full_row_border_exclude_pattern[[1]]))) {
      exclude_rows <- grep(as.character(full_row_border_exclude_pattern[[1]]), row_labels_chr)
      full_rows <- setdiff(full_rows, exclude_rows)
    }

    if (length(full_rows) > 0) {
      border_cols <- full_row_border_cols
      if (is.null(border_cols) || length(border_cols) == 0) {
        border_cols <- tryCatch({
          if (!is.null(ft$col_keys) && length(ft$col_keys) > 0) {
            as.character(ft$col_keys)
          } else if (!is.null(ft$body$dataset)) {
            names(ft$body$dataset)
          } else {
            character(0)
          }
        }, error = function(e) character(0))
      }

      top_rows <- unique(full_rows[full_rows > 1L] - 1L)

      if (length(border_cols) > 0) {
        if (length(top_rows) > 0) {
          ft <- flextable::hline(
            ft,
            i = top_rows,
            j = border_cols,
            border = border,
            part = "body"
          )
        }
        ft <- flextable::hline(
          ft,
          i = full_rows,
          j = border_cols,
          border = border,
          part = "body"
        )
        if (any(full_rows == 1L)) {
          ft <- flextable::hline_top(
            ft,
            j = border_cols,
            border = border,
            part = "body"
          )
        }
      } else {
        if (length(top_rows) > 0) {
          ft <- flextable::hline(
            ft,
            i = top_rows,
            border = border,
            part = "body"
          )
        }
        ft <- flextable::hline(
          ft,
          i = full_rows,
          border = border,
          part = "body"
        )
        if (any(full_rows == 1L)) {
          ft <- flextable::hline_top(
            ft,
            border = border,
            part = "body"
          )
        }
      }
    }
  }

  ft
}

adfg_ft_apply_break_columns <- function(ft,
                                        break_cols,
                                        width = NULL,
                                        clear_header_borders = TRUE,
                                        clear_header_padding = TRUE,
                                        header_bg = "white") {
  if (length(break_cols) == 0) return(ft)

  break_cols <- as.character(break_cols)
  break_cols <- break_cols[!is.na(break_cols) & nzchar(trimws(break_cols))]
  if (length(break_cols) == 0) return(ft)

  if (!is.null(width)) {
    ft <- flextable::width(ft, j = break_cols, width = width)
  }

  if (isTRUE(clear_header_borders)) {
    no_border <- officer::fp_border(style = "none", width = 0)
    ft <- flextable::border(ft, j = break_cols, border.left = no_border, border.right = no_border, part = "header")
  }

  if (isTRUE(clear_header_padding)) {
    ft <- flextable::padding(
      ft,
      j = break_cols,
      padding.left = 0,
      padding.right = 0,
      padding.top = 0,
      padding.bottom = 0,
      part = "header"
    )
  }

  if (!is.null(header_bg) && nzchar(as.character(header_bg[[1]]))) {
    ft <- flextable::bg(ft, j = break_cols, bg = as.character(header_bg[[1]]), part = "header")
  }

  ft
}

adfg_ft_add_footer_notes <- function(ft,
                                     note_lines = character(0),
                                     marker_map = NULL,
                                     base_note = "NA indicates not applicable. ND indicates no data.",
                                     footer_font_size = 9,
                                     fontname = "Times New Roman",
                                     word_style = NULL,
                                     min_font_size = 9,
                                     max_font_size = 10,
                                     line_space = 0.9,
                                     row_height = 0.2,
                                     render_mode = c("table", "paragraph")) {
  render_mode <- match.arg(render_mode)

  if (is.null(word_style) || !nzchar(as.character(word_style))) {
    word_style <- adfg_table_footnote_style()
  }

  payload <- adfg_ft_build_footnote_payload(
    note_lines = note_lines,
    marker_map = marker_map,
    base_note = base_note,
    word_style = word_style,
    render_mode = render_mode
  )

  footer_lines <- c("*Note*: NA indicates not applicable. ND indicates no data.", note_lines)
  if (length(footer_lines) == 0 && length(payload$items) == 0) {
    return(ft)
  }

  if (identical(render_mode, "paragraph")) {
    attr(ft, "adfg_dynamic_footnotes") <- payload
    return(ft)
  }

  ft <- flextable::add_footer_lines(ft, values = footer_lines)
  ft <- flextable::compose(
    ft,
    part = "footer",
    i = 1,
    j = 1,
    value = flextable::as_paragraph(
      flextable::as_i("Note"),
      flextable::as_chunk(paste0(": ", base_note))
    )
  )

  if (length(note_lines) > 0 && !is.null(marker_map)) {
    for (idx in seq_along(note_lines)) {
      cmt <- note_lines[[idx]]
      marker <- unname(marker_map[[cmt]])
      if (is.null(marker) || is.na(marker) || !nzchar(marker)) next
      ft <- flextable::compose(
        ft,
        part = "footer",
        i = idx + 1,
        j = 1,
        value = adfg_ft_paragraph_with_tab_marker(marker, cmt)
      )
    }
  }

  ft <- adfg_ft_apply_footer_guardrails(
    ft,
    fontname = fontname,
    font_size = footer_font_size,
    word_style = word_style,
    min_font_size = min_font_size,
    max_font_size = max_font_size,
    line_space = line_space,
    row_height = row_height
  )

  ft <- flextable::hrule(ft, rule = "atleast", part = "footer")
  attr(ft, "adfg_dynamic_footnotes") <- payload
  ft
}

adfg_ft_build_footnote_payload <- function(note_lines = character(0),
                                           marker_map = NULL,
                                           base_note = "NA indicates not applicable. ND indicates no data.",
                                           word_style = NULL,
                                           render_mode = "table",
                                           trailing_break = "none") {
  if (is.null(word_style) || !nzchar(as.character(word_style))) {
    word_style <- adfg_table_footnote_style()
  }

  note_lines <- as.character(note_lines)
  note_lines <- note_lines[!is.na(note_lines) & nzchar(trimws(note_lines))]

  items <- lapply(note_lines, function(cmt) {
    marker <- if (!is.null(marker_map)) unname(marker_map[[cmt]]) else NA_character_
    marker <- as.character(marker[[1]])
    has_marker <- !is.na(marker) && nzchar(trimws(marker))
    list(
      marker = if (has_marker) trimws(marker) else NA_character_,
      text = as.character(cmt),
      has_marker = has_marker
    )
  })

  base_note_scalar <- if (is.null(base_note) || length(base_note) == 0) {
    NA_character_
  } else {
    as.character(base_note[[1]])
  }

  word_style_scalar <- if (is.null(word_style) || length(word_style) == 0) {
    as.character(adfg_table_footnote_style())
  } else {
    as.character(word_style[[1]])
  }

  render_mode_scalar <- if (is.null(render_mode) || length(render_mode) == 0) {
    "table"
  } else {
    as.character(render_mode[[1]])
  }

  trailing_break_scalar <- if (is.null(trailing_break) || length(trailing_break) == 0) {
    "none"
  } else {
    as.character(trailing_break[[1]])
  }

  list(
    base_note = base_note_scalar,
    items = items,
    word_style = word_style_scalar,
    render_mode = render_mode_scalar,
    trailing_break = trailing_break_scalar
  )
}

adfg_ft_set_trailing_break <- function(ft,
                                       trailing_break = c("none", "page", "landscape_end")) {
  trailing_break <- match.arg(trailing_break)

  payload <- attr(ft, "adfg_dynamic_footnotes", exact = TRUE)
  if (is.null(payload)) {
    return(ft)
  }

  payload$trailing_break <- trailing_break
  attr(ft, "adfg_dynamic_footnotes") <- payload
  ft
}

adfg_emit_pending_break <- function(trailing_break = c("none", "page", "landscape_end")) {
  trailing_break <- match.arg(trailing_break)

  if (identical(trailing_break, "page")) {
    word_pagebreak()
    return(invisible(NULL))
  }

  if (identical(trailing_break, "landscape_end")) {
    adfg_word_landscape_block_end()
    return(invisible(NULL))
  }

  invisible(NULL)
}

adfg_emit_footnote_payload <- function(payload,
                                       style = NULL) {
  if (is.null(payload)) return(invisible(NULL))

  payload_style <- payload$word_style
  if (is.null(style) || !nzchar(as.character(style))) {
    style <- payload_style
  }
  if (is.null(style) || !nzchar(as.character(style)) || identical(trimws(as.character(style[[1]])), "Normal")) {
    style <- adfg_table_footnote_style()
  }
  style <- as.character(style[[1]])

  trailing_break <- if (is.null(payload$trailing_break) || length(payload$trailing_break) == 0) {
    "none"
  } else {
    as.character(payload$trailing_break[[1]])
  }
  if (!trailing_break %in% c("none", "page", "landscape_end")) {
    trailing_break <- "none"
  }

  trailing_section_xml <- if (identical(trailing_break, "landscape_end") &&
                              identical(knitr::pandoc_to(), "docx")) {
    adfg_word_section_pr_xml("landscape", "nextPage")
  } else {
    NULL
  }

  base_note <- if (is.null(payload$base_note) || length(payload$base_note) == 0) {
    NA_character_
  } else {
    as.character(payload$base_note[[1]])
  }

  items <- payload$items
  if (is.null(items) || !is.list(items)) {
    items <- list()
  }

  nonempty_items <- list()
  if (length(items) > 0) {
    for (it in items) {
      if (is.null(it) || !is.list(it) || is.null(it$text) || length(it$text) == 0) next
      txt <- as.character(it$text[[1]])
      if (is.na(txt) || !nzchar(trimws(txt))) next
      nonempty_items[[length(nonempty_items) + 1L]] <- it
    }
  }

  base_note_has_text <- !is.na(base_note) && nzchar(trimws(base_note))
  if (base_note_has_text) {
    base_note_pagebreak <- identical(trailing_break, "page") && length(nonempty_items) == 0
    if (!is.null(trailing_section_xml) && length(nonempty_items) == 0) {
      xml_escape <- function(x) {
        x <- gsub("&", "&amp;", x, fixed = TRUE)
        x <- gsub("<", "&lt;", x, fixed = TRUE)
        x <- gsub(">", "&gt;", x, fixed = TRUE)
        x
      }
      pagebreak_xml <- if (isTRUE(base_note_pagebreak)) "<w:r><w:br w:type=\"page\"/></w:r>" else ""
      adfg_emit_openxml_block(
        paste0(
          "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
          "<w:pPr><w:pStyle w:val=\"", xml_escape(style), "\"/>", trailing_section_xml, "</w:pPr>",
          "<w:r><w:rPr><w:i/></w:rPr><w:t xml:space=\"preserve\">Note:</w:t></w:r>",
          "<w:r><w:t xml:space=\"preserve\"> ", xml_escape(base_note), "</w:t></w:r>",
          pagebreak_xml,
          "</w:p>"
        ),
        mark_pagebreak = TRUE
      )
    } else {
      adfg_style_note_line(base_note, style = style, pagebreak = base_note_pagebreak)
    }
  }

  if (length(nonempty_items) == 0) {
    if (!identical(trailing_break, "none") && !identical(trailing_break, "page") && is.null(trailing_section_xml)) {
      adfg_emit_pending_break(trailing_break)
    }
    return(invisible(NULL))
  }

  for (idx in seq_along(nonempty_items)) {
    it <- nonempty_items[[idx]]
    txt <- as.character(it$text[[1]])
    if (is.na(txt) || !nzchar(trimws(txt))) next

    has_marker <- isTRUE(it$has_marker)
    marker <- as.character(it$marker[[1]])
    is_last_item <- idx == length(nonempty_items)
    item_pagebreak <- identical(trailing_break, "page") && is_last_item
    item_section_xml <- if (!is.null(trailing_section_xml) && is_last_item) trailing_section_xml else NULL
    if (has_marker && !is.na(marker) && nzchar(trimws(marker))) {
      if (!is.null(item_section_xml) && identical(knitr::pandoc_to(), "docx")) {
        xml_escape <- function(x) {
          x <- gsub("&", "&amp;", x, fixed = TRUE)
          x <- gsub("<", "&lt;", x, fixed = TRUE)
          x <- gsub(">", "&gt;", x, fixed = TRUE)
          x
        }
        tab_inches <- suppressWarnings(as.numeric(getOption("adfg.footnote.tab_inches", NA_real_)))
        if (is.na(tab_inches) || tab_inches <= 0) {
          tab_twips_opt <- suppressWarnings(as.integer(getOption("adfg.footnote.tab_twips", 216L)))
          if (is.na(tab_twips_opt) || tab_twips_opt < 60L) tab_twips_opt <- 216L
          tab_inches <- tab_twips_opt / 1440
        }
        tab_twips <- as.integer(round(tab_inches * 1440))
        if (is.na(tab_twips) || tab_twips < 60L) tab_twips <- 216L
        left_indent_twips <- suppressWarnings(as.integer(getOption("adfg.footnote.left_indent_twips", tab_twips)))
        if (is.na(left_indent_twips) || left_indent_twips < tab_twips) left_indent_twips <- tab_twips
        pagebreak_xml <- if (isTRUE(item_pagebreak)) "<w:r><w:br w:type=\"page\"/></w:r>" else ""
        adfg_emit_openxml_block(
          paste0(
            "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
            "<w:pPr>",
            "<w:pStyle w:val=\"", xml_escape(style), "\"/>",
            "<w:tabs><w:tab w:val=\"left\" w:pos=\"", tab_twips, "\"/></w:tabs>",
            "<w:ind w:left=\"", left_indent_twips, "\" w:hanging=\"", tab_twips, "\"/>",
            item_section_xml,
            "</w:pPr>",
            "<w:r><w:rPr><w:vertAlign w:val=\"superscript\"/></w:rPr><w:t xml:space=\"preserve\">", xml_escape(trimws(marker)), "</w:t></w:r>",
            "<w:r><w:tab/></w:r>",
            "<w:r><w:t xml:space=\"preserve\">", xml_escape(txt), "</w:t></w:r>",
            pagebreak_xml,
            "</w:p>"
          ),
          mark_pagebreak = TRUE
        )
      } else {
        adfg_style_footnote_line(trimws(marker), txt, style = style, pagebreak = item_pagebreak)
      }
    } else {
      if (!is.null(item_section_xml) && identical(knitr::pandoc_to(), "docx")) {
        xml_escape <- function(x) {
          x <- gsub("&", "&amp;", x, fixed = TRUE)
          x <- gsub("<", "&lt;", x, fixed = TRUE)
          x <- gsub(">", "&gt;", x, fixed = TRUE)
          x
        }
        adfg_emit_openxml_block(
          paste0(
            "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
            "<w:pPr><w:pStyle w:val=\"", xml_escape(style), "\"/>", item_section_xml, "</w:pPr>",
            "<w:r><w:t xml:space=\"preserve\">", xml_escape(txt), "</w:t></w:r>",
            "</w:p>"
          ),
          mark_pagebreak = TRUE
        )
      } else {
        adfg_style_paragraph(txt, style = style)
      }
      if (item_pagebreak) {
        adfg_emit_pending_break("page")
      }
    }
  }

  if (!identical(trailing_break, "none") && !identical(trailing_break, "page") && is.null(trailing_section_xml)) {
    adfg_emit_pending_break(trailing_break)
  }

  invisible(NULL)
}

adfg_print_flextable_with_footnotes <- function(ft) {
  cat(as.character(knitr::knit_print(ft)), sep = "\n")
  invisible(NULL)
}

adfg_register_knit_print_flextable <- function() {
  if (!requireNamespace("knitr", quietly = TRUE) || !requireNamespace("flextable", quietly = TRUE)) {
    return(invisible(FALSE))
  }

  orig_method <- get0(
    "knit_print.flextable",
    envir = asNamespace("flextable"),
    mode = "function",
    inherits = FALSE
  )
  if (is.null(orig_method)) {
    return(invisible(FALSE))
  }

  assign(
    "knit_print.flextable",
    function(x, ...) {
      base_out <- orig_method(x, ...)

      payload <- attr(x, "adfg_dynamic_footnotes", exact = TRUE)
      payload_mode <- if (is.null(payload) || is.null(payload$render_mode) || length(payload$render_mode) == 0) {
        ""
      } else {
        as.character(payload$render_mode[[1]])
      }
      if (is.null(payload) || !identical(payload_mode, "paragraph")) {
        return(base_out)
      }

      note_block <- paste(capture.output(adfg_emit_footnote_payload(payload)), collapse = "\n")
      if (!nzchar(note_block)) {
        return(base_out)
      }

      knitr::asis_output(paste0(as.character(base_out), note_block, "\n"))
    },
    envir = .GlobalEnv
  )

  invisible(TRUE)
}

adfg_register_knit_print_flextable()

adfg_ft_apply_footer_guardrails <- function(ft,
                                            fontname = "Times New Roman",
                                            font_size = 9,
                                            word_style = NULL,
                                            min_font_size = 9,
                                            max_font_size = 10,
                                            line_space = 0.9,
                                            row_height = 0.2) {
  default_style <- adfg_table_footnote_style()
  if (is.null(word_style) || !nzchar(as.character(word_style))) {
    word_style <- default_style
  }
  if (identical(trimws(as.character(word_style[[1]])), "Normal")) {
    word_style <- default_style
  }

  tab_inches <- suppressWarnings(as.numeric(getOption("adfg.footnote.tab_inches", 0.15)))
  if (is.na(tab_inches) || tab_inches <= 0) tab_inches <- 0.15

  clamped_size <- adfg_ft_clamp_font_size(font_size, min_font_size, max_font_size)
  ft <- flextable::align(ft, align = "left", part = "footer")
  ft <- flextable::font(ft, fontname = fontname, part = "footer")
  ft <- flextable::fontsize(ft, size = clamped_size, part = "footer")
  ft <- flextable::line_spacing(ft, space = line_space, part = "footer")
  ft <- flextable::height(ft, part = "footer", height = row_height)
  ft <- flextable::padding(ft, part = "footer", padding.top = 0, padding.bottom = 0, padding.left = 0, padding.right = 0)

  # Apply paragraph style last and explicitly scope to footer cell ranges
  # so custom Word styles are written on each footer paragraph.
  footer_n <- tryCatch(flextable::nrow_part(ft, "footer"), error = function(e) 0L)
  footer_cols <- tryCatch(ft$col_keys, error = function(e) NULL)
  if (is.null(footer_cols) || length(footer_cols) == 0) {
    footer_cols <- 1
  }

  if (footer_n > 0) {
    ft <- flextable::style(
      ft,
      i = seq_len(footer_n),
      j = footer_cols,
      part = "footer",
      pr_p = officer::fp_par(
        word_style = word_style,
        line_spacing = line_space,
        tabs = officer::fp_tabs(officer::fp_tab(pos = tab_inches, style = "left")),
        hanging = tab_inches
      )
    )
  }

  ft
}

adfg_ft_build_basic_table <- function(data_obj,
                                      left_cols = character(0),
                                      right_cols = NULL,
                                      header_center_cols = NULL) {
  ft <- flextable::flextable(data_obj) %>%
    flextable::theme_booktabs() %>%
    flextable::autofit()

  if (is.null(right_cols)) {
    right_cols <- names(data_obj)[vapply(data_obj, is.numeric, logical(1))]
  }

  adfg_ft_apply_standard_rules(
    ft,
    left_cols = left_cols,
    right_cols = right_cols,
    header_center_cols = header_center_cols,
    header_rule_cols = header_center_cols,
    group_boundary_cols = NULL
  )
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

adfg_word_section_pr_xml <- function(orientation = c("portrait", "landscape"),
                                     break_type = c("nextPage", "continuous")) {
  orientation <- match.arg(orientation)
  break_type <- match.arg(break_type)

  profile <- adfg_word_section_profile(orientation)

  paste0(
    "<w:sectPr>",
    "<w:type w:val=\"", break_type, "\"/>",
    profile$footerRef,
    profile$pgSz,
    profile$pgMar,
    profile$cols,
    profile$formProt,
    profile$pgNumType,
    profile$docGrid,
    "</w:sectPr>"
  )
}

adfg_style_paragraph <- function(text,
                                 style = "Normal",
                                 heading1_space_before_pt = getOption("adfg.heading1.space_before.pt", 200)) {
  if (length(text) == 0) return(invisible(NULL))

  text <- as.character(text[[1]])
  style <- as.character(style[[1]])

  if (grepl("Footnote", style, fixed = TRUE) && grepl("^\\s*Note:", text, perl = TRUE)) {
    text <- sub("^\\s*Note:", "*Note:*", text, perl = TRUE)
  }

  is_docx <- requireNamespace("knitr", quietly = TRUE) && identical(knitr::pandoc_to(), "docx")

  if (is_docx && identical(style, "Heading 1")) {
    xml_escape <- function(x) {
      x <- gsub("&", "&amp;", x, fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x
    }

    before_twips <- 0L
    before_pt <- suppressWarnings(as.numeric(heading1_space_before_pt))
    if (!is.na(before_pt) && before_pt > 0) {
      before_twips <- as.integer(round(before_pt * 20))
    }

    spacing_xml <- if (before_twips > 0L) {
      paste0("<w:spacing w:before=\"", before_twips, "\"/>")
    } else {
      ""
    }

    adfg_emit_openxml_block(
      paste0(
        "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
        "<w:pPr><w:pStyle w:val=\"", xml_escape(style), "\"/>", spacing_xml, "</w:pPr>",
        "<w:r><w:t xml:space=\"preserve\">", xml_escape(text), "</w:t></w:r>",
        "</w:p>"
      )
    )
    return(invisible(NULL))
  }

  cat("::: {custom-style=\"", style, "\"}\n", text, "\n:::\n\n", sep = "")
  invisible(NULL)
}

adfg_style_paragraph_with_before <- function(text,
                                             style = "Heading 1",
                                             space_before_pt = getOption("adfg.heading1.space_before.pt", 200),
                                             leading_blank_paragraph = FALSE) {
  if (length(text) == 0) return(invisible(NULL))

  text <- as.character(text[[1]])
  style <- as.character(style[[1]])

  is_docx <- requireNamespace("knitr", quietly = TRUE) && identical(knitr::pandoc_to(), "docx")
  before_pt <- suppressWarnings(as.numeric(space_before_pt))

  if (!is_docx || is.na(before_pt) || before_pt <= 0) {
    adfg_style_paragraph(text, style = style)
    return(invisible(NULL))
  }

  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  before_twips <- as.integer(round(before_pt * 20))
  spacing_xml <- paste0("<w:spacing w:before=\"", before_twips, "\"/>")

  if (isTRUE(leading_blank_paragraph)) {
    cat("\n\n")
    adfg_emit_openxml_block(
      paste0(
        "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
        "<w:r><w:t xml:space=\"preserve\"> </w:t></w:r>",
        "</w:p>"
      )
    )
  }

  adfg_emit_openxml_block(
    paste0(
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
      "<w:pPr><w:pStyle w:val=\"", xml_escape(style), "\"/>", spacing_xml, "</w:pPr>",
      "<w:r><w:t xml:space=\"preserve\">", xml_escape(text), "</w:t></w:r>",
      "</w:p>"
    )
  )

  invisible(NULL)
}

adfg_word_section_profile <- function(orientation = c("portrait", "landscape")) {
  orientation <- match.arg(orientation)

  if (identical(orientation, "landscape")) {
    return(list(
      footerRef = "",
      pgSz = "<w:pgSz w:w=\"15840\" w:h=\"12240\" w:orient=\"landscape\" w:code=\"1\"/>",
      pgMar = "<w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" w:header=\"708\" w:footer=\"708\" w:gutter=\"0\"/>",
      cols = "<w:cols w:space=\"432\"/>",
      formProt = "<w:formProt w:val=\"0\"/>",
      pgNumType = "<w:pgNumType w:fmt=\"decimal\"/>",
      docGrid = "<w:docGrid w:linePitch=\"326\"/>"
    ))
  }

  list(
    footerRef = "",
    pgSz = "<w:pgSz w:w=\"12240\" w:h=\"15840\"/>",
    pgMar = "<w:pgMar w:top=\"1440\" w:right=\"1440\" w:bottom=\"1440\" w:left=\"1440\" w:header=\"708\" w:footer=\"708\" w:gutter=\"0\"/>",
    cols = "<w:cols w:space=\"720\"/>",
    formProt = "",
    pgNumType = "<w:pgNumType w:fmt=\"decimal\"/>",
    docGrid = "<w:docGrid w:linePitch=\"360\"/>"
  )
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

adfg_word_section_break <- function(orientation = c("portrait", "landscape"), break_type = c("nextPage", "continuous")) {
  orientation <- match.arg(orientation)
  break_type <- match.arg(break_type)
  if (!identical(knitr::pandoc_to(), "docx")) return(invisible(NULL))

  state <- adfg_get_pagebreak_state()
  if (isTRUE(state$last_was_pagebreak)) {
    return(invisible(NULL))
  }
  state$last_was_pagebreak <- TRUE

  profile <- adfg_word_section_profile(orientation)

  adfg_emit_openxml_block(
    paste0(
      "<w:p><w:pPr><w:sectPr>",
      "<w:type w:val=\"", break_type, "\"/>",
      profile$footerRef,
      profile$pgSz,
      profile$pgMar,
      profile$cols,
      profile$formProt,
      profile$pgNumType,
      profile$docGrid,
      "</w:sectPr></w:pPr></w:p>"
    ),
    mark_pagebreak = TRUE
  )
}

adfg_word_landscape_block_start <- function() {
  # In WordprocessingML, sectPr on this break applies to the section that just ended.
  adfg_word_section_break("portrait", "nextPage")
}

adfg_word_landscape_block_end <- function() {
  # Apply landscape to the section that just ended (the table block).
  # Do not emit a trailing portrait section break here; that extra break
  # can create blank portrait pages in downstream sections.
  adfg_word_section_break("landscape", "nextPage")
}

if (!exists("adfg_table_emit_continued_rule", mode = "function", inherits = TRUE)) {
  adfg_table_emit_continued_rule <- function(has_split,
                                             table_num,
                                             action = c("marker", "pagebreak", "caption"),
                                             page = 2,
                                             total_pages = 2,
                                             marker_text = "-continued-",
                                             marker_style = "Table-Continued",
                                             caption_style = "Caption") {
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

    # For split tables, use a page break within the same section so orientation is preserved.
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

    if (identical(action, "pagebreak")) {
      emit_helper_result(in_section_pagebreak_fn())
      return(invisible(TRUE))
    }

    emit_helper_result(style_fn(
      paste0("Table ", table_num, ".–Page ", page, " of ", total_pages, "."),
      caption_style
    ))
    invisible(TRUE)
  }
}

adfg_pick_split_row <- function(rows_df,
                                label_col,
                                anchor_labels = character(0),
                                fallback_row = NA_integer_,
                                min_head_rows = 1L,
                                min_tail_rows = 1L) {
  if (is.null(rows_df) || !is.data.frame(rows_df) || nrow(rows_df) == 0) return(NA_integer_)
  if (!label_col %in% names(rows_df)) return(NA_integer_)

  n <- nrow(rows_df)
  min_head_rows <- max(1L, suppressWarnings(as.integer(min_head_rows)))
  min_tail_rows <- max(1L, suppressWarnings(as.integer(min_tail_rows)))
  if (n < (min_head_rows + min_tail_rows)) return(NA_integer_)

  labels <- trimws(as.character(rows_df[[label_col]]))
  split_row <- NA_integer_

  for (anchor in anchor_labels) {
    hit <- match(trimws(as.character(anchor)), labels)
    if (!is.na(hit)) {
      split_row <- as.integer(hit)
      break
    }
  }

  if (is.na(split_row)) {
    candidate <- suppressWarnings(as.integer(fallback_row))
    if (!is.na(candidate)) {
      split_row <- candidate
    }
  }

  if (is.na(split_row)) {
    split_row <- as.integer(floor(n / 2))
  }

  lower_bound <- min_head_rows
  upper_bound <- n - min_tail_rows
  split_row <- max(lower_bound, min(split_row, upper_bound))

  if (split_row < 1L || split_row >= n) return(NA_integer_)
  split_row
}

adfg_ft_superscript_parts <- function(marker, leading_space = TRUE) {
  marker <- as.character(marker[[1]])
  if (is.na(marker) || !nzchar(marker)) {
    return(list())
  }

  parts <- list()
  if (isTRUE(leading_space)) {
    parts <- c(parts, list(flextable::as_chunk(" ")))
  }
  parts <- c(parts, list(flextable::as_sup(marker)))
  parts
}

adfg_ft_paragraph_with_sup <- function(label, marker, leading_space = TRUE) {
  base <- list(flextable::as_chunk(label))
  sup_parts <- adfg_ft_superscript_parts(marker, leading_space = leading_space)
  do.call(flextable::as_paragraph, c(base, sup_parts))
}

# Guardrail: footnote markers are always rendered as marker + tab + description.
adfg_ft_paragraph_with_tab_marker <- function(marker, text) {
  marker <- trimws(as.character(marker[[1]]))
  text <- as.character(text[[1]])
  flextable::as_paragraph(
    flextable::as_chunk(marker),
    flextable::as_chunk("\t"),
    flextable::as_chunk(text)
  )
}

adfg_format_footnote_line <- function(marker, text) {
  marker <- trimws(as.character(marker[[1]]))
  text <- as.character(text[[1]])
  paste0(marker, "\t", text)
}

adfg_style_footnote_line <- function(marker,
                                     text,
                                     style = adfg_table_footnote_style(),
                                     pagebreak = FALSE) {
  marker <- trimws(as.character(marker[[1]]))
  text <- as.character(text[[1]])
  style <- as.character(style[[1]])
  if (!nzchar(style) || identical(trimws(style), "Normal")) {
    style <- adfg_table_footnote_style()
  }

  is_docx <- requireNamespace("knitr", quietly = TRUE) && identical(knitr::pandoc_to(), "docx")

  adfg_reset_pagebreak_state()

  if (!is_docx) {
    adfg_style_paragraph(adfg_format_footnote_line(marker, text), style = style)
    if (isTRUE(pagebreak)) {
      pagebreak_fn <- get0("word_pagebreak", mode = "function", inherits = TRUE)
      if (!is.null(pagebreak_fn)) pagebreak_fn()
    }
    return(invisible(NULL))
  }

  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  tab_inches <- suppressWarnings(as.numeric(getOption("adfg.footnote.tab_inches", NA_real_)))
  if (is.na(tab_inches) || tab_inches <= 0) {
    tab_twips_opt <- suppressWarnings(as.integer(getOption("adfg.footnote.tab_twips", 216L)))
    if (is.na(tab_twips_opt) || tab_twips_opt < 60L) tab_twips_opt <- 216L
    tab_inches <- tab_twips_opt / 1440
  }

  tab_twips <- as.integer(round(tab_inches * 1440))
  if (is.na(tab_twips) || tab_twips < 60L) tab_twips <- 216L

  left_indent_twips <- suppressWarnings(as.integer(getOption("adfg.footnote.left_indent_twips", tab_twips)))
  if (is.na(left_indent_twips) || left_indent_twips < tab_twips) {
    left_indent_twips <- tab_twips
  }

  pagebreak_xml <- if (isTRUE(pagebreak)) {
    "<w:r><w:br w:type=\"page\"/></w:r>"
  } else {
    ""
  }

  adfg_emit_openxml_block(
    paste0(
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
      "<w:pPr>",
      "<w:pStyle w:val=\"", xml_escape(style), "\"/>",
      "<w:tabs><w:tab w:val=\"left\" w:pos=\"", tab_twips, "\"/></w:tabs>",
      "<w:ind w:left=\"", left_indent_twips, "\" w:hanging=\"", tab_twips, "\"/>",
      "</w:pPr>",
      "<w:r><w:rPr><w:vertAlign w:val=\"superscript\"/></w:rPr><w:t xml:space=\"preserve\">", xml_escape(marker), "</w:t></w:r>",
      "<w:r><w:tab/></w:r>",
      "<w:r><w:t xml:space=\"preserve\">", xml_escape(text), "</w:t></w:r>",
      pagebreak_xml,
      "</w:p>"
    ),
    mark_pagebreak = isTRUE(pagebreak)
  )

  invisible(NULL)
}

adfg_style_note_line <- function(text,
                                 style = adfg_table_footnote_style(),
                                 pagebreak = FALSE) {
  text <- as.character(text[[1]])
  style <- as.character(style[[1]])
  if (!nzchar(style) || identical(trimws(style), "Normal")) {
    style <- adfg_table_footnote_style()
  }

  is_docx <- requireNamespace("knitr", quietly = TRUE) && identical(knitr::pandoc_to(), "docx")

  adfg_reset_pagebreak_state()

  if (!is_docx) {
    adfg_style_paragraph(paste0("*Note:* ", text), style = style)
    if (isTRUE(pagebreak)) {
      pagebreak_fn <- get0("word_pagebreak", mode = "function", inherits = TRUE)
      if (!is.null(pagebreak_fn)) pagebreak_fn()
    }
    return(invisible(NULL))
  }

  xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x
  }

  note_content <- paste0(
    "<w:r><w:rPr><w:i/></w:rPr><w:t xml:space=\"preserve\">Note:</w:t></w:r>",
    "<w:r><w:t xml:space=\"preserve\"> ", xml_escape(text), "</w:t></w:r>"
  )

  pagebreak_xml <- if (isTRUE(pagebreak)) {
    "<w:r><w:br w:type=\"page\"/></w:r>"
  } else {
    ""
  }

  adfg_emit_openxml_block(
    paste0(
      "<w:p xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">",
      "<w:pPr><w:pStyle w:val=\"", xml_escape(style), "\"/></w:pPr>",
      note_content,
      pagebreak_xml,
      "</w:p>"
    ),
    mark_pagebreak = isTRUE(pagebreak)
  )

  invisible(NULL)
}