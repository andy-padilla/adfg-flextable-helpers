adfg_fds_knit <- function(inputFile, encoding) {
  find_template_in_github_clones <- function() {
    user_home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
    roots <- unique(c(
      file.path(user_home, "Documents", "GitHub"),
      file.path(user_home, "OneDrive - State of Alaska", "Documents", "GitHub"),
      file.path(path.expand("~"), "GitHub")
    ))
    roots <- roots[dir.exists(roots)]
    if (length(roots) == 0) return(character())

    hits <- character()
    for (root in roots) {
      repos <- list.dirs(root, full.names = TRUE, recursive = FALSE)
      for (repo in repos) {
        nested <- c(repo, list.dirs(repo, full.names = TRUE, recursive = FALSE))
        for (candidate in nested) {
          if (file.exists(file.path(candidate, "Rcode", "template_report_helpers.R")) &&
              dir.exists(file.path(candidate, "report"))) {
            hits <- c(hits, candidate)
          }
        }
      }
    }
    unique(hits)
  }

  locate_template_dir <- function(start = getwd()) {
    env_template <- Sys.getenv("ADFG_TEMPLATE_DIR", unset = "")
    onedrive_template <- file.path(
      Sys.getenv("USERPROFILE", unset = path.expand("~")),
      "OneDrive - State of Alaska", "Documents", "GitHub", "ADFG_Report_Templates", "ADFG FDS template"
    )
    candidates <- unique(c(
      env_template,
      onedrive_template,
      start,
      dirname(start),
      file.path(start, "ADFG FDS template"),
      file.path(dirname(start), "ADFG FDS template"),
      find_template_in_github_clones()
    ))
    candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]

    for (candidate in candidates) {
      if (file.exists(file.path(candidate, "Rcode", "template_report_helpers.R")) &&
          dir.exists(file.path(candidate, "report"))) {
        return(normalizePath(candidate, mustWork = TRUE, winslash = "/"))
      }
    }

    stop(
      "Cannot locate the ADFG template folder. Set ADFG_TEMPLATE_DIR or open from a workspace that includes the template.",
      call. = FALSE
    )
  }

  template_dir <- locate_template_dir()
  template_runtime_helpers_file <- file.path(template_dir, "Rcode", "template_report_helpers.R")
  source(template_runtime_helpers_file, local = TRUE)
  # adfg_word_document (cover/TOC post-processing) lives in render_report.R, not the helpers file.
  template_render_file <- file.path(template_dir, "Rcode", "render_report.R")
  if (file.exists(template_render_file)) source(template_render_file, local = TRUE)

  report_dir <- normalizePath(dirname(inputFile), winslash = "/", mustWork = TRUE)
  options(
    adfg_report_dir = report_dir,
    adfg_template_dir = template_dir,
    adfg_render_pipeline = "template_postprocess"
  )

  extract_report_year <- function(rmd_path) {
    lines <- readLines(rmd_path, warn = FALSE)
    yr_line <- grep("^\\s*report_year\\s*<-\\s*[0-9]{4}\\s*$", lines, value = TRUE)
    if (length(yr_line) == 0) return(NA_integer_)
    suppressWarnings(as.integer(gsub("[^0-9]", "", yr_line[[1]])))
  }

  rendered_file <- rmarkdown::render(
    normalizePath(inputFile, winslash = "/", mustWork = TRUE),
    encoding = encoding,
    output_dir = report_dir,
    output_format = adfg_word_document(
      reference_docx = normalizePath(file.path(template_dir, "my-styles-template.docx"), winslash = "/", mustWork = TRUE),
      helpers_file = template_runtime_helpers_file
    ),
    knit_root_dir = report_dir
  )

  apply_landscape_footer_refs <- function(docx_path) {
    if (!file.exists(docx_path)) return(invisible(FALSE))

    tmp_dir <- tempfile("adfg_docx_landscape_footer_")
    adfg_unzip_docx(docx_path, exdir = tmp_dir)

    document_xml <- file.path(tmp_dir, "word", "document.xml")
    rels_xml <- file.path(tmp_dir, "word", "_rels", "document.xml.rels")
    content_types_xml <- file.path(tmp_dir, "[Content_Types].xml")
    if (!file.exists(document_xml) || !file.exists(rels_xml) || !file.exists(content_types_xml)) return(invisible(FALSE))

    rels_doc <- xml2::read_xml(rels_xml)
    rel_ns <- c(rel = "http://schemas.openxmlformats.org/package/2006/relationships")
    footer_rels <- xml2::xml_find_all(
      rels_doc,
      ".//rel:Relationship[@Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer']",
      ns = rel_ns
    )
    if (length(footer_rels) == 0) return(invisible(FALSE))

    landscape_footer_id <- NA_character_
    for (node in footer_rels) {
      rel_id <- xml2::xml_attr(node, "Id")
      target <- xml2::xml_attr(node, "Target")
      footer_path <- file.path(tmp_dir, "word", gsub("/", "\\\\", target, fixed = TRUE))
      if (!file.exists(footer_path)) next
      footer_text <- paste(readLines(footer_path, warn = FALSE), collapse = "\n")
      is_landscape_footer <- grepl("w:textDirection w:val=\"tbRl\"", footer_text, fixed = TRUE)
      has_page_field <- grepl("PAGE", footer_text, fixed = TRUE)
      if (is_landscape_footer && has_page_field) {
        landscape_footer_id <- rel_id
        break
      }
    }

    if (is.na(landscape_footer_id) || !nzchar(landscape_footer_id)) {
      import_landscape_footer_from_docx <- function(source_docx) {
        if (is.na(source_docx) || !nzchar(source_docx) || !file.exists(source_docx)) return(NA_character_)

        src_dir <- tempfile("adfg_landscape_src_")
        adfg_unzip_docx(source_docx, exdir = src_dir)
        src_word <- file.path(src_dir, "word")
        if (!dir.exists(src_word)) return(NA_character_)

        footer_files <- list.files(src_word, pattern = "^footer[0-9]+\\.xml$", full.names = TRUE)
        if (length(footer_files) == 0) return(NA_character_)

        chosen_xml <- NULL
        for (ff in footer_files) {
          ftxt <- paste(readLines(ff, warn = FALSE), collapse = "\n")
          if (grepl("w:textDirection w:val=\"tbRl\"", ftxt, fixed = TRUE) && grepl("PAGE", ftxt, fixed = TRUE)) {
            chosen_xml <- ftxt
            break
          }
        }
        if (is.null(chosen_xml)) return(NA_character_)

        existing_footer_files <- list.files(file.path(tmp_dir, "word"), pattern = "^footer[0-9]+\\.xml$")
        next_footer_num <- if (length(existing_footer_files) == 0) {
          1L
        } else {
          max(suppressWarnings(as.integer(sub("^footer([0-9]+)\\.xml$", "\\1", existing_footer_files))), na.rm = TRUE) + 1L
        }
        new_footer_file <- paste0("footer", next_footer_num, ".xml")
        writeLines(chosen_xml, file.path(tmp_dir, "word", new_footer_file), useBytes = TRUE)

        existing_rel_ids <- xml2::xml_attr(xml2::xml_find_all(rels_doc, ".//rel:Relationship", ns = rel_ns), "Id")
        new_rel_id <- if (exists("next_relationship_id", mode = "function", inherits = TRUE)) {
          next_relationship_id(existing_rel_ids)
        } else {
          nums <- suppressWarnings(as.integer(sub("^rId", "", existing_rel_ids)))
          nums <- nums[!is.na(nums)]
          paste0("rId", if (length(nums) == 0) 1 else max(nums) + 1)
        }

        rel_xml <- sprintf(
          '<Relationship xmlns="%s" Id="%s" Type="%s" Target="%s"/>',
          "http://schemas.openxmlformats.org/package/2006/relationships",
          new_rel_id,
          "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer",
          new_footer_file
        )
        xml2::xml_add_child(xml2::xml_root(rels_doc), xml2::read_xml(rel_xml))

        content_types_doc <- xml2::read_xml(content_types_xml)
        if (exists("add_content_type_override", mode = "function", inherits = TRUE)) {
          add_content_type_override(
            content_types_doc,
            paste0("/word/", new_footer_file),
            "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"
          )
        }
        xml2::write_xml(content_types_doc, content_types_xml)

        new_rel_id
      }

      template_docx <- normalizePath(file.path(template_dir, "my-styles-template.docx"), winslash = "/", mustWork = FALSE)
      landscape_footer_id <- import_landscape_footer_from_docx(template_docx)

      if (is.na(landscape_footer_id) || !nzchar(landscape_footer_id)) {
        locate_blank_landscape_docx <- function() {
          user_home <- Sys.getenv("USERPROFILE", unset = path.expand("~"))
          candidates <- unique(c(
            file.path(dirname(template_dir), "RTS help documents", "blank landscape page.docx"),
            file.path(user_home, "OneDrive - State of Alaska", "Documents", "GitHub", "ADFG-FDS-template--Pfisterer", "RTS help documents", "blank landscape page.docx"),
            file.path(user_home, "Documents", "GitHub", "ADFG-FDS-template--Pfisterer", "RTS help documents", "blank landscape page.docx")
          ))
          hit <- candidates[file.exists(candidates)]
          if (length(hit) == 0) return(NA_character_)
          normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
        }

        blank_docx <- locate_blank_landscape_docx()
        if (!is.na(blank_docx)) {
          blank_dir <- tempfile("adfg_blank_landscape_")
          adfg_unzip_docx(blank_docx, exdir = blank_dir)

          blank_rels_xml <- file.path(blank_dir, "word", "_rels", "document.xml.rels")
          if (file.exists(blank_rels_xml)) {
            blank_rels_doc <- xml2::read_xml(blank_rels_xml)
            blank_footer_rels <- xml2::xml_find_all(
              blank_rels_doc,
              ".//rel:Relationship[@Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer']",
              ns = rel_ns
            )

            landscape_footer_xml <- NULL
            for (bnode in blank_footer_rels) {
              btarget <- xml2::xml_attr(bnode, "Target")
              bpath <- file.path(blank_dir, "word", gsub("/", "\\\\", btarget, fixed = TRUE))
              if (!file.exists(bpath)) next
              btxt <- paste(readLines(bpath, warn = FALSE), collapse = "\n")
              if (grepl("PAGE", btxt, fixed = TRUE) && grepl("w:textDirection w:val=\"tbRl\"", btxt, fixed = TRUE)) {
                landscape_footer_xml <- btxt
                break
              }
            }

            if (!is.null(landscape_footer_xml)) {
              existing_footer_files <- list.files(file.path(tmp_dir, "word"), pattern = "^footer[0-9]+\\.xml$")
              next_footer_num <- if (length(existing_footer_files) == 0) {
                1L
              } else {
                max(suppressWarnings(as.integer(sub("^footer([0-9]+)\\.xml$", "\\1", existing_footer_files))), na.rm = TRUE) + 1L
              }
              new_footer_file <- paste0("footer", next_footer_num, ".xml")
              writeLines(landscape_footer_xml, file.path(tmp_dir, "word", new_footer_file), useBytes = TRUE)

              existing_rel_ids <- xml2::xml_attr(xml2::xml_find_all(rels_doc, ".//rel:Relationship", ns = rel_ns), "Id")
              new_rel_id <- if (exists("next_relationship_id", mode = "function", inherits = TRUE)) {
                next_relationship_id(existing_rel_ids)
              } else {
                nums <- suppressWarnings(as.integer(sub("^rId", "", existing_rel_ids)))
                nums <- nums[!is.na(nums)]
                paste0("rId", if (length(nums) == 0) 1 else max(nums) + 1)
              }

              rel_xml <- sprintf(
                '<Relationship xmlns="%s" Id="%s" Type="%s" Target="%s"/>',
                "http://schemas.openxmlformats.org/package/2006/relationships",
                new_rel_id,
                "http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer",
                new_footer_file
              )
              xml2::xml_add_child(xml2::xml_root(rels_doc), xml2::read_xml(rel_xml))

              content_types_doc <- xml2::read_xml(content_types_xml)
              if (exists("add_content_type_override", mode = "function", inherits = TRUE)) {
                add_content_type_override(
                  content_types_doc,
                  paste0("/word/", new_footer_file),
                  "application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"
                )
              }
              xml2::write_xml(content_types_doc, content_types_xml)

              landscape_footer_id <- new_rel_id
            }
          }
        }
      }
    }

    if (is.na(landscape_footer_id) || !nzchar(landscape_footer_id)) {
      for (node in footer_rels) {
        rel_id <- xml2::xml_attr(node, "Id")
        target <- xml2::xml_attr(node, "Target")
        footer_path <- file.path(tmp_dir, "word", gsub("/", "\\\\", target, fixed = TRUE))
        if (!file.exists(footer_path)) next
        footer_text <- paste(readLines(footer_path, warn = FALSE), collapse = "\n")
        if (grepl("PAGE", footer_text, fixed = TRUE)) {
          landscape_footer_id <- rel_id
          break
        }
      }
    }

    if (is.na(landscape_footer_id) || !nzchar(landscape_footer_id)) {
      landscape_footer_id <- xml2::xml_attr(footer_rels[[1]], "Id")
    }

    if (is.na(landscape_footer_id) || !nzchar(landscape_footer_id)) return(invisible(FALSE))

    doc <- xml2::read_xml(document_xml)
    ns <- c(
      w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
      r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
    )
    sect_nodes <- xml2::xml_find_all(doc, ".//w:sectPr", ns = ns)
    if (length(sect_nodes) == 0) return(invisible(FALSE))

    for (sect in sect_nodes) {
      is_landscape_section <- !inherits(
        xml2::xml_find_first(sect, "./w:pgSz[@w:orient='landscape']", ns = ns),
        "xml_missing"
      )
      if (!is_landscape_section) next

      footer_refs <- xml2::xml_find_all(sect, "./w:footerReference", ns = ns)
      if (length(footer_refs) > 0) {
        xml2::xml_remove(footer_refs)
      }

      for (ftype in c("default", "even", "first")) {
        frag <- xml2::read_xml(
          paste0(
            '<w:footerReference xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
            'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
            'w:type="', ftype, '" r:id="', landscape_footer_id, '"/>'
          )
        )
        xml2::xml_add_child(sect, frag)
      }
    }

    xml2::write_xml(doc, document_xml)
    xml2::write_xml(rels_doc, rels_xml)
    adfg_repack_docx(tmp_dir, docx_path)
    invisible(TRUE)
  }

  apply_front_matter_page_numbering <- function(docx_path,
                                                abstract_heading = "Abstract") {
    if (!file.exists(docx_path)) return(invisible(FALSE))

    tmp_dir <- tempfile("adfg_docx_frontmatter_paging_")
    adfg_unzip_docx(docx_path, exdir = tmp_dir)

    document_xml <- file.path(tmp_dir, "word", "document.xml")
    if (!file.exists(document_xml)) return(invisible(FALSE))

    doc <- xml2::read_xml(document_xml)
    ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
    body <- xml2::xml_find_first(doc, ".//w:body", ns = ns)
    if (inherits(body, "xml_missing")) return(invisible(FALSE))

    flatten_paragraph_text <- function(p_node) {
      out <- character(0)
      children <- xml2::xml_children(p_node)
      if (length(children) == 0) return("")

      for (child in children) {
        if (!identical(xml2::xml_name(child), "r")) next

        tabs <- xml2::xml_find_all(child, ".//w:tab", ns = ns)
        if (length(tabs) > 0) out <- c(out, rep("\t", length(tabs)))

        txt <- xml2::xml_find_all(child, ".//w:t", ns = ns)
        if (length(txt) > 0) out <- c(out, xml2::xml_text(txt))
      }

      paste0(out, collapse = "")
    }

    ensure_ppr <- function(p_node) {
      ppr <- xml2::xml_find_first(p_node, "./w:pPr", ns = ns)
      if (inherits(ppr, "xml_missing")) {
        ppr <- xml2::xml_add_child(
          p_node,
          xml2::read_xml('<w:pPr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"/>'),
          .where = 0
        )
      }
      ppr
    }

    set_section_page_numbering <- function(sect_node, fmt = NULL, start = NULL, type = NULL) {
      if (inherits(sect_node, "xml_missing")) return(invisible(FALSE))

      old_type <- xml2::xml_find_all(sect_node, "./w:type", ns = ns)
      if (length(old_type) > 0) xml2::xml_remove(old_type)

      old_pg <- xml2::xml_find_all(sect_node, "./w:pgNumType", ns = ns)
      if (length(old_pg) > 0) xml2::xml_remove(old_pg)

      insert_at <- 0
      if (!is.null(type) && nzchar(type)) {
        xml2::xml_add_child(
          sect_node,
          xml2::read_xml(
            paste0(
              '<w:type xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" w:val="',
              type,
              '"/>'
            )
          ),
          .where = insert_at
        )
        insert_at <- insert_at + 1
      }

      if (!is.null(fmt) && nzchar(fmt)) {
        pg_attrs <- paste0(' w:fmt="', fmt, '"')
        if (!is.null(start) && !is.na(start)) {
          pg_attrs <- paste0(pg_attrs, ' w:start="', as.integer(start), '"')
        }
        xml2::xml_add_child(
          sect_node,
          xml2::read_xml(
            paste0(
              '<w:pgNumType xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"',
              pg_attrs,
              '/>'
            )
          ),
          .where = insert_at
        )
      }

      invisible(TRUE)
  }

    body_children <- xml2::xml_children(body)
    paragraph_children <- body_children[vapply(body_children, function(node) identical(xml2::xml_name(node), "p"), logical(1))]
    if (length(paragraph_children) == 0) return(invisible(FALSE))

    paragraph_text <- trimws(vapply(paragraph_children, flatten_paragraph_text, character(1)))
    abstract_idx <- match(abstract_heading, paragraph_text)
    if (is.na(abstract_idx) || abstract_idx <= 1) return(invisible(FALSE))

    prev_p <- paragraph_children[[abstract_idx - 1L]]

    sect_nodes <- xml2::xml_find_all(doc, ".//w:sectPr", ns = ns)
    anchor_index <- vapply(
      sect_nodes,
      function(sect) {
        parent_p <- xml2::xml_find_first(sect, "ancestor::w:p[1]", ns = ns)
        if (inherits(parent_p, "xml_missing")) {
          Inf
        } else {
          idx <- match(parent_p, paragraph_children)
          if (is.na(idx)) Inf else idx
        }
      },
      numeric(1)
    )

    next_sect_idx <- which(anchor_index >= abstract_idx)[1]
    if (is.na(next_sect_idx)) {
      next_sect <- xml2::xml_find_first(body, "./w:sectPr", ns = ns)
    } else {
      next_sect <- sect_nodes[[next_sect_idx]]
    }
    if (inherits(next_sect, "xml_missing")) return(invisible(FALSE))

    prev_ppr <- ensure_ppr(prev_p)
    prev_sect <- xml2::xml_find_first(prev_ppr, "./w:sectPr", ns = ns)
    if (inherits(prev_sect, "xml_missing")) {
      prev_sect <- xml2::xml_add_child(prev_ppr, xml2::read_xml(as.character(next_sect)))
    }

    set_section_page_numbering(prev_sect, fmt = "lowerRoman", start = 1L, type = "nextPage")
    set_section_page_numbering(next_sect, fmt = "decimal", start = 1L)

    xml2::write_xml(doc, document_xml)
    adfg_repack_docx(tmp_dir, docx_path)
    invisible(TRUE)
  }

  apply_table_footnote_paragraph_style <- function(docx_path,
                                                   style_id = "Table-Footnote") {
    if (!file.exists(docx_path)) return(invisible(FALSE))

    tmp_dir <- tempfile("adfg_docx_table_footnotes_")
    adfg_unzip_docx(docx_path, exdir = tmp_dir)

    document_xml <- file.path(tmp_dir, "word", "document.xml")
    if (!file.exists(document_xml)) return(invisible(FALSE))

    doc <- xml2::read_xml(document_xml)
    ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

    flatten_paragraph_text <- function(p_node) {
      out <- character(0)
      children <- xml2::xml_children(p_node)
      if (length(children) == 0) return("")

      for (child in children) {
        nm <- xml2::xml_name(child)
        if (!identical(nm, "r")) next

        tabs <- xml2::xml_find_all(child, ".//w:tab", ns = ns)
        if (length(tabs) > 0) {
          out <- c(out, rep("\t", length(tabs)))
        }

        txt <- xml2::xml_find_all(child, ".//w:t", ns = ns)
        if (length(txt) > 0) {
          out <- c(out, xml2::xml_text(txt))
        }
      }

      paste0(out, collapse = "")
    }

    is_table_footnote_paragraph <- function(p_node) {
      para_text <- trimws(flatten_paragraph_text(p_node))
      if (!nzchar(para_text)) return(FALSE)

      starts_note <- grepl("^Note:\\s", para_text)
      starts_marker <- grepl("^[a-z]{1,2}\\t", para_text)
      starts_note || starts_marker
    }

    table_paragraphs <- xml2::xml_find_all(doc, ".//w:tbl//w:tc//w:p", ns = ns)
    if (length(table_paragraphs) == 0) return(invisible(FALSE))

    style_xml_escape <- function(x) {
      x <- as.character(x)
      x <- gsub("&", "&amp;", x, fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x
    }

    style_id_xml <- style_xml_escape(style_id)
    updated <- 0L
    for (p in table_paragraphs) {
      if (!is_table_footnote_paragraph(p)) next

      ppr <- xml2::xml_find_first(p, "./w:pPr", ns = ns)
      if (inherits(ppr, "xml_missing")) {
        ppr <- xml2::xml_add_child(p, xml2::read_xml('<w:pPr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"/>'), .where = 0)
      }

      existing_pstyles <- xml2::xml_find_all(ppr, "./w:pStyle", ns = ns)
      if (length(existing_pstyles) > 0) {
        xml2::xml_remove(existing_pstyles)
      }

      xml2::xml_add_child(
        ppr,
        xml2::read_xml(
          paste0(
            '<w:pStyle xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" w:val="',
            style_id_xml,
            '"/>'
          )
        ),
        .where = 0
      )

      updated <- updated + 1L
    }

    if (updated > 0L) {
      xml2::write_xml(doc, document_xml)
      adfg_repack_docx(tmp_dir, docx_path)
    }

    message("Table footnote style post-process updated ", updated, " paragraph(s) to ", style_id, ".")
    invisible(updated)
  }

  inline_table_footnote_section_breaks <- function(docx_path,
                                                   style_id = "Table-Footnote") {
    if (!file.exists(docx_path)) return(invisible(FALSE))

    tmp_dir <- tempfile("adfg_docx_inline_footnote_sectpr_")
    adfg_unzip_docx(docx_path, exdir = tmp_dir)

    document_xml <- file.path(tmp_dir, "word", "document.xml")
    if (!file.exists(document_xml)) return(invisible(FALSE))

    doc <- xml2::read_xml(document_xml)
    ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
    body <- xml2::xml_find_first(doc, ".//w:body", ns = ns)
    if (inherits(body, "xml_missing")) return(invisible(FALSE))

    is_blank_break_paragraph <- function(p_node) {
      sect <- xml2::xml_find_first(p_node, "./w:pPr/w:sectPr", ns = ns)
      if (inherits(sect, "xml_missing")) return(FALSE)

      para_text <- paste0(xml2::xml_text(xml2::xml_find_all(p_node, ".//w:t", ns = ns)), collapse = "")
      if (nzchar(trimws(para_text))) return(FALSE)

      runs <- xml2::xml_find_all(p_node, "./w:r", ns = ns)
      if (length(runs) == 0) return(TRUE)

      all_run_children <- unlist(lapply(runs, xml2::xml_children), recursive = FALSE)
      child_names <- vapply(all_run_children, xml2::xml_name, character(1))
      all(child_names %in% c("br", "lastRenderedPageBreak"))
    }

    has_style <- function(p_node, target_style) {
      pstyle <- xml2::xml_find_first(p_node, "./w:pPr/w:pStyle", ns = ns)
      !inherits(pstyle, "xml_missing") && identical(xml2::xml_attr(pstyle, "val", ns = ns), target_style)
    }

    updated <- 0L
    body_children <- xml2::xml_children(body)
    for (idx in seq_along(body_children)) {
      node <- body_children[[idx]]
      if (!identical(xml2::xml_name(node), "p")) next
      if (!is_blank_break_paragraph(node)) next
      if (idx <= 1L) next

      prev_node <- body_children[[idx - 1L]]
      if (!identical(xml2::xml_name(prev_node), "p")) next
      if (!has_style(prev_node, style_id)) next

      prev_ppr <- xml2::xml_find_first(prev_node, "./w:pPr", ns = ns)
      if (inherits(prev_ppr, "xml_missing")) {
        prev_ppr <- xml2::xml_add_child(
          prev_node,
          xml2::read_xml('<w:pPr xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"/>'),
          .where = 0
        )
      }

      old_prev_sect <- xml2::xml_find_all(prev_ppr, "./w:sectPr", ns = ns)
      if (length(old_prev_sect) > 0) xml2::xml_remove(old_prev_sect)

      sect <- xml2::xml_find_first(node, "./w:pPr/w:sectPr", ns = ns)
      xml2::xml_add_child(prev_ppr, xml2::read_xml(as.character(sect)))
      xml2::xml_remove(node)
      updated <- updated + 1L
    }

    xml2::write_xml(doc, document_xml)
    adfg_repack_docx(tmp_dir, docx_path)
    invisible(updated > 0L)
  }

  apply_rmd_figure_alt_text <- function(docx_path, rmd_path) {
    if (!file.exists(docx_path) || !file.exists(rmd_path)) return(invisible(FALSE))

    rmd_lines <- readLines(rmd_path, warn = FALSE, encoding = "UTF-8")
    alt_lines <- grep("fig\\.alt\\s*=", rmd_lines, value = TRUE)
    if (length(alt_lines) == 0) return(invisible(FALSE))

    extract_alt <- function(line) {
      match <- regexec("fig\\.alt\\s*=\\s*([\\\"'])(.*?)\\1", line, perl = TRUE)
      parts <- regmatches(line, match)[[1]]
      if (length(parts) < 3) return(NA_character_)
      parts[[3]]
    }

    alt_text <- vapply(alt_lines, extract_alt, character(1))
    alt_text <- alt_text[!is.na(alt_text) & nzchar(trimws(alt_text))]
    if (length(alt_text) == 0) return(invisible(FALSE))

    tmp_dir <- tempfile("adfg_docx_figure_alt_")
    adfg_unzip_docx(docx_path, exdir = tmp_dir)
    document_xml <- file.path(tmp_dir, "word", "document.xml")
    if (!file.exists(document_xml)) return(invisible(FALSE))

    doc <- xml2::read_xml(document_xml)
    ns <- c(wp = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing")
    doc_pr <- xml2::xml_find_all(doc, ".//wp:docPr", ns = ns)
    if (length(doc_pr) == 0) return(invisible(FALSE))

    n <- min(length(doc_pr), length(alt_text))
    for (idx in seq_len(n)) {
      xml2::xml_set_attr(doc_pr[[idx]], "descr", alt_text[[idx]])
    }

    xml2::write_xml(doc, document_xml)
    adfg_repack_docx(tmp_dir, docx_path)
    message("DOCX figure alt text post-process updated ", n, " figure description(s).")
    invisible(n > 0L)
  }

  if (grepl("\\.docx$", rendered_file, ignore.case = TRUE) && file.exists(rendered_file)) {
    tryCatch(
      apply_front_matter_page_numbering(rendered_file),
      error = function(e) message("Front-matter page numbering post-process skipped: ", conditionMessage(e))
    )
      tryCatch(
        apply_rmd_figure_alt_text(rendered_file, normalizePath(inputFile, winslash = "/", mustWork = TRUE)),
        error = function(e) message("Figure alt-text post-process skipped: ", conditionMessage(e))
      )
    tryCatch(
      apply_landscape_footer_refs(rendered_file),
      error = function(e) message("Landscape footer post-process skipped: ", conditionMessage(e))
    )
    tryCatch(
      apply_table_footnote_paragraph_style(rendered_file, style_id = "Table-Footnote"),
      error = function(e) message("Table footnote style post-process skipped: ", conditionMessage(e))
    )
    tryCatch(
      inline_table_footnote_section_breaks(rendered_file, style_id = "Table-Footnote"),
      error = function(e) message("Table footnote section-break post-process skipped: ", conditionMessage(e))
    )
  }

  report_year <- extract_report_year(normalizePath(inputFile, winslash = "/", mustWork = TRUE))
  if (!is.na(report_year) && file.exists(rendered_file)) {
    generic_docx <- file.path(dirname(rendered_file), paste0("FDS_Report_", report_year, ".docx"))
    file.copy(rendered_file, generic_docx, overwrite = TRUE)
    message("Created year-based report copy: ", generic_docx)
  }

  rendered_file
}
