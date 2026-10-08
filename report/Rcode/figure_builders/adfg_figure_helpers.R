adfg_find_appendix_duckdb_path <- function(db_path = NULL) {
  if (!is.null(db_path) && length(db_path) > 0L) {
    candidate <- as.character(db_path[[1]])
    if (!is.na(candidate) && nzchar(candidate)) {
      return(if (file.exists(candidate)) candidate else NA_character_)
    }
  }

  candidates <- c(
    here::here("output", "harvest_estimates_comparison.duckdb"),
    here::here("harvest_estimates_comparison.duckdb"),
    here::here("output", "Harvest_estimates_database_UI", "harvest_estimates_comparison.duckdb")
  )

  hits <- candidates[file.exists(candidates)]
  if (length(hits) == 0L) return(NA_character_)
  hits[[1]]
}

adfg_load_nonsalmon_duckdb <- function(start_year = 1993L, db_path = NULL) {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE)) {
    return(NULL)
  }

  db_path <- adfg_find_appendix_duckdb_path(db_path)
  if (is.na(db_path)) return(NULL)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  qry <- paste(
    "SELECT",
    "  CAST(year AS INTEGER) AS year,",
    "  CAST(species AS TEXT) AS species_code,",
    "  SUM(CAST(harvest AS DOUBLE)) AS harvest",
    "FROM harvest_estimates",
    "WHERE year IS NOT NULL",
    "  AND CAST(year AS INTEGER) >= ?",
    "  AND species IN ('PIKE','SHEE','SMWF','LGWF','HWFH','BWHF','WFH','CISCO')",
    "GROUP BY 1, 2",
    "ORDER BY 1, 2"
  )

  out <- tryCatch(
    DBI::dbGetQuery(con, qry, params = list(as.integer(start_year))),
    error = function(e) NULL
  )

  if (is.null(out) || !is.data.frame(out) || nrow(out) == 0) return(NULL)

  dplyr::mutate(
    out,
    species = dplyr::case_when(
      species_code == "SHEE" ~ "Sheefish",
      species_code == "PIKE" ~ "Pike",
      species_code %in% c("SMWF", "LGWF", "HWFH", "BWHF", "WFH", "CISCO") ~ "Whitefish",
      TRUE ~ as.character(species_code)
    ),
    species_display = dplyr::case_when(
      species_code == "SHEE" ~ "Sheefish",
      species_code == "PIKE" ~ "Pike",
      species_code == "SMWF" ~ "Small Whitefish",
      species_code == "LGWF" ~ "Large Whitefish",
      species_code == "HWFH" ~ "Humpback Whitefish",
      species_code == "BWHF" ~ "Broad Whitefish",
      species_code == "WFH" ~ "Whitefish (General)",
      species_code == "CISCO" ~ "Cisco",
      TRUE ~ as.character(species_code)
    )
  )
}

adfg_appendix_nonsalmon_year_range <- function(nonsalmon_data = NULL,
                                               start_year = 1993L,
                                               db_path = NULL) {
  ns <- nonsalmon_data
  if (is.null(ns) || !is.data.frame(ns) || nrow(ns) == 0) {
    ns <- adfg_load_nonsalmon_duckdb(start_year = start_year, db_path = db_path)
  }
  if (is.null(ns) || !is.data.frame(ns) || nrow(ns) == 0 || !("year" %in% names(ns))) {
    return(c(NA_integer_, NA_integer_))
  }

  yrs <- suppressWarnings(as.integer(ns$year))
  yrs <- yrs[!is.na(yrs)]
  if (length(yrs) == 0) return(c(NA_integer_, NA_integer_))

  c(min(yrs), max(yrs))
}

adfg_format_year_span <- function(year_range, fallback = "1993–present") {
  if (length(year_range) != 2 || any(is.na(year_range))) return(fallback)
  paste0(year_range[[1]], "–", year_range[[2]])
<<<<<<< HEAD
}
=======
}
>>>>>>> origin/main
