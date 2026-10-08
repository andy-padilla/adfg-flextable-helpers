required_packages <- c("flextable", "officer")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages first: ", paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Run this example with Rscript examples/adfg_flextable_helpers_example.R", call. = FALSE)
}

script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
source(file.path(repo_root, "functions", "adfg_flextable_helpers.R"))

example_data <- data.frame(
  Community = c("Koyuk", "Nulato", "Total"),
  Harvest = c(1250, 980, 2230),
  stringsAsFactors = FALSE
)
numeric_cols <- "Harvest"

ft <- flextable::flextable(example_data)
ft <- adfg_ft_apply_table_guardrails(
  ft,
  left_cols = "Community",
  right_cols = numeric_cols,
  header_center_cols = numeric_cols,
  font_size = 9,
  word_title = "Illustrative subsistence harvest table",
  word_description = "Illustrative harvest estimates for three rows."
)
ft <- adfg_ft_apply_context_borders(
  ft,
  row_labels = example_data$Community,
  numeric_cols = numeric_cols
)
note <- "All values in this example are fictional."
ft <- adfg_ft_add_footer_notes(
  ft,
  note_lines = note,
  marker_map = stats::setNames(list("a"), note),
  base_note = "ND indicates no data."
)

output_path <- if (length(commandArgs(trailingOnly = TRUE)) > 0L) {
  commandArgs(trailingOnly = TRUE)[[1]]
} else {
  file.path(tempdir(), "adfg_flextable_helpers_example.docx")
}
output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, value = ft)
print(doc, target = output_path)
message("Wrote example document: ", output_path)