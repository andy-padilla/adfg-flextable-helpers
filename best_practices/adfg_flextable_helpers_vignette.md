# ADF&G Flextable Helpers

## Where the helpers live

- [functions/adfg_flextable_helpers.R](../functions/adfg_flextable_helpers.R) contains the shared flextable helpers: table guardrails, alignment and borders, footer notes, markers, width handling, and Word table/page-break support.
- [report/fds_annual/Rcode/fds_report_helpers.R](../report/fds_annual/Rcode/fds_report_helpers.R) contains FDS report captions, continuation handling, and report-specific helpers.
- [report/Rcode/template_report_helpers.R](../report/Rcode/template_report_helpers.R) contains shared report caption and theme helpers.
- [report/Rcode/figure_builders/fds_report_figures.R](../report/Rcode/figure_builders/fds_report_figures.R) contains appendix figure data-loading and year-label helpers.
- [functions/theme_adfg.R](../functions/theme_adfg.R) contains the `theme_adfg*` plotting themes.

The report pipeline sources these files; they are not currently exposed through
an installable R package (`DESCRIPTION`/`NAMESPACE` are not present). For a
reproducible example, see the executable
[R Markdown vignette](adfg_flextable_helpers_vignette.Rmd) and the standalone
[R script](../examples/adfg_flextable_helpers_example.R). Both use fictional
data and do not depend on report inputs.

The standalone example can be run from the repository root with:

```sh
Rscript examples/adfg_flextable_helpers_example.R
```

It requires the R packages `flextable` and `officer` and writes the DOCX to
`tempdir()` by default. Pass a file path as the first argument to choose another
output location.

Render the vignette as a Word document with:

```r
rmarkdown::render(
  "best_practices/adfg_flextable_helpers_vignette.Rmd",
  output_file = "adfg_flextable_helpers_vignette.docx",
  output_dir = tempdir()
)
```

## Core Pattern

Use this sequence in each table builder:

1. Build the flextable structure (columns, headers, widths).
2. Apply shared guardrails.
3. Apply table-specific contextual borders (subtotal/total logic).
4. Add standardized footer notes.
5. Return the flextable object.

## Main Helper Functions

### adfg_ft_apply_table_guardrails

Applies common typography, alignment, row heights, and header rules.

Typical usage:

```r
ft <- adfg_ft_apply_table_guardrails(
  ft,
  left_cols = "Community",
  right_cols = numeric_cols,
  header_center_cols = numeric_cols,
  header_center_rows = 1:2,
  header_rule_rows = 2,
  header_rule_cols = numeric_cols,
  border = officer::fp_border(width = 0.5),
  font_size = 9,
  line_space = 1.0,
  padding = 2
)
```

### adfg_ft_apply_context_borders

Draws subtotal/total separator rules based on row labels.

Typical usage:

```r
ft <- adfg_ft_apply_context_borders(
  ft,
  row_labels = rows_df$Community,
  numeric_cols = numeric_cols,
  border = officer::fp_border(width = 0.5)
)
```

### adfg_ft_add_footer_notes

Adds standardized footer notes, including superscript marker formatting and base note.

Typical usage:

```r
ft <- adfg_ft_add_footer_notes(
  ft,
  note_lines = note_lines,
  marker_map = marker_map,
  base_note = "NA indicates not applicable. ND indicates no data.",
  footer_font_size = 9
)
```

### adfg_table_emit_continued_rule

Emits continued-page marker, page break, or continued caption from one rule function.

Actions:
- marker
- pagebreak
- caption

Typical usage:

```r
adfg_table_emit_continued_rule(has_split, table_num = 2, action = "marker")
adfg_table_emit_continued_rule(has_split, table_num = 2, action = "pagebreak")
adfg_table_emit_continued_rule(has_split, table_num = 2, action = "caption")
```

## Recent Style Decision

Vertical group borders were replaced with narrow spacer rows for visual separation where needed.

Reason:
- Avoid heavy visual dividers while preserving readability.
- Keep the layout compatible with installed flextable versions.

## Compatibility Note

Some flextable APIs vary by version. Prefer helper methods that avoid version-specific arguments unless tested in this repo environment.

## Maintenance Checklist

When adding or changing table style rules:

1. Update helpers first.
2. Keep table builders thin and helper-driven.
3. Run updated estimates pipeline.
4. Verify captions, continued sections, and footer notes in the DOCX output.
5. Record any version-specific workaround in this vignette.
