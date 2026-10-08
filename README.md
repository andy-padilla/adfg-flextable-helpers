# ADF&G Flextable and Report Helpers

This repository is a source collection of reusable ADF&G-style table and report
helpers. It is not currently an installable R package. The modules are kept in
their report-oriented source layout because some functions share names and
assume a particular report/template runtime.

## Contents

- `functions/adfg_flextable_helpers.R`: flextable formatting, accessibility,
  borders, notes, and Word layout helpers.
- `report/fds_annual/Rcode/fds_report_helpers.R`: consolidated FDS report
  captions, continuation/page-break helpers, and the FDS helper loader.
- `report/Rcode/template_report_helpers.R`: minimal generic report-template
  caption and formatting helpers. It overlaps names in the FDS helper module;
  source the module appropriate to your report, not both into one environment.
- `report/fds_annual/Rcode/fds_subsistence_knit_wrapper.R`: optional FDS
  template integration. It expects the separate ADF&G report-template project
  and is not a standalone renderer.
- `report/Rcode/figure_builders/adfg_figure_helpers.R`: DuckDB lookup,
  nonsalmon summary loading, and year-span formatting. The DuckDB file is
  supplied by the user and is never included here.
- `functions/theme_adfg.R`: plot themes and the associated palette utility.
- `examples/adfg_flextable_helpers_example.R` and
  `best_practices/adfg_flextable_helpers_vignette.Rmd`: synthetic-data table
  example and executable Word vignette.

No subsistence data, report output, or ADF&G Word template is included.

## Requirements

The table example needs R plus `flextable` and `officer`. The vignette also
needs `rmarkdown` and `knitr`. Optional modules use `ggplot2`, `viridisLite`,
`DBI`, `duckdb`, `dplyr`, and `here`. The FDS rendering wrapper additionally
requires its matching external ADF&G template project and its dependencies.

## Try the table helpers

From the repository root:

```sh
Rscript examples/adfg_flextable_helpers_example.R
```

The example writes a DOCX under R's temporary directory. Supply a path as its
first argument to select another destination. To render the full vignette, see
`best_practices/adfg_flextable_helpers_vignette.md`.

## License

GPL-3.0-only. See `LICENSE` for the SPDX identifier and the complete license
text link.