#' Build a manifest of .ice archives
#'
#' Scans directories (and/or explicit file paths) for `.ice` archives and
#' assembles a manifest with one row per archive: file facts (size,
#' SHA-256 of the archive) and key metadata fields extracted from each
#' archive's `metadata.yaml`. Damaged or incompatible archives are
#' included with `status = "error"` and the error message, so a library
#' scan never fails on a single bad file.
#'
#' The archive-level SHA-256 lives here, outside the archive (an archive
#' cannot embed its own hash); per-member checksums live inside
#' `metadata.yaml` (see [generate_ice()]).
#'
#' @param paths Character vector of directories to scan and/or `.ice`
#'   file paths.
#' @param output_csv Optional path; the manifest is written there as CSV.
#' @param output_yaml Optional path; the manifest is written there as
#'   YAML (one record per artifact).
#' @param recursive Scan directories recursively (default `FALSE`).
#' @return A `data.table` with columns `artifact`, `path`, `size_bytes`,
#'   `sha256`, `format_version`, `identifier`, `title`, `description`,
#'   `version`, `timestamp`, `n_tables`, `n_scalars`, `status`, `error`.
#' @examples
#' \dontrun{
#' manifest <- build_manifest("output/initial_conditions",
#'                            output_csv = "manifest.csv",
#'                            output_yaml = "manifest.yaml")
#' }
#' @export
build_manifest <- function(paths, output_csv = NULL, output_yaml = NULL,
                           recursive = FALSE) {
  files <- character(0)
  for (p in paths) {
    if (dir.exists(p)) {
      files <- c(files, list.files(p, pattern = "\\.ice$",
                                   full.names = TRUE, recursive = recursive))
    } else if (file.exists(p)) {
      files <- c(files, p)
    } else {
      cli::cli_warn("{.path {p}} does not exist; skipped.")
    }
  }
  files <- unique(normalizePath(files))

  rows <- lapply(files, function(f) {
    row <- data.table::data.table(
      artifact = basename(f),
      path = f,
      size_bytes = file.info(f)$size,
      sha256 = sha256_file(f),
      format_version = NA_character_,
      identifier = NA_character_,
      title = NA_character_,
      description = NA_character_,
      version = NA_character_,
      timestamp = NA_character_,
      n_tables = NA_integer_,
      n_scalars = NA_integer_,
      status = "ok",
      error = NA_character_
    )
    md <- tryCatch(read_ice_metadata(f), error = function(e) e)
    if (inherits(md, "error")) {
      row$status <- "error"
      row$error <- conditionMessage(md)
      return(row)
    }
    scalar_field <- function(x) {
      if (is.null(x) || !is.atomic(x) || !length(x)) NA_character_ else as.character(x[1])
    }
    row$format_version <- scalar_field(md$format_version) %||% "1.0"
    if (is.na(row$format_version)) row$format_version <- "1.0"
    row$identifier <- scalar_field(md$identifier)
    row$title <- scalar_field(md$title)
    row$description <- scalar_field(md$description)
    row$version <- scalar_field(md$version)
    row$timestamp <- scalar_field(md$timestamp)
    row$n_tables <- length(md$tables)
    row$n_scalars <- length(md$scalars)
    row
  })

  manifest <- if (length(rows)) {
    data.table::rbindlist(rows)
  } else {
    data.table::data.table(
      artifact = character(0), path = character(0), size_bytes = numeric(0),
      sha256 = character(0), format_version = character(0),
      identifier = character(0), title = character(0),
      description = character(0), version = character(0),
      timestamp = character(0), n_tables = integer(0),
      n_scalars = integer(0), status = character(0), error = character(0)
    )
  }
  data.table::setorderv(manifest, "artifact")

  if (!is.null(output_csv)) data.table::fwrite(manifest, output_csv)
  if (!is.null(output_yaml)) {
    records <- lapply(seq_len(nrow(manifest)), function(i) {
      rec <- as.list(manifest[i])
      rec[!vapply(rec, function(v) is.na(v), logical(1))]
    })
    names(records) <- manifest$artifact
    yaml::write_yaml(records, output_yaml)
  }

  manifest[]
}
