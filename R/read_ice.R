#' Read a .ice file
#'
#' Reads a `.ice` archive and returns its components: data tables as
#' `data.table` objects and scalar components as plain vectors. The
#' archive's metadata is attached as the `metadata` attribute and drives
#' column types: key/value columns are parsed according to the types
#' recorded in `metadata.yaml`, so e.g. character keys with leading zeros
#' survive and the value column of every table declared numeric or
#' integer is ALWAYS a double, whatever fread's type inference would have
#' guessed (the read side of the numeric round-trip guarantee).
#'
#' Components are returned in metadata order (tables first, then
#' scalars). Data members present in the archive but missing from the
#' metadata are appended at the end with a warning; members declared in
#' the metadata but missing from the archive warn and are skipped (use
#' [validate_ice()] to make either fatal).
#'
#' @param ice_file Path to the .ice file.
#' @return A list of class `"ice"`: tables as `data.table`s (each with
#'   its table metadata attached as attribute `metadata`), scalars as
#'   length-1 vectors. Attributes `metadata` (full metadata list) and
#'   `filename` are set on the list.
#' @examples
#' \dontrun{
#' ice <- read_ice("example.ice")
#' print(ice)
#' ice$trade_flows            # a data.table
#' attr(ice, "metadata")      # full metadata
#' }
#' @export
read_ice <- function(ice_file) {
  if (!file.exists(ice_file)) {
    cli::cli_abort("File does not exist: {.path {ice_file}}")
  }

  temp_dir <- tempfile("read_ice")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # internal untar first (pure R error conditions, reads ustar/gnu/pax);
  # fall back to the system tar for exotic archives
  extracted <- tryCatch(
    { utils::untar(ice_file, exdir = temp_dir, tar = "internal"); TRUE },
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (!extracted) {
    status <- suppressWarnings(tryCatch(
      utils::untar(ice_file, exdir = temp_dir),
      error = function(e) 1L
    ))
    if (!identical(status, 0L)) {
      cli::cli_abort("{.path {ice_file}} is not a readable .ice archive (tar extraction failed).")
    }
  }

  meta_path <- file.path(temp_dir, "metadata.yaml")
  if (!file.exists(meta_path)) {
    cli::cli_abort("{.path {ice_file}} is not a valid .ice archive: no {.file metadata.yaml} member.")
  }
  metadata <- yaml::read_yaml(meta_path)

  on_disk <- list.files(temp_dir, pattern = "\\.csv$")
  table_members <- setdiff(on_disk, "scalars.csv")

  # metadata order is authoritative; extra on-disk members come last
  declared <- names(metadata$tables) %||% character(0)
  missing_members <- setdiff(paste0(declared, ".csv"), table_members)
  if (length(missing_members)) {
    cli::cli_warn("{.file {missing_members}} declared in metadata but missing from {.path {ice_file}}; skipped.")
  }
  extras <- setdiff(table_members, paste0(declared, ".csv"))
  if (length(extras) && length(declared)) {
    cli::cli_warn("{.file {extras}} in archive but undeclared in metadata; appended as-is.")
  }
  ordered <- c(intersect(paste0(declared, ".csv"), table_members), extras)

  tables <- list()
  for (member in ordered) {
    table_name <- tools::file_path_sans_ext(member)
    table_meta <- metadata$tables[[table_name]]
    tables[[table_name]] <- read_ice_table(file.path(temp_dir, member), table_meta)
    if (!is.null(table_meta)) {
      data.table::setattr(tables[[table_name]], "metadata", table_meta)
    }
  }

  # scalar components (format >= 1.1, issue #3)
  if ("scalars.csv" %in% on_disk) {
    scalar_dt <- data.table::fread(file.path(temp_dir, "scalars.csv"),
                                   colClasses = "character")
    for (i in seq_len(nrow(scalar_dt))) {
      scalar_name <- scalar_dt$name[i]
      declared_type <- metadata$scalars[[scalar_name]]$type
      tables[[scalar_name]] <- coerce_scalar(scalar_dt$value[i], declared_type)
    }
  }

  data.table::setattr(tables, "metadata", metadata)
  data.table::setattr(tables, "filename", ice_file)
  class(tables) <- "ice"
  tables
}

# Read one table member with metadata-driven types (issues #6, #11):
# - declared character/factor columns are read as character, protecting
#   codes like "01" from integer inference
# - other columns use fread's inference (never integer64), then are
#   coerced to their declared type
# - the value column of a table declared numeric/integer is always a
#   double; without metadata, the pre-1.1 heuristic applies (a column
#   literally named "value" is coerced to double)
read_ice_table <- function(csv_file, table_meta) {
  char_cols <- character(0)
  col_types <- list()
  if (!is.null(table_meta)) {
    specs <- c(table_meta$keys, list(table_meta$value))
    for (spec in specs) {
      if (is.null(spec$name)) next
      col_types[[spec$name]] <- spec$type %||% NA_character_
      if (identical(metadata_type_to_colclass(spec$type %||% ""), "character")) {
        char_cols <- c(char_cols, spec$name)
      }
    }
  }

  header <- names(data.table::fread(csv_file, nrows = 0L))
  char_cols <- intersect(char_cols, header)
  table <- data.table::fread(
    csv_file,
    integer64 = "double",
    colClasses = if (length(char_cols)) stats::setNames(rep("character", length(char_cols)), char_cols)
  )

  if (!is.null(table_meta)) {
    value_name <- table_meta$value$name
    for (col in intersect(names(col_types), names(table))) {
      type <- col_types[[col]]
      current <- table[[col]]
      coerced <- switch(type,
        numeric = ,
        double = ,
        integer64 = suppressWarnings(as.numeric(current)),
        # integer VALUE columns are normalized to double by the format's
        # always-double guarantee; integer keys stay integer
        integer = if (identical(col, value_name)) as.numeric(current) else as.integer(current),
        logical = as.logical(current),
        NULL
      )
      if (!is.null(coerced)) data.table::set(table, j = col, value = coerced)
    }
  } else if ("value" %in% names(table)) {
    # pre-1.1 fallback: numeric round-trip guarantee for the conventional
    # value column name (coercion warnings intentionally visible)
    data.table::set(table, j = "value", value = as.numeric(table[["value"]]))
  }

  table
}
