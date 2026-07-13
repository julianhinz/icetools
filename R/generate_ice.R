#' Generate a .ice file
#'
#' Writes a list of components (data tables and scalars) plus their
#' metadata into a `.ice` archive: a gzip-compressed tar containing one
#' CSV per table, a `scalars.csv` member if any scalar components are
#' present, and a `metadata.yaml`. See `docs/ice-schema.md` for the
#' format specification.
#'
#' Numeric columns are serialized so that reading the file back with
#' [read_ice()] reproduces every double bit-for-bit (verified at write
#' time): shortest round-trip decimal strings, with a whole-column
#' fallback to C99 hex-float literals for columns the platform's decimal
#' parsers cannot round-trip (rare; see the schema document).
#'
#' @param tables A named list of components: data.frames/data.tables
#'   (last column = value column) and/or length-1 scalars. Names must be
#'   unique and archive-safe; see [create_metadata()].
#' @param metadata Metadata list, typically from [create_metadata()].
#'   Component entries should match `tables`; a mismatch triggers a
#'   warning (and a validation failure if `validate = TRUE`). Extra
#'   top-level fields (e.g. `coordinates`, `provenance`) are written
#'   verbatim.
#' @param output_file Path where the `.ice` file is written (the
#'   extension is appended if missing). Parent directories are created
#'   as needed.
#' @param validate If `TRUE`, the written file is read back and checked
#'   with [validate_ice()] before returning (read-after-write
#'   validation). Default `FALSE`.
#' @param checksums If `TRUE` (default), a `checksums` section with the
#'   SHA-256 of every data member is written into `metadata.yaml`.
#' @return Invisibly, the path to the written file.
#' @examples
#' tables <- list(
#'   trade_flows = data.frame(country = c("USA", "CHN"), value = c(100, 200)),
#'   tariffs = data.frame(country = c("USA", "CHN"), value = c(0.1, 0.2))
#' )
#' metadata <- create_metadata(tables)
#' out <- file.path(tempdir(), "example.ice")
#' generate_ice(tables, metadata, out)
#' @export
generate_ice <- function(tables, metadata, output_file,
                         validate = FALSE, checksums = TRUE) {
  if (!is.list(tables) || is.data.frame(tables) || !is.list(metadata)) {
    cli::cli_abort("Both 'tables' and 'metadata' must be lists.")
  }
  components <- split_components(tables)

  # metadata drift is a warning here; validate = TRUE makes it fatal
  declared <- c(names(metadata$tables), names(metadata$scalars))
  actual <- names(tables)
  if (!setequal(declared, actual) && length(declared)) {
    cli::cli_warn(c(
      "Component names in 'metadata' do not match 'tables'.",
      "*" = if (length(setdiff(actual, declared)))
        "Missing from metadata: {.val {setdiff(actual, declared)}}",
      "*" = if (length(setdiff(declared, actual)))
        "Only in metadata: {.val {setdiff(declared, actual)}}"
    ))
  }

  # stage all members in a temp directory
  staging <- tempfile("generate_ice")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE), add = TRUE)

  for (table_name in names(components$tables)) {
    table <- data.table::as.data.table(components$tables[[table_name]])
    # normalize for serialization: factors to character, doubles to
    # verified round-trip strings (the write side of the numeric
    # round-trip guarantee, issue #10)
    for (col in names(table)) {
      if (is.factor(table[[col]])) {
        data.table::set(table, j = col, value = as.character(table[[col]]))
      } else if (is.double(table[[col]]) && !inherits(table[[col]], c("Date", "POSIXt"))) {
        data.table::set(table, j = col, value = format_double_shortest(table[[col]]))
      }
    }
    data.table::fwrite(table,
                       file = file.path(staging, paste0(table_name, ".csv")),
                       scipen = 999)
  }

  if (length(components$scalars)) {
    serialize_scalar <- function(x) {
      if (is.double(x)) format_double_shortest(x) else as.character(x)
    }
    scalar_dt <- data.table::data.table(
      name = names(components$scalars),
      value = vapply(components$scalars, serialize_scalar, character(1))
    )
    data.table::fwrite(scalar_dt, file = file.path(staging, "scalars.csv"))
  }

  # trade_elasticity sign convention (format 1.2, issue #15): KITE-dev
  # #167 standardized on positive theta, but pre-1.2 files carry legacy
  # negative values with nothing recording which is which. Auto-detect
  # from the data unless the caller declared it explicitly.
  theta <- theta_values(components)
  if (!is.null(theta) && is.null(metadata$conventions$trade_elasticity)) {
    detected <- theta_convention(theta)
    if (is.na(detected)) {
      cli::cli_warn("trade_elasticity values have mixed or zero signs; no sign convention recorded in the metadata.")
    } else {
      metadata$conventions$trade_elasticity <- detected
    }
  }

  # stamp format/generator info and member checksums on a copy, leaving
  # the caller's metadata object untouched
  metadata$format_version <- metadata$format_version %||% ICE_FORMAT_VERSION
  metadata$generator <- metadata$generator %||% list()
  metadata$generator$icetools_version <- as.character(utils::packageVersion("icetools"))
  if (checksums) {
    members <- sort(list.files(staging, pattern = "\\.csv$"))
    metadata$checksums <- stats::setNames(
      lapply(file.path(staging, members), sha256_file), members)
  }
  yaml::write_yaml(metadata, file = file.path(staging, "metadata.yaml"))

  # ensure .ice extension
  if (!grepl("\\.ice$", output_file)) output_file <- paste0(output_file, ".ice")

  # build the archive from the staging directory with the portable
  # internal tar implementation; no setwd() leakage on error (issue #6)
  temp_tar <- tempfile(fileext = ".ice")
  on.exit(unlink(temp_tar), add = TRUE)
  with_dir(staging, {
    utils::tar(temp_tar, files = list.files("."),
               compression = "gzip", tar = "internal")
  })

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  # file.copy() only warns on failure; a FALSE return used to be silently
  # ignored, reporting success while writing nothing (issue #8)
  if (!isTRUE(file.copy(temp_tar, output_file, overwrite = TRUE))) {
    cli::cli_abort("Failed to write {.path {output_file}}.")
  }

  if (validate) {
    validate_ice(output_file, quiet = TRUE)
  }

  cli::cli_inform(".ice file created at {.path {output_file}}")
  invisible(output_file)
}
