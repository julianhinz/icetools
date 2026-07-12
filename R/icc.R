#' Compile a .ice file into a .icc file
#'
#' `.icc` ("compiled initial conditions") caches the expensive cast from
#' long-format initial-condition tables into dense named arrays, so model
#' setup does it once instead of on every run (measured 30-40x faster
#' setup in KITE, with bit-identical results). `compile_ice()` reads a
#' `.ice` archive, casts every table, and returns an object of class
#' `"icc"`; with `output_file` set it also serializes the object (with
#' qs2 when installed, otherwise uncompressed RDS - see [read_icc()]).
#'
#' The `.icc` records the SHA-256 and coordinates of its source `.ice`,
#' the icetools version, and the compile options, so a stale cache is
#' detectable: see [validate_icc()]. Format details in
#' `docs/icc-schema.md`.
#'
#' Casting (ported from KITE's `cast_variable()`): key columns become
#' array dimensions in `variable_order` (known dimensions first, others
#' in table order), rows are sorted by the reversed key columns, and the
#' value column fills the array. Tables without key columns stay plain
#' vectors. Scalar components pass through unchanged.
#'
#' @param ice_file Path to the source `.ice` file.
#' @param output_file Optional path for the serialized `.icc` (the
#'   extension is appended if missing). Parent directories are created.
#' @param variable_order Character vector giving the preferred dimension
#'   order of the cast arrays. The default is the canonical KITE order.
#' @param use_qs2 Serialize with qs2 if installed (default `TRUE`). Set
#'   `FALSE` to force the RDS fallback.
#' @return The `"icc"` object, invisibly when `output_file` is given.
#' @examples
#' \dontrun{
#' compile_ice("baseline.ice", "baseline.icc")
#' icc <- read_icc("baseline.icc")
#' icc$arrays$trade_share  # dense named array
#' }
#' @export
compile_ice <- function(ice_file,
                        output_file = NULL,
                        variable_order = c("country", "headquarter", "factor",
                                           "origin", "destination", "sector",
                                           "input", "output"),
                        use_qs2 = TRUE) {
  ice <- read_ice(ice_file)
  metadata <- attr(ice, "metadata")

  is_table <- vapply(ice, is.data.frame, logical(1))
  tables <- ice[is_table]
  scalars <- ice[!is_table]

  arrays <- lapply(tables, cast_ice_table, variable_order = variable_order)
  value_names <- vapply(names(tables), function(nm) {
    metadata$tables[[nm]]$value$name %||% names(tables[[nm]])[ncol(tables[[nm]])]
  }, character(1))

  serializer <- if (use_qs2 && requireNamespace("qs2", quietly = TRUE)) "qs2" else "rds"

  compiled <- structure(
    list(
      arrays = arrays,
      scalars = scalars,
      dimensions = ice_dimensions(tables),
      meta = list(
        icc_format_version = ICC_FORMAT_VERSION,
        icetools_version = as.character(utils::packageVersion("icetools")),
        created = as.character(Sys.time()),
        source = list(
          file = basename(ice_file),
          sha256 = sha256_file(ice_file),
          ice_format_version = metadata$format_version %||% "1.0",
          description = metadata$description,
          version = metadata$version,
          timestamp = metadata$timestamp,
          coordinates = metadata$coordinates
        ),
        compile_options = list(
          variable_order = variable_order,
          serializer = serializer
        ),
        variables = names(arrays),
        value_names = value_names
      )
    ),
    class = "icc"
  )

  if (is.null(output_file)) return(compiled)

  if (!grepl("\\.icc$", output_file)) output_file <- paste0(output_file, ".icc")
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  if (identical(serializer, "qs2")) {
    qs2::qs_save(compiled, output_file)
  } else {
    saveRDS(compiled, output_file, compress = FALSE)
  }
  cli::cli_inform(".icc file created at {.path {output_file}} ({serializer} serialization)")
  invisible(compiled)
}

#' Read a .icc file
#'
#' Loads a compiled-initial-conditions object written by [compile_ice()].
#' The serialization format is sniffed from the file's magic bytes (qs2
#' files start with `0x0b 0x0e`; RDS with `X\n` or a gzip header), so
#' files travel between machines regardless of whether qs2 is installed
#' at either end - reading a qs2-written file does require qs2.
#'
#' Reading warns if the file was compiled by a different icetools
#' version; use [validate_icc()] with the source `.ice` to check for
#' staleness proper.
#'
#' @param icc_file Path to the .icc file.
#' @return An object of class `"icc"`: `arrays` (dense named arrays),
#'   `scalars`, `dimensions`, and `meta` (provenance + compile options).
#' @export
read_icc <- function(icc_file) {
  if (!file.exists(icc_file)) {
    cli::cli_abort("File does not exist: {.path {icc_file}}")
  }
  # magic bytes verified against qs2::qs_save() output (qs2 >= 0.2):
  # 0x0b 0x0e; uncompressed RDS starts with "X\n" (0x58 0x0a), compressed
  # RDS with a gzip/bzip2/xz header - no collision risk
  magic <- readBin(icc_file, "raw", n = 2L)
  compiled <- if (identical(magic, as.raw(c(0x0b, 0x0e)))) {
    if (!requireNamespace("qs2", quietly = TRUE)) {
      cli::cli_abort("{.path {icc_file}} was written with qs2; install the {.pkg qs2} package to read it.")
    }
    qs2::qs_read(icc_file)
  } else {
    tryCatch(readRDS(icc_file),
             error = function(e) cli::cli_abort("{.path {icc_file}} is not a readable .icc file."))
  }
  if (!inherits(compiled, "icc")) {
    cli::cli_abort("{.path {icc_file}} does not contain an icc object.")
  }
  current <- as.character(utils::packageVersion("icetools"))
  if (!identical(compiled$meta$icetools_version, current)) {
    cli::cli_warn("{.path {icc_file}} was compiled with icetools {compiled$meta$icetools_version}, running {current}; recompile if in doubt.")
  }
  compiled
}

#' Validate a .icc file, optionally against its source .ice
#'
#' Structural checks on a compiled-initial-conditions file and - when the
#' source `.ice` is supplied - a staleness check: the SHA-256 recorded at
#' compile time must match the current hash of the `.ice` file. A stale
#' `.icc` is a cache of data that no longer exists and must be
#' recompiled.
#'
#' @param icc_file Path to the .icc file.
#' @param ice_file Optional path to the source .ice file for the
#'   staleness check.
#' @param quiet If `TRUE`, suppress the success message.
#' @return Invisibly `TRUE` if valid (and fresh, when `ice_file` is
#'   given); otherwise an error listing every problem.
#' @export
validate_icc <- function(icc_file, ice_file = NULL, quiet = FALSE) {
  problems <- character(0)
  compiled <- withCallingHandlers(
    read_icc(icc_file),
    warning = function(w) {
      problems <<- c(problems, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  meta <- compiled$meta
  for (field in c("icc_format_version", "icetools_version", "created", "source", "compile_options")) {
    if (is.null(meta[[field]])) {
      problems <- c(problems, paste0("meta$", field, " is missing."))
    }
  }
  if (!is.list(compiled$arrays) || is.null(names(compiled$arrays))) {
    problems <- c(problems, "arrays is not a named list.")
  } else {
    if (!setequal(names(compiled$arrays), meta$variables %||% character(0))) {
      problems <- c(problems, "arrays do not match meta$variables.")
    }
    for (nm in names(compiled$arrays)) {
      arr <- compiled$arrays[[nm]]
      if (!is.array(arr)) next  # keyless tables stay plain vectors
      dn <- dimnames(arr)
      if (is.null(dn) || is.null(names(dn))) {
        problems <- c(problems, paste0("array '", nm, "' has no named dimnames."))
        next
      }
      unknown <- setdiff(names(dn), names(compiled$dimensions))
      if (length(unknown)) {
        problems <- c(problems, paste0(
          "array '", nm, "' uses dimension(s) ",
          paste0("'", unknown, "'", collapse = ", "),
          " absent from the dimension inventory."))
      }
    }
  }

  if (!is.null(ice_file)) {
    if (!file.exists(ice_file)) {
      problems <- c(problems, paste0("Source .ice file not found: ", ice_file))
    } else {
      current_sha <- sha256_file(ice_file)
      recorded_sha <- meta$source$sha256
      if (!identical(current_sha, recorded_sha)) {
        problems <- c(problems, paste0(
          "STALE: source .ice checksum changed since compilation (recorded ",
          substr(recorded_sha %||% "<none>", 1, 12), "..., current ",
          substr(current_sha, 1, 12), "...). Recompile with compile_ice()."))
      }
    }
  }

  if (length(problems)) {
    problems <- gsub("\\}", "}}", gsub("\\{", "{{", problems))
    cli::cli_abort(c(
      "{.path {icc_file}} failed validation:",
      stats::setNames(problems, rep("x", length(problems)))
    ))
  }
  if (!quiet) {
    cli::cli_alert_success("Validation successful: {.path {icc_file}} is properly structured{if (!is.null(ice_file)) ' and fresh' else ''}.")
  }
  invisible(TRUE)
}

#' Melt a compiled icc object back into long-format tables
#'
#' Inverse of the compile cast, for debugging and testing: every dense
#' array becomes a long-format `data.table` again (original value-column
#' names are restored), scalars pass through. Row order follows the
#' array layout and may differ from the source `.ice`; values are
#' bit-identical.
#'
#' @param compiled An `"icc"` object from [compile_ice()] or [read_icc()].
#' @return Named list of `data.table`s and scalars.
#' @export
icc_tables <- function(compiled) {
  if (!inherits(compiled, "icc")) {
    cli::cli_abort("'compiled' must be an icc object (see compile_ice()).")
  }
  tables <- lapply(names(compiled$arrays), function(nm) {
    table <- melt_ice_array(compiled$arrays[[nm]])
    value_name <- compiled$meta$value_names[[nm]] %||% "value"
    if (data.table::is.data.table(table) && !identical(value_name, "value")) {
      data.table::setnames(table, "value", value_name)
    }
    table
  })
  names(tables) <- names(compiled$arrays)
  c(tables, compiled$scalars)
}

#' @export
print.icc <- function(x, ...) {
  m <- x$meta
  cli::cli_text("<icc> {length(x$arrays)} array{?s}, {length(x$scalars)} scalar{?s}, compiled {m$created} (icetools {m$icetools_version}, {m$compile_options$serializer})")
  cli::cli_text("source: {.file {m$source$file}} (sha256 {substr(m$source$sha256, 1, 12)}...)")
  dims <- x$dimensions
  if (length(dims)) {
    cli::cli_text("dimensions: {paste(names(dims), vapply(dims, length, integer(1)), sep = '=', collapse = ', ')}")
  }
  invisible(x)
}

# --- internal: faithful ports of KITE-dev's casting machinery ------------

# Cast one long-format table to a dense named array (KITE cast_variable(),
# data.table branch). The value column is the table's LAST column (the
# format's convention), not necessarily one named "value" - that
# generalization and the keyless-table vector passthrough are the only
# deviations from the KITE original, which hardcoded `value` and never
# saw keyless tables.
cast_ice_table <- function(table, variable_order) {
  table <- data.table::as.data.table(table)
  if (ncol(table) == 1L) return(table[[1L]])   # keyless: plain vector

  keep <- if (is.null(variable_order)) character(0) else intersect(variable_order, names(table))
  table <- table[, c(keep, setdiff(names(table), keep)), with = FALSE]
  key_cols <- names(table)[-ncol(table)]
  # reverse sort by all key columns for correct array layout
  data.table::setorderv(table, rev(key_cols))
  dims <- vapply(key_cols, function(n) data.table::uniqueN(table[[n]]), integer(1))
  dim_names <- lapply(key_cols, function(n) {
    v <- unique(table[[n]])
    if (is.factor(v)) as.character(v) else v
  })
  names(dim_names) <- key_cols
  array(data = table[[ncol(table)]], dim = unname(dims), dimnames = dim_names)
}

# Dimension inventory across all tables (KITE get_model_dimensions()):
# unique values of every key column, first table to use a dimension name
# wins (KITE's !duplicated(names) semantics, ported faithfully - KITE
# initial conditions use each dimension consistently across tables), then
# sorted ascending. Sorting is load-bearing: arrays initialized from
# these dimensions must align positionally with cast arrays (KITE #142).
ice_dimensions <- function(tables) {
  dims <- list()
  for (table in tables) {
    key_cols <- names(table)[-ncol(table)]
    for (col in key_cols) {
      if (!is.null(dims[[col]])) next
      v <- unique(table[[col]])
      if (is.factor(v)) v <- as.character(v)
      if (is.character(v) || is.numeric(v)) v <- sort(v)
      dims[[col]] <- v
    }
  }
  dims
}

# Melt a dense array back to a long data.table (KITE melt_variable()).
melt_ice_array <- function(x) {
  if (!is.array(x)) return(x)   # keyless tables stayed plain vectors
  if (length(dim(x)) == 2L && dim(x)[2L] == 1L) {
    x <- array(x, dim = dim(x)[1L], dimnames = dimnames(x)[1L])
  }
  data.table::setDT(as.data.frame.table(x, responseName = "value",
                                        stringsAsFactors = FALSE))[]
}
