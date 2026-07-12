#' Validate a .ice file
#'
#' Checks a `.ice` archive against its own metadata and the format
#' contract (see `docs/ice-schema.md`). All problems are collected and
#' reported together in a single error. Checks performed:
#'
#' * the archive is readable and carries a `metadata.yaml`;
#' * every table/scalar declared in the metadata exists in the archive,
#'   and every data member in the archive is declared in the metadata;
#' * every table has exactly its declared key columns plus its declared
#'   value column, with matching column types;
#' * no missing values (`check_na = TRUE`);
#' * keys form a complete Cartesian grid: every combination of observed
#'   key values occurs exactly once (`check_complete = TRUE`);
#' * recorded member checksums match (format >= 1.1 archives written
#'   with `checksums = TRUE`).
#'
#' Note that versions up to 0.1.2 ran none of these checks: a bug made
#' `validate_ice()` silently validate anything (issue #7).
#'
#' @param ice_file Path to the .ice file.
#' @param check_na If `TRUE` (default), missing values in any table are
#'   an error.
#' @param check_complete If `TRUE` (default), each table must contain
#'   every combination of its observed key values exactly once.
#' @param quiet If `TRUE`, suppress the success message.
#' @return Invisibly `TRUE` if the file is valid; otherwise an error
#'   listing every problem found.
#' @examples
#' \dontrun{
#' validate_ice("example.ice")
#' }
#' @export
validate_ice <- function(ice_file, check_na = TRUE, check_complete = TRUE,
                         quiet = FALSE) {
  # read_ice() itself errors on unreadable/metadata-less archives and
  # warns on member/metadata drift; collect those warnings as problems
  problems <- character(0)
  ice_data <- withCallingHandlers(
    read_ice(ice_file),
    warning = function(w) {
      problems <<- c(problems, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  metadata <- attr(ice_data, "metadata")

  is_scalar <- !vapply(ice_data, is.data.frame, logical(1))
  tables <- ice_data[!is_scalar]
  scalars <- ice_data[is_scalar]

  for (table_name in names(metadata$tables)) {
    table <- tables[[table_name]]
    if (is.null(table)) next  # already reported as a read warning
    table_meta <- metadata$tables[[table_name]]

    key_names <- vapply(table_meta$keys, function(k) k$name, character(1))
    value_name <- table_meta$value$name %||% "value"

    missing_cols <- setdiff(c(key_names, value_name), names(table))
    extra_cols <- setdiff(names(table), c(key_names, value_name))
    if (length(missing_cols)) {
      problems <- c(problems, paste0(
        "Table '", table_name, "': declared column(s) ",
        paste0("'", missing_cols, "'", collapse = ", "), " missing."))
    }
    if (length(extra_cols)) {
      problems <- c(problems, paste0(
        "Table '", table_name, "': column(s) ",
        paste0("'", extra_cols, "'", collapse = ", "),
        " not declared in metadata (last column must be the value column)."))
    }
    if (length(missing_cols)) next

    # declared vs actual types (post-coercion; numeric/integer value
    # columns are doubles by the format's always-double guarantee)
    specs <- c(table_meta$keys, list(table_meta$value))
    for (spec in specs) {
      declared <- metadata_type_to_colclass(spec$type %||% "")
      if (is.na(declared)) next
      if (identical(spec$name, value_name) && declared %in% c("numeric", "integer")) {
        declared <- "numeric"
      }
      actual <- class(table[[spec$name]])[1]
      compatible <- switch(declared,
        numeric = actual %in% c("numeric", "integer"),
        integer = actual %in% c("integer", "numeric"),
        identical(actual, declared)
      )
      if (!compatible) {
        problems <- c(problems, paste0(
          "Table '", table_name, "', column '", spec$name, "': declared type '",
          spec$type, "' but read as '", actual, "'."))
      }
    }

    if (check_na && anyNA(table)) {
      problems <- c(problems, paste0("Table '", table_name, "' contains missing values."))
    }

    # completeness: every combination of observed key values exactly once
    # (count-based; no expand.grid materialization)
    if (check_complete && length(key_names)) {
      key_dt <- table[, key_names, with = FALSE]
      n_expected <- prod(vapply(key_dt, function(col) data.table::uniqueN(col), numeric(1)))
      if (anyDuplicated(key_dt)) {
        problems <- c(problems, paste0(
          "Table '", table_name, "' has duplicated key combinations."))
      } else if (nrow(key_dt) != n_expected) {
        problems <- c(problems, paste0(
          "Table '", table_name, "' does not contain all combinations of keys (",
          nrow(key_dt), " rows, expected ", format(n_expected, big.mark = ","), ")."))
      }
    }
  }

  # scalars declared in metadata must have arrived (reader warnings catch
  # the archive-side half)
  missing_scalars <- setdiff(names(metadata$scalars), names(scalars))
  if (length(missing_scalars)) {
    problems <- c(problems, paste0(
      "Scalar(s) ", paste0("'", missing_scalars, "'", collapse = ", "),
      " declared in metadata but missing from the archive."))
  }
  undeclared_scalars <- setdiff(names(scalars), names(metadata$scalars))
  if (length(undeclared_scalars)) {
    problems <- c(problems, paste0(
      "Scalar(s) ", paste0("'", undeclared_scalars, "'", collapse = ", "),
      " present in the archive but not declared in metadata."))
  }

  # member checksums (format >= 1.1)
  if (!is.null(metadata$checksums)) {
    temp_dir <- tempfile("validate_ice")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
    utils::untar(ice_file, exdir = temp_dir, tar = "internal")
    for (member in names(metadata$checksums)) {
      member_path <- file.path(temp_dir, member)
      if (!file.exists(member_path)) next  # drift already reported
      if (!identical(sha256_file(member_path), metadata$checksums[[member]])) {
        problems <- c(problems, paste0("Checksum mismatch for member '", member, "'."))
      }
    }
  }

  if (length(problems)) {
    # problem strings carry user data; escape cli's {} interpolation
    problems <- gsub("\\}", "}}", gsub("\\{", "{{", problems))
    cli::cli_abort(c(
      "{.path {ice_file}} failed validation:",
      stats::setNames(problems, rep("x", length(problems)))
    ))
  }

  if (!quiet) {
    cli::cli_alert_success("Validation successful: {.path {ice_file}} is properly structured.")
  }
  invisible(TRUE)
}
