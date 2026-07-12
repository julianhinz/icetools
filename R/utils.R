# Internal helpers and format constants

# let data.table find its [ methods when icetools is Imported, not attached
.datatable.aware <- TRUE

# .ice format version written by this package. Files written by icetools
# <= 0.1.2 carry no format_version field and are treated as "1.0".
# Schema changes bump this and get documented in docs/ice-schema.md.
ICE_FORMAT_VERSION <- "1.1"

# .icc format version written by this package (see docs/icc-schema.md).
ICC_FORMAT_VERSION <- "1.0"

# archive member names that table/scalar components may not use
RESERVED_COMPONENT_NAMES <- c("metadata", "scalars")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Exception-safe working-directory change (the withr::with_dir pattern,
# without the dependency): the old directory is restored even if expr errors.
with_dir <- function(dir, expr) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  force(expr)
}

# Shortest decimal strings that round-trip to the exact same double.
# sprintf("%.15g") keeps CSVs clean for the common case ("0.1" stays "0.1")
# but cannot distinguish all doubles; stragglers are re-printed with more
# significant digits until the reader recovers the input bit-for-bit. This
# is the write side of the numeric round-trip guarantee (issue #10).
#
# Verification uses data.table::fread() — the format's actual reader — NOT
# base as.numeric(): both R's literal parser and as.numeric() are off by an
# ulp near the representable extremes (e.g. the exact double.xmax string
# parses to Inf), so verifying with anything other than the reader would
# certify strings the reader disagrees about. The ladder extends past the
# theoretical 17-digit bound because R's parsers (fread included) can
# misround near-midpoint strings, and on platforms without extended-
# precision long doubles (ARM/Apple Silicon) fread's decimal parser can be
# a full ulp off even for a double's EXACT decimal expansion (observed for
# trailing-zero-rich mantissas, e.g. runif output). If ANY cell of a column
# fails the decimal ladder, the whole column falls back to C99 hex-float
# literals (sprintf "%a"), which fread leaves as character and the reader
# converts with as.numeric() — R parses hex floats exactly by construction,
# on every platform. Non-finite cells stay "NaN"/"Inf"/"-Inf" literals in
# both modes. The round-trip guarantee is enforced at write time:
# reading back format_double_shortest(x) with the reader's exact parse
# path is bit-identical to x for every double, or the writer aborts.
# See docs/ice-schema.md for the cell syntax.
format_double_shortest <- function(x) {
  if (!length(x)) return(character(0))
  out <- sprintf("%.15g", x)
  for (fmt in c("%.16g", "%.17g", "%.18g", "%.20g", "%.22g")) {
    bad <- which(is.finite(x) & fread_doubles(out) != x)
    if (!length(bad)) break
    out[bad] <- sprintf(fmt, x[bad])
  }
  if (length(which(is.finite(x) & fread_doubles(out) != x))) {
    # hex-float mode for the whole column; exact under as.numeric()
    out <- sprintf_hex_exact(x)
  }
  if (length(which(is.finite(x) & fread_doubles(out) != x))) {
    cli::cli_abort("Internal error: numeric column could not be serialized round-trip-exactly (first offender: {format(x[which(is.finite(x) & fread_doubles(out) != x)[1]], digits = 22)}).")
  }
  out[is.na(x) & !is.nan(x)] <- NA_character_
  out
}

# C99 hex-float literals that R's as.numeric() parses back exactly.
# sprintf("%a") is exact but prints subnormals in normalized form with
# exponents below -1022 (e.g. "0x1.e1c575cp-1047"), which R's hex parser
# silently reads as 0. Subnormals are re-spelled in the C99 subnormal form
# "0x0.<13 hex digits>p-1022", which parses exactly. Non-finite cells stay
# "NaN"/"Inf"/"-Inf" literals.
sprintf_hex_exact <- function(x) {
  out <- sprintf("%a", x)
  nf <- !is.finite(x)
  out[nf] <- as.character(x[nf])
  sub <- which(is.finite(x) & x != 0 & abs(x) < 2^-1022)
  if (length(sub)) {
    m <- abs(x[sub]) / 2^-1074           # integral mantissa, < 2^52
    hi <- m %/% 16^7                     # top 24 bits
    lo <- m %% 16^7                      # bottom 28 bits
    out[sub] <- paste0(ifelse(x[sub] < 0, "-", ""),
                       "0x0.", sprintf("%06x%07x", hi, lo), "p-1022")
  }
  out
}

# Parse a column of decimal/hex-float strings exactly as read_ice() will:
# fread with type inference (integer64 never survives; the format guarantees
# doubles), then as.numeric() on whatever comes back. Columns containing
# hex floats arrive as character and as.numeric() converts them exactly.
fread_doubles <- function(s) {
  s[is.na(s)] <- "NA"
  v <- data.table::fread(text = c("v", s), header = TRUE,
                         integer64 = "double", na.strings = "NA")$v
  suppressWarnings(as.numeric(v))
}

#' Compute the SHA-256 checksum of a file
#'
#' Portable helper used for per-table checksums inside `metadata.yaml`,
#' archive-level checksums in manifests ([build_manifest()]), and `.icc`
#' staleness detection ([validate_icc()]).
#'
#' @param path Path to an existing file.
#' @return Lower-case hex SHA-256 string.
#' @examples
#' f <- tempfile()
#' writeLines("x", f)
#' sha256_file(f)
#' @export
sha256_file <- function(path) {
  if (!file.exists(path)) cli::cli_abort("File does not exist: {.path {path}}")
  digest::digest(file = path, algo = "sha256")
}

# utils::untar(tar = "internal") leaks an open file connection when it
# errors mid-extraction (e.g. truncated gzip). Track connections and close
# whatever a failed extraction leaves behind. Returns TRUE on success.
untar_safely <- function(tarfile, ...) {
  before <- getAllConnections()
  ok <- tryCatch(
    { utils::untar(tarfile, ..., tar = "internal"); TRUE },
    error = function(e) FALSE, warning = function(w) FALSE
  )
  if (!ok) {
    for (con in setdiff(getAllConnections(), before)) {
      try(close(getConnection(con)), silent = TRUE)
    }
  }
  ok
}

# Classify a component of the initial-conditions list.
# "table": any data.frame with at least one column.
# "scalar": length-1 atomic of a CSV-representable type.
# Everything else is invalid and gets a clear error naming the offender.
component_kind <- function(x) {
  if (is.data.frame(x)) {
    if (ncol(x) < 1L) return("invalid")
    return("table")
  }
  if (is.atomic(x) && !is.array(x) && length(x) == 1L &&
      (is.numeric(x) || is.character(x) || is.logical(x))) {
    return("scalar")
  }
  "invalid"
}

# Validate component names: non-empty, unique, filesystem/archive-safe,
# not colliding with reserved archive members (issue #9).
check_component_names <- function(nms) {
  if (is.null(nms) || any(is.na(nms)) || any(nms == "")) {
    cli::cli_abort("All components must be named (non-empty, non-NA names).")
  }
  if (anyDuplicated(nms)) {
    cli::cli_abort("Duplicate component name{?s}: {.val {unique(nms[duplicated(nms)])}}.")
  }
  ok <- grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", nms)
  if (!all(ok)) {
    cli::cli_abort(c(
      "Component name{?s} {.val {nms[!ok]}} {?is/are} not archive-safe.",
      "i" = "Names must match ^[A-Za-z0-9][A-Za-z0-9._-]*$ (no path separators or leading dots)."
    ))
  }
  reserved <- nms %in% RESERVED_COMPONENT_NAMES
  if (any(reserved)) {
    cli::cli_abort("Component name{?s} {.val {nms[reserved]}} {?is/are} reserved for archive members.")
  }
  invisible(nms)
}

# Split a component list into data tables and scalars, erroring on anything
# else (fixes the cryptic crashes of issues #3 and #13).
split_components <- function(components) {
  check_component_names(names(components))
  kinds <- vapply(components, component_kind, character(1))
  if (any(kinds == "invalid")) {
    cli::cli_abort(c(
      "Component{?s} {.val {names(components)[kinds == 'invalid']}} {?is/are} neither a data.frame with columns nor a length-1 atomic scalar.",
      "i" = "Allowed components: data.frames/data.tables (last column = value), or single numeric/character/logical scalars."
    ))
  }
  list(tables = components[kinds == "table"], scalars = components[kinds == "scalar"])
}

# Map a type recorded in metadata.yaml to an fread colClasses entry.
# Factors are stored as character in the CSV; unknown types return NA so
# fread falls back to type inference.
metadata_type_to_colclass <- function(type) {
  switch(type,
    character = "character",
    factor = "character",
    numeric = "numeric",
    double = "numeric",
    integer = "integer",
    logical = "logical",
    NA_character_
  )
}

# Coerce a parsed scalar (read from scalars.csv as character) back to its
# declared type.
coerce_scalar <- function(value, type) {
  suppressWarnings(switch(type %||% "numeric",
    character = as.character(value),
    logical = as.logical(value),
    integer = as.integer(value),
    as.numeric(value)
  ))
}
