#' Extract a single component from a .ice file
#'
#' Extracts one table (or scalar) without reading the rest of the
#' archive: only `metadata.yaml` and the requested member are unpacked
#' and parsed, with the same metadata-driven column types as
#' [read_ice()].
#'
#' Versions up to 0.1.2 always errored here due to a metadata access bug
#' (issue #7).
#'
#' @param ice_file Path to the .ice file.
#' @param table_name Name of the table or scalar to extract.
#' @return The requested table as a `data.table`, or the requested
#'   scalar as a length-1 vector.
#' @examples
#' \dontrun{
#' extract_table("example.ice", "trade_flows")
#' }
#' @export
extract_table <- function(ice_file, table_name) {
  metadata <- read_ice_metadata(ice_file)

  temp_dir <- tempfile("extract_table")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  if (table_name %in% names(metadata$scalars)) {
    utils::untar(ice_file, files = "scalars.csv", exdir = temp_dir, tar = "internal")
    scalar_dt <- data.table::fread(file.path(temp_dir, "scalars.csv"),
                                   colClasses = "character")
    row <- which(scalar_dt$name == table_name)
    if (!length(row)) {
      cli::cli_abort("Scalar {.val {table_name}} is declared in metadata but missing from {.file scalars.csv}.")
    }
    return(coerce_scalar(scalar_dt$value[row[1]], metadata$scalars[[table_name]]$type))
  }

  member <- paste0(table_name, ".csv")
  ok <- untar_safely(ice_file, files = member, exdir = temp_dir)
  member_path <- file.path(temp_dir, member)
  if (!ok || !file.exists(member_path)) {
    available <- c(names(metadata$tables), names(metadata$scalars))
    cli::cli_abort(c(
      "Component {.val {table_name}} does not exist in {.path {ice_file}}.",
      "i" = "Available components: {.val {available}}."
    ))
  }

  table <- read_ice_table(member_path, metadata$tables[[table_name]])
  if (!is.null(metadata$tables[[table_name]])) {
    data.table::setattr(table, "metadata", metadata$tables[[table_name]])
  }
  table
}
