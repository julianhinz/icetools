#' Read a .ice file
#'
#' @param ice_file The file path to the .ice file.
#' @return A list containing the tables (data frames) and metadata.
#' @examples
#' ice <- read_ice("example.ice")
#' @export
read_ice <- function(ice_file) {
  # Unpack .ice file
  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::untar(ice_file, exdir = temp_dir)

  # Read metadata
  metadata <- yaml::read_yaml(file.path(temp_dir, "metadata.yaml"))

  # Read tables
  tables <- list()
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  for (csv_file in csv_files) {
    table_name <- tools::file_path_sans_ext(basename(csv_file))
    table <- utils::read.csv(csv_file)
    attr(table, "metadata") <- metadata$tables[[table_name]]
    tables[[table_name]] <- table
  }

  ice <- list(tables = tables, metadata = metadata)
  class(ice) <- "ice"
  return(ice)
}
