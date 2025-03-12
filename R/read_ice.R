read_ice <- function(ice_file) {
  # Unpack .ice file
  temp_dir <- tempfile()
  dir.create(temp_dir)
  utils::untar(ice_file, exdir = temp_dir)

  # Read metadata
  metadata <- yaml::read_yaml(file.path(temp_dir, "metadata.yaml"))

  # Read tables and attach metadata to them
  tables <- list()
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  for (csv_file in csv_files) {
    table_name <- tools::file_path_sans_ext(basename(csv_file))
    table <- utils::read.csv(csv_file)
    if (!is.null(metadata$tables[[table_name]])) {
      attr(table, "metadata") <- metadata$tables[[table_name]]
    }
    tables[[table_name]] <- table
  }

  # Set metadata and filename as attributes of the list
  attr(tables, "metadata") <- metadata
  attr(tables, "filename") <- ice_file

  # Clean up temporary directory
  unlink(temp_dir, recursive = TRUE)

  # Assign class
  class(tables) <- "ice"

  return(tables)
}
