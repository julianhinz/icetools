#' Generate a .ice file
#'
#' @param tables A named list of data frames, where names correspond to table names.
#' @param metadata A list containing metadata for the .ice file, including table details.
#' @param output_file The file path where the .ice file will be saved (should end with .ice).
#' @return A message indicating the file creation location.
#' @examples
#' tables <- list(
#'   trade_flows = data.frame(country = c("USA", "CHN"), value = c(100, 200)),
#'   tariffs = data.frame(country = c("USA", "CHN"), value = c(0.1, 0.2))
#' )
#' metadata <- create_metadata(tables)
#' generate_ice(tables, metadata, "example.ice")
#' @export
generate_ice <- function(tables, metadata, output_file) {
  # Validation
  if (!is.list(tables) || !is.list(metadata)) {
    stop("Both 'tables' and 'metadata' must be lists.")
  }

  if (is.null(names(tables)) || any(names(tables) == "")) {
    stop("All elements in 'tables' must be named.")
  }

  # Write tables as CSVs
  temp_dir <- tempfile()
  dir.create(temp_dir)
  for (table_name in names(tables)) {
    utils::write.csv(tables[[table_name]], file = file.path(temp_dir, paste0(table_name, ".csv")), row.names = FALSE)
  }

  # Write metadata
  yaml::write_yaml(metadata, file = file.path(temp_dir, "metadata.yaml"))

  # Ensure output_file has .ice extension
  output_file <- ifelse(grepl("\\.ice$", output_file), output_file, paste0(output_file, ".ice"))

  # Create a temporary tar file first
  temp_tar <- tempfile(fileext = ".ice")

  # Save current working directory
  old_wd <- getwd()

  # Change to temp directory and create archive
  setwd(temp_dir)
  tryCatch({
    utils::tar(temp_tar, files = list.files(".", full.names = FALSE), compression = "gzip")

    # Move the temp file to the desired location
    setwd(old_wd)
    file.copy(temp_tar, output_file, overwrite = TRUE)
    file.remove(temp_tar)
  }, finally = {
    # Always restore original working directory
    setwd(old_wd)
  })

  # Clean up temp directory
  unlink(temp_dir, recursive = TRUE)

  message(".ice file created at ", output_file)
}
