create_sample_ice <- function() {
  temp_dir <- tempfile("ice_dir_")
  dir.create(temp_dir)

  metadata <- list(
    tables = list(
      sample_table = list(
        description = "Sample table",
        keys = list(
          list(name = "geo"),
          list(name = "year")
        )
      )
    )
  )

  metadata_path <- file.path(temp_dir, "metadata.yaml")
  yaml::write_yaml(metadata, metadata_path)

  sample_data <- data.table::data.table(
    geo = rep(c("A", "B"), each = 2),
    year = rep(c(2020, 2021), times = 2),
    value = c(1, 2, 3, 4)
  )

  data.table::fwrite(sample_data, file.path(temp_dir, "sample_table.csv"))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(temp_dir)
  ice_filename <- file.path(temp_dir, "sample.ice")
  utils::tar(basename(ice_filename), files = c("metadata.yaml", "sample_table.csv"), compression = "none")
  list(path = ice_filename, dir = temp_dir)
}

cleanup_sample_ice <- function(sample) {
  unlink(sample$dir, recursive = TRUE, force = TRUE)
}

test_that("extract_table returns requested table", {
  sample <- create_sample_ice()
  on.exit(cleanup_sample_ice(sample))

  table <- extract_table(sample$path, "sample_table")
  expect_s3_class(table, "data.table")
  expect_equal(nrow(table), 4)
  expect_equal(table$value, c(1, 2, 3, 4))
})

test_that("list_tables enumerates tables from metadata", {
  sample <- create_sample_ice()
  on.exit(cleanup_sample_ice(sample))

  table_list <- list_tables(sample$path)
  expect_equal(table_list$name, "sample_table")
  expect_equal(table_list$description, "Sample table")
})

test_that("validate_ice checks structure", {
  sample <- create_sample_ice()
  on.exit(cleanup_sample_ice(sample))

  expect_message(validate_ice(sample$path), "Validation successful")
})
