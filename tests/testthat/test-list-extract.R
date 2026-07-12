# list_tables() and extract_table() (both broken in 0.1.2, issue #7)

test_that("list_tables returns one row per component with kinds and types", {
  path <- write_example_ice()
  listing <- list_tables(path)
  expect_s3_class(listing, "data.frame")
  expect_identical(listing$name,
                   c("trade_share", "trade_elasticity", "sector_share",
                     "consumption", "n_iterations", "calibration_tag"))
  expect_identical(listing$kind, c(rep("table", 3), rep("scalar", 3)))
  expect_identical(listing[listing$name == "trade_share", ]$keys, "origin, destination")
  expect_identical(listing[listing$name == "trade_elasticity", ]$value_name, "theta")
  expect_identical(listing[listing$name == "consumption", ]$value_type, "numeric")
})

test_that("extract_table returns single tables and scalars", {
  path <- write_example_ice()
  components <- example_components()

  tab <- extract_table(path, "trade_share")
  expect_s3_class(tab, "data.table")
  expect_identical(tab$value, components$trade_share$value)

  expect_identical(extract_table(path, "sector_share")$sector, c("01", "02"))
  expect_identical(extract_table(path, "consumption"), 0.35)
  expect_identical(extract_table(path, "n_iterations"), 25L)
})

test_that("extract_table errors clearly for unknown components", {
  path <- write_example_ice()
  expect_error(extract_table(path, "nope"), "does not exist")
  expect_error(extract_table(path, "nope"), "Available components")
})

test_that("print.ice shows tables and scalars", {
  path <- write_example_ice()
  ice <- read_ice(path)
  out <- paste(cli::cli_fmt(print(ice)), collapse = "\n")
  expect_match(out, "trade_share")
  expect_match(out, "Scalars")
  expect_match(out, "consumption")
})
