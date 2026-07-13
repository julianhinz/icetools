# .icc: compile -> read -> validate -> staleness (#4)

test_that("compile_ice casts tables to dense named arrays (KITE semantics)", {
  path <- write_example_ice()
  compiled <- compile_ice(path)

  expect_s3_class(compiled, "icc")
  expect_identical(sort(names(compiled$arrays)),
                   sort(c("trade_share", "trade_elasticity", "sector_share")))

  arr <- compiled$arrays$trade_share
  expect_true(is.array(arr))
  expect_identical(names(dimnames(arr)), c("origin", "destination"))
  # cell-exact cast
  expect_identical(arr["USA", "USA"], 0.75)
  expect_identical(arr["USA", "CHN"], 0.25)
  expect_identical(arr["CHN", "USA"], 0.4)
  expect_identical(arr["CHN", "CHN"], 0.6)

  # scalars pass through
  expect_identical(compiled$scalars$consumption, 0.35)
  expect_identical(compiled$scalars$calibration_tag, "gtap11")

  # dimension inventory: sorted ascending (KITE #142 alignment); the first
  # table using a dimension name defines its values (faithful port of
  # KITE's get_model_dimensions !duplicated(names) semantics) - here
  # trade_elasticity (sectors A, B) comes before sector_share (01, 02)
  expect_identical(compiled$dimensions$origin, c("CHN", "USA"))
  expect_identical(compiled$dimensions$sector, c("A", "B"))
})

test_that("variable_order controls dimension order", {
  path <- write_example_ice()
  compiled <- compile_ice(path, variable_order = c("destination", "origin"))
  expect_identical(names(dimnames(compiled$arrays$trade_share)),
                   c("destination", "origin"))
  expect_identical(compiled$arrays$trade_share["CHN", "USA"], 0.25)  # dest CHN, origin USA
  expect_identical(compiled$meta$compile_options$variable_order,
                   c("destination", "origin"))
})

test_that("icc_tables melts back to bit-identical values with original value names", {
  path <- write_example_ice()
  components <- example_components()
  back <- icc_tables(compile_ice(path))

  a <- back$trade_share[order(origin, destination)]
  b <- data.table::as.data.table(components$trade_share)[order(origin, destination)]
  expect_identical(a$value, b$value)
  expect_identical(a$origin, b$origin)

  # non-"value" value column name restored
  expect_identical(names(back$trade_elasticity), c("sector", "theta"))
  expect_identical(back$trade_elasticity[order(sector)]$theta, c(4.5, 6.0))

  expect_identical(back$consumption, 0.35)
})

test_that("write -> read round-trips the icc object (RDS fallback path)", {
  path <- write_example_ice()
  out <- tempfile(fileext = ".icc")
  suppressMessages(compile_ice(path, out, use_qs2 = FALSE))
  expect_true(file.exists(out))
  # RDS magic: "X\n"
  expect_identical(readBin(out, "raw", 2), as.raw(c(0x58, 0x0a)))

  compiled <- read_icc(out)
  expect_s3_class(compiled, "icc")
  expect_identical(compiled$meta$compile_options$serializer, "rds")
  expect_identical(compiled$meta$source$sha256, sha256_file(path))
  expect_identical(compiled$meta$source$file, basename(path))
  expect_identical(compiled$meta$icetools_version,
                   as.character(utils::packageVersion("icetools")))
  expect_identical(compiled$arrays$trade_share["USA", "CHN"], 0.25)
})

test_that("qs2 serialization round-trips and is magic-byte detected", {
  skip_if_not_installed("qs2")
  path <- write_example_ice()
  out <- tempfile(fileext = ".icc")
  suppressMessages(compile_ice(path, out, use_qs2 = TRUE))
  expect_identical(readBin(out, "raw", 2), as.raw(c(0x0b, 0x0e)))
  compiled <- read_icc(out)
  expect_identical(compiled$meta$compile_options$serializer, "qs2")
  expect_identical(compiled$arrays$trade_share["CHN", "CHN"], 0.6)
})

test_that(".icc extension is appended when missing", {
  path <- write_example_ice()
  out <- file.path(tempdir(), paste0("noext_icc", as.integer(runif(1) * 1e8)))
  suppressMessages(compile_ice(path, out, use_qs2 = FALSE))
  expect_true(file.exists(paste0(out, ".icc")))
})

test_that("validate_icc accepts a fresh icc and flags a stale one", {
  ice_path <- tempfile(fileext = ".ice")
  write_example_ice(ice_path)
  icc_path <- tempfile(fileext = ".icc")
  suppressMessages(compile_ice(ice_path, icc_path, use_qs2 = FALSE))

  expect_true(suppressMessages(validate_icc(icc_path)))
  expect_true(suppressMessages(validate_icc(icc_path, ice_path)))

  # regenerate the source .ice with different content -> stale
  components <- example_components()
  components$trade_share$value <- rev(components$trade_share$value)
  md <- create_metadata(components)
  suppressMessages(generate_ice(components, md, ice_path))
  expect_error(suppressMessages(validate_icc(icc_path, ice_path)), "STALE")
})

test_that("validate_icc flags a missing source file and structural damage", {
  ice_path <- write_example_ice()
  icc_path <- tempfile(fileext = ".icc")
  suppressMessages(compile_ice(ice_path, icc_path, use_qs2 = FALSE))
  expect_error(suppressMessages(validate_icc(icc_path, tempfile())), "not found")

  # structural damage: drop meta fields
  broken <- readRDS(icc_path)
  broken$meta$source <- NULL
  broken$meta$compile_options <- NULL
  bpath <- tempfile(fileext = ".icc")
  saveRDS(broken, bpath, compress = FALSE)
  expect_error(suppressMessages(suppressWarnings(validate_icc(bpath))), "meta")
})

test_that("read_icc rejects non-icc files and warns on version drift", {
  f <- tempfile(fileext = ".icc")
  saveRDS(list(not = "an icc"), f, compress = FALSE)
  expect_error(read_icc(f), "does not contain an icc object")

  expect_error(read_icc(tempfile(fileext = ".icc")), "does not exist")

  ice_path <- write_example_ice()
  icc_path <- tempfile(fileext = ".icc")
  suppressMessages(compile_ice(ice_path, icc_path, use_qs2 = FALSE))
  aged <- readRDS(icc_path)
  aged$meta$icetools_version <- "0.0.1"
  saveRDS(aged, icc_path, compress = FALSE)
  expect_warning(read_icc(icc_path), "0.0.1")
})

test_that("incomplete key grids abort the cast instead of silently recycling (#14)", {
  # the BACI trade-only shape: no origin == destination rows; array()
  # would recycle values across the full 2x2 grid without this guard
  incomplete <- list(trade_share = data.frame(
    origin = c("USA", "CHN"),
    destination = c("CHN", "USA"),
    value = c(0.25, 0.4)
  ))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(incomplete, create_metadata(incomplete), path))
  err <- expect_error(compile_ice(path), class = "icetools_incomplete_grid")
  expect_match(conditionMessage(err), "trade_share")
  expect_match(conditionMessage(err$parent), "2 rows")
  expect_match(conditionMessage(err$parent), "4 cells")
})

test_that("duplicated key combinations abort the cast (#14)", {
  dupes <- list(t = data.frame(k = c("a", "a"), value = c(1, 2)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(dupes, create_metadata(dupes), path))
  err <- expect_error(compile_ice(path), class = "icetools_incomplete_grid")
  expect_match(conditionMessage(err$parent), "duplicated")
})

test_that("complete grids still cast; zero-row tables pass the guard", {
  components <- list(z = data.frame(k = character(0), value = numeric(0)),
                     t = data.frame(k = c("a", "b"), value = c(1, 2)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  compiled <- compile_ice(path)
  expect_identical(compiled$arrays$t[["b"]], 2)
  expect_identical(length(compiled$arrays$z), 0L)
})

test_that("keyless tables stay plain vectors through compile and melt", {
  components <- list(world_gdp = data.frame(value = 84.5),
                     t = data.frame(k = c("a", "b"), value = c(1, 2)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  compiled <- compile_ice(path)
  expect_identical(compiled$arrays$world_gdp, 84.5)
  expect_identical(icc_tables(compiled)$world_gdp, 84.5)
})

test_that("print.icc summarizes the object", {
  compiled <- compile_ice(write_example_ice())
  out <- paste(cli::cli_fmt(print(compiled)), collapse = "\n")
  expect_match(out, "3 arrays")
  expect_match(out, "3 scalars")
  expect_match(out, "sha256")
})

test_that("compiled arrays are bit-identical to a manual dcast", {
  # independent construction of the dense array for cross-checking
  path <- write_example_ice()
  compiled <- compile_ice(path)
  ts <- example_components()$trade_share
  manual <- array(NA_real_, dim = c(2, 2),
                  dimnames = list(origin = c("CHN", "USA"),
                                  destination = c("CHN", "USA")))
  for (i in seq_len(nrow(ts))) {
    manual[ts$origin[i], ts$destination[i]] <- ts$value[i]
  }
  arr <- compiled$arrays$trade_share
  expect_identical(arr[rownames(manual), colnames(manual)], manual)
})
