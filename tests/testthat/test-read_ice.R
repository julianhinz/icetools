# read_ice() robustness (issue #12)

test_that("nonexistent file gives a clean error", {
  expect_error(read_ice(tempfile(fileext = ".ice")), "does not exist")
})

test_that("non-tar files give a clean error", {
  f <- tempfile(fileext = ".ice")
  writeLines("this is not a tar archive", f)
  expect_error(read_ice(f), "not a readable .ice archive")
})

test_that("truncated archives give a clean error", {
  path <- write_example_ice()
  raw <- readBin(path, "raw", file.info(path)$size)
  trunc <- tempfile(fileext = ".ice")
  writeBin(raw[seq_len(length(raw) %/% 3)], trunc)
  expect_error(read_ice(trunc), "archive")
})

test_that("archives without metadata.yaml give a clean error", {
  d <- tempfile(); dir.create(d)
  utils::write.csv(data.frame(k = 1, value = 1), file.path(d, "t.csv"), row.names = FALSE)
  f <- tempfile(fileext = ".ice")
  icetools:::with_dir(d, utils::tar(f, files = "t.csv", compression = "gzip", tar = "internal"))
  expect_error(read_ice(f), "metadata.yaml")
  expect_error(validate_ice(f), "metadata.yaml")
  expect_error(list_tables(f), "metadata.yaml")
})

test_that("undeclared members warn and are appended; declared-but-missing warn and skip", {
  path <- write_example_ice()

  # unpack, add an undeclared member, drop a declared one, repack
  d <- tempfile(); dir.create(d)
  utils::untar(path, exdir = d)
  utils::write.csv(data.frame(k = "x", value = 9), file.path(d, "undeclared.csv"),
                   row.names = FALSE)
  file.remove(file.path(d, "trade_elasticity.csv"))
  f <- tempfile(fileext = ".ice")
  icetools:::with_dir(d, utils::tar(f, files = list.files("."),
                                    compression = "gzip", tar = "internal"))

  expect_warning(expect_warning(read_ice(f), "undeclared"), "trade_elasticity")
  ice <- suppressWarnings(read_ice(f))
  expect_true("undeclared" %in% names(ice))
  expect_false("trade_elasticity" %in% names(ice))
})

test_that("tables come back in metadata order, not alphabetical order", {
  components <- list(
    zebra = data.frame(k = "a", value = 1),
    alpha = data.frame(k = "a", value = 1)
  )
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_identical(names(read_ice(path)), c("zebra", "alpha"))
})

test_that("large integral values never come back as integer64", {
  components <- list(t = data.frame(k = "a", value = 9007199254740992))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  v <- read_ice(path)$t$value
  expect_type(v, "double")
  expect_identical(v, 9007199254740992)
})
