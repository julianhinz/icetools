# build_manifest() (#2)

test_that("build_manifest scans directories and reports good and bad archives", {
  lib <- tempfile("icelib")
  dir.create(lib)
  write_example_ice(file.path(lib, "good_a.ice"))
  write_example_ice(file.path(lib, "good_b.ice"))
  writeLines("garbage", file.path(lib, "broken.ice"))

  manifest <- suppressWarnings(build_manifest(lib))
  expect_identical(nrow(manifest), 3L)
  expect_identical(manifest$artifact, c("broken.ice", "good_a.ice", "good_b.ice"))
  expect_identical(manifest$status, c("error", "ok", "ok"))
  expect_match(manifest$sha256, "^[0-9a-f]{64}$")
  expect_identical(manifest[manifest$artifact == "good_a.ice", ]$n_tables, 3L)
  expect_identical(manifest[manifest$artifact == "good_a.ice", ]$n_scalars, 3L)
  expect_identical(manifest[manifest$artifact == "good_a.ice", ]$format_version, "1.1")
  expect_false(is.na(manifest[manifest$artifact == "broken.ice", ]$error))

  # archive-level sha256 matches an independent computation
  expect_identical(manifest[manifest$artifact == "good_a.ice", ]$sha256,
                   sha256_file(file.path(lib, "good_a.ice")))
})

test_that("build_manifest writes CSV and YAML outputs", {
  lib <- tempfile("icelib")
  dir.create(lib)
  write_example_ice(file.path(lib, "one.ice"))
  csv <- tempfile(fileext = ".csv")
  yml <- tempfile(fileext = ".yaml")
  build_manifest(lib, output_csv = csv, output_yaml = yml)

  back <- data.table::fread(csv)
  expect_identical(back$artifact, "one.ice")
  y <- yaml::read_yaml(yml)
  expect_identical(names(y), "one.ice")
  expect_identical(y$one.ice$status, "ok")
})

test_that("build_manifest handles pre-1.1 archives and explicit file paths", {
  manifest <- build_manifest(fixture_path("v012_fixture.ice"))
  expect_identical(manifest$status, "ok")
  expect_identical(manifest$format_version, "1.0")   # no field = 1.0
  expect_identical(manifest$n_tables, 5L)
  expect_identical(manifest$n_scalars, 0L)
})

test_that("build_manifest warns on nonexistent paths and returns empty cleanly", {
  expect_warning(m <- build_manifest(tempfile()), "skipped")
  expect_identical(nrow(m), 0L)
})
