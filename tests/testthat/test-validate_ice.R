# validate_ice(): acceptance AND rejection (issue #7 made it a no-op)

repack <- function(src, mutate) {
  d <- tempfile(); dir.create(d)
  utils::untar(src, exdir = d)
  mutate(d)
  f <- tempfile(fileext = ".ice")
  icetools:::with_dir(d, utils::tar(f, files = list.files("."),
                                    compression = "gzip", tar = "internal"))
  f
}

test_that("a well-formed artifact validates", {
  path <- write_example_ice()
  expect_true(suppressMessages(validate_ice(path)))
})

test_that("the metadata-vs-archive mismatch that 0.1.2 waved through is rejected", {
  # metadata for one component set, archive contents for another (the exact
  # scenario validate_ice 0.1.2 declared 'successful', issue #7)
  junk <- list(junk = data.frame(k = "USA", value = NA_real_))
  md <- create_metadata(list(trade_flows = data.frame(country = "USA", value = 100)))
  path <- tempfile(fileext = ".ice")
  suppressWarnings(suppressMessages(generate_ice(junk, md, path)))
  expect_error(suppressMessages(validate_ice(path)), "failed validation")
})

test_that("missing values are rejected, unless check_na = FALSE", {
  components <- list(t = data.frame(k = c("a", "b"), value = c(1, NA)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_error(suppressMessages(validate_ice(path)), "missing values")
  expect_true(suppressMessages(validate_ice(path, check_na = FALSE)))
})

test_that("incomplete key grids and duplicate keys are rejected", {
  incomplete <- list(t = data.frame(
    origin = c("USA", "USA", "CHN"),
    destination = c("USA", "CHN", "USA"),
    value = c(1, 2, 3)
  ))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(incomplete, create_metadata(incomplete), path))
  expect_error(suppressMessages(validate_ice(path)), "combinations")
  expect_true(suppressMessages(validate_ice(path, check_complete = FALSE)))

  dupes <- list(t = data.frame(k = c("a", "a"), value = c(1, 2)))
  path2 <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(dupes, create_metadata(dupes), path2))
  expect_error(suppressMessages(validate_ice(path2)), "duplicated key")
})

test_that("tampered members fail the checksum verification", {
  path <- write_example_ice()
  tampered <- repack(path, function(d) {
    csv <- file.path(d, "trade_share.csv")
    txt <- readLines(csv)
    txt[2] <- sub("0.75", "0.76", txt[2], fixed = TRUE)
    writeLines(txt, csv)
  })
  expect_error(suppressMessages(validate_ice(tampered)), "Checksum mismatch")
})

test_that("member/metadata drift is fatal in validation", {
  path <- write_example_ice()
  missing_member <- repack(path, function(d) file.remove(file.path(d, "sector_share.csv")))
  expect_error(suppressMessages(validate_ice(missing_member)), "sector_share")

  extra_member <- repack(path, function(d) {
    utils::write.csv(data.frame(k = "x", value = 1), file.path(d, "stray.csv"),
                     row.names = FALSE)
  })
  expect_error(suppressMessages(validate_ice(extra_member)), "stray")
})

test_that("scalar drift is rejected", {
  path <- write_example_ice()
  no_scalars <- repack(path, function(d) file.remove(file.path(d, "scalars.csv")))
  expect_error(suppressMessages(validate_ice(no_scalars)), "calibration_tag|consumption")
})

test_that("metadata's declared types are authoritative on read", {
  # declare theta (numeric cells) as character: the reader must honor the
  # declaration and return character, and validation must stay coherent
  path <- write_example_ice()
  retyped <- repack(path, function(d) {
    meta_path <- file.path(d, "metadata.yaml")
    md <- yaml::read_yaml(meta_path)
    md$tables$trade_elasticity$value$type <- "character"
    md$checksums <- NULL  # metadata edited, drop now-stale checksums
    yaml::write_yaml(md, meta_path)
  })
  expect_type(suppressWarnings(read_ice(retyped))$trade_elasticity$theta, "character")
  expect_true(suppressMessages(validate_ice(retyped, check_complete = FALSE)))
})

test_that("genuinely non-numeric cells under a numeric declaration are caught via NA check", {
  components <- list(t = data.frame(k = c("a", "b"), value = c("x1", "x2")))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  corrupted <- repack(path, function(d) {
    meta_path <- file.path(d, "metadata.yaml")
    md <- yaml::read_yaml(meta_path)
    md$tables$t$value$type <- "numeric"   # lie about the type
    md$checksums <- NULL
    yaml::write_yaml(md, meta_path)
  })
  # coercion turns "x1"/"x2" into NA; the NA check flags it
  expect_error(suppressMessages(suppressWarnings(validate_ice(corrupted))),
               "missing values")
})
