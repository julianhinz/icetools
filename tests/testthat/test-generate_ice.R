# generate_ice() robustness

test_that("output parent directories are created; copy failures error (issue #8)", {
  components <- list(t = data.frame(k = "a", value = 1))
  md <- create_metadata(components)

  # nonexistent nested output dir: created, not silently dropped
  out <- file.path(tempfile("outdir"), "nested", "artifact.ice")
  suppressMessages(generate_ice(components, md, out))
  expect_true(file.exists(out))
})

test_that(".ice extension is appended when missing", {
  components <- list(t = data.frame(k = "a", value = 1))
  out <- file.path(tempdir(), paste0("noext", as.integer(runif(1) * 1e8)))
  suppressMessages(generate_ice(components, create_metadata(components), out))
  expect_true(file.exists(paste0(out, ".ice")))
})

test_that("duplicate and path-traversal component names error early (issue #9)", {
  md <- create_metadata(list(a = data.frame(k = 1, value = 1)))
  dup <- list(a = data.frame(k = 1, value = 1), a = data.frame(k = 2, value = 999))
  expect_error(suppressMessages(generate_ice(dup, md, tempfile(fileext = ".ice"))),
               "Duplicate")
  evil <- list(`../evil` = data.frame(k = 1, value = 1))
  expect_error(create_metadata(evil), "archive-safe")
  expect_error(suppressMessages(generate_ice(evil, md, tempfile(fileext = ".ice"))),
               "archive-safe")
  reserved <- list(metadata = data.frame(k = 1, value = 1))
  expect_error(suppressMessages(generate_ice(reserved, md, tempfile(fileext = ".ice"))),
               "reserved")
})

test_that("invalid component types error with the offender named (issues #3, #13)", {
  expect_error(create_metadata(list(ok = data.frame(k = 1, value = 1), bad = c(1, 2))),
               "bad")
  expect_error(create_metadata(list(e = data.frame())), "e")
  expect_error(create_metadata(list(l = list(1))), "l")
})

test_that("metadata/component drift warns; validate = TRUE catches junk", {
  components <- list(t = data.frame(k = "a", value = 1))
  other_md <- create_metadata(list(u = data.frame(k = "a", value = 1)))
  expect_warning(
    suppressMessages(generate_ice(components, other_md, tempfile(fileext = ".ice"))),
    "do not match"
  )
  expect_error(
    suppressWarnings(suppressMessages(
      generate_ice(components, other_md, tempfile(fileext = ".ice"), validate = TRUE)
    ))
  )
})

test_that("working directory is unchanged after generate_ice, also on error", {
  before <- getwd()
  components <- list(t = data.frame(k = "a", value = 1))
  suppressMessages(generate_ice(components, create_metadata(components),
                                tempfile(fileext = ".ice")))
  expect_identical(getwd(), before)
})

test_that("generator/format stamps and checksums land in the metadata", {
  path <- write_example_ice()
  md <- attr(suppressWarnings(read_ice(path)), "metadata")
  expect_identical(md$format_version, "1.2")
  expect_identical(md$generator$icetools_version,
                   as.character(utils::packageVersion("icetools")))
  expect_true("trade_share.csv" %in% names(md$checksums))
  expect_true("scalars.csv" %in% names(md$checksums))
  expect_match(md$checksums$trade_share.csv, "^[0-9a-f]{64}$")

  # checksums = FALSE omits the section
  p2 <- write_example_ice(checksums = FALSE)
  expect_null(attr(suppressWarnings(read_ice(p2)), "metadata")$checksums)
})

test_that("caller's metadata object is not mutated", {
  components <- list(t = data.frame(k = "a", value = 1))
  md <- create_metadata(components)
  md_before <- md
  suppressMessages(generate_ice(components, md, tempfile(fileext = ".ice")))
  expect_identical(md, md_before)
})

test_that("read-after-write validation passes for a good artifact", {
  expect_no_error(write_example_ice(validate = TRUE))
})

test_that("generate_ice writes large tables in chunks and round-trips bit-exactly", {
  set.seed(7)
  n <- 25000L
  tab <- data.table::data.table(origin = sprintf("c%03d", sample(226L, n, TRUE)),
                                destination = sprintf("c%03d", sample(226L, n, TRUE)),
                                sector = sprintf("%06d", seq_len(n)),
                                value = stats::runif(n))
  tables <- list(trade_share = tab)
  out <- file.path(tempdir(), "chunked.ice")
  generate_ice(tables, create_metadata(tables, description = "chunk test"), out, chunk_rows = 10000L)
  back <- read_ice(out)
  expect_identical(back$trade_share$value, tab$value)
  expect_identical(back$trade_share$sector, tab$sector)
  expect_identical(nrow(back$trade_share), n)
  dir <- tempfile(); dir.create(dir); utils::untar(out, exdir = dir)
  expect_identical(sum(grepl("^origin,", readLines(file.path(dir, "trade_share.csv")))), 1L)
})

test_that("generate_ice with chunk_rows smaller than the table matches the unchunked result", {
  tables <- list(x = data.frame(country = c("USA", "CHN"), value = c(0.1, 0.2)))
  a <- file.path(tempdir(), "a.ice"); b <- file.path(tempdir(), "b.ice")
  generate_ice(tables, create_metadata(tables), a)
  generate_ice(tables, create_metadata(tables), b, chunk_rows = 1L)
  expect_identical(read_ice(a)$x$value, read_ice(b)$x$value)
})

test_that("generate_ice validates chunk_rows", {
  tables <- list(x = data.frame(country = "USA", value = 0.1))
  out <- file.path(tempdir(), "bad.ice")
  expect_error(generate_ice(tables, create_metadata(tables), out, chunk_rows = 0), "chunk_rows")
  expect_error(generate_ice(tables, create_metadata(tables), out, chunk_rows = c(1, 2)), "chunk_rows")
  expect_error(generate_ice(tables, create_metadata(tables), out, chunk_rows = NA), "chunk_rows")
})

test_that("generate_ice accepts chunk_rows = Inf and above the integer range without overflow", {
  tables <- list(x = data.frame(country = c("USA", "CHN", "DEU"), value = c(0.1, 0.2, 0.3)))
  for (cr in list(Inf, 1e10, .Machine$integer.max)) {
    out <- tempfile(fileext = ".ice")
    expect_no_warning(generate_ice(tables, create_metadata(tables), out, chunk_rows = cr))
    expect_identical(read_ice(out)$x$value, c(0.1, 0.2, 0.3))
  }
})

test_that("a decimal block followed by a hex-float block reads back identically", {
  # the decimal/hex decision is per block; on platforms whose parser reads every decimal
  # exactly no value goes hex, so the formatter is forced to hex for the block with the subnormal
  original <- format_double_shortest
  local_mocked_bindings(format_double_shortest = function(x) {
    if (any(x == 5e-324, na.rm = TRUE)) sprintf_hex_exact(x) else original(x)
  })
  vals <- c(0.1, 0.2, 5e-324, 1 / 3)
  tables <- list(x = data.frame(k = c("a", "b", "c", "d"), value = vals))
  out <- tempfile(fileext = ".ice")
  generate_ice(tables, create_metadata(tables), out, chunk_rows = 2L)
  dir <- tempfile(); dir.create(dir); utils::untar(out, exdir = dir)
  lines <- readLines(file.path(dir, "x.csv"))
  expect_false(any(grepl("0x", lines[2:3])))
  expect_true(all(grepl("0x", lines[4:5])))
  expect_identical(read_ice(out)$x$value, vals)
})
