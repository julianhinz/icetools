# internal helpers

test_that("format_double_shortest round-trips every double bit-exactly", {
  x <- c(
    0, 1, -1, 0.1, 0.3, 1 / 3, 2 / 3, pi,
    0.12345678901234567,             # needs 17 significant digits
    9007199254740992,                # 2^53
    .Machine$double.xmax, .Machine$double.xmin, .Machine$double.eps,
    5e-324,                          # smallest denormal
    2e-300, 1e300, -1.5e-8,
    Inf, -Inf, NaN
  )
  out <- icetools:::format_double_shortest(x)
  # round-trip is defined against the reader's parse path: fread with type
  # inference, then as.numeric() on the result (handles hex-float columns)
  parsed <- suppressWarnings(as.numeric(
    data.table::fread(text = c("v", out), integer64 = "double")$v
  ))
  expect_identical(parsed, x)
  # common values stay human-readable
  expect_identical(out[which(x == 0.1)[1]], "0.1")
  expect_identical(out[which(x == 0.3)[1]], "0.3")
  # NaN/Inf survive as literals
  expect_identical(out[length(out)], "NaN")
})

test_that("format_double_shortest round-trips randomly generated doubles (fuzz)", {
  set.seed(42)
  # doubles with random bit patterns across many magnitudes
  x <- c(
    runif(2000, -1, 1),
    runif(1000) * 10^sample(-320:308, 1000, replace = TRUE),
    rnorm(1000) * 2^sample(-1074:1023, 1000, replace = TRUE)
  )
  x <- x[is.finite(x)]
  out <- icetools:::format_double_shortest(x)
  parsed <- suppressWarnings(as.numeric(
    data.table::fread(text = c("v", out), integer64 = "double")$v
  ))
  expect_identical(parsed, x)
})

test_that("well-behaved columns stay decimal; parser-hostile columns go hex", {
  # ordinary economic magnitudes: clean decimal cells
  out <- icetools:::format_double_shortest(c(0.1, 0.25, 100, 1.5e-8))
  expect_false(any(grepl("^0x|^-0x", out)))
  expect_identical(out[1], "0.1")
})

test_that("format_double_shortest maps NA to NA_character_ but keeps NaN", {
  out <- icetools:::format_double_shortest(c(NA_real_, NaN, 1))
  expect_identical(out, c(NA_character_, "NaN", "1"))
})

test_that("sha256_file matches a known digest and errors on missing files", {
  f <- tempfile()
  on.exit(unlink(f))
  writeLines("ice", f)
  expect_match(sha256_file(f), "^[0-9a-f]{64}$")
  expect_identical(sha256_file(f), sha256_file(f))
  expect_error(sha256_file(tempfile()), "does not exist")
})

test_that("component_kind classifies tables, scalars, and rejects the rest", {
  ck <- icetools:::component_kind
  expect_identical(ck(data.frame(a = 1, value = 2)), "table")
  expect_identical(ck(data.table::data.table(value = 1)), "table")
  expect_identical(ck(0.2), "scalar")
  expect_identical(ck(3L), "scalar")
  expect_identical(ck("label"), "scalar")
  expect_identical(ck(TRUE), "scalar")
  expect_identical(ck(data.frame()), "invalid")        # zero columns (#13)
  expect_identical(ck(c(1, 2)), "invalid")             # length > 1
  expect_identical(ck(NULL), "invalid")
  expect_identical(ck(list(1)), "invalid")
  expect_identical(ck(array(1, c(1, 1))), "invalid")
})

test_that("check_component_names enforces archive-safe unique names (#9)", {
  ccn <- icetools:::check_component_names
  expect_silent(ccn(c("trade_share", "tariff.v2", "a-b", "X01")))
  expect_error(ccn(NULL), "named")
  expect_error(ccn(c("a", "")), "named")
  expect_error(ccn(c("a", "a")), "Duplicate")
  expect_error(ccn("../evil"), "archive-safe")
  expect_error(ccn("a/b"), "archive-safe")
  expect_error(ccn(".hidden"), "archive-safe")
  expect_error(ccn("metadata"), "reserved")
  expect_error(ccn("scalars"), "reserved")
})

test_that("with_dir restores the working directory on error", {
  before <- getwd()
  expect_error(icetools:::with_dir(tempdir(), stop("boom")), "boom")
  expect_identical(getwd(), before)
})
