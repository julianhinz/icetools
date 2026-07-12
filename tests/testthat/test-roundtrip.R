# write -> read round-trips

test_that("full component set round-trips with exact values and types", {
  components <- example_components()
  path <- write_example_ice()
  ice <- suppressWarnings(read_ice(path))

  # component order = metadata order (tables first, then scalars)
  expect_identical(names(ice),
                   c("trade_share", "trade_elasticity", "sector_share",
                     "consumption", "n_iterations", "calibration_tag"))

  # tables: exact values, exact column names/order
  expect_identical(names(ice$trade_share), c("origin", "destination", "value"))
  expect_identical(ice$trade_share$value, components$trade_share$value)
  expect_identical(ice$trade_share$origin, components$trade_share$origin)

  # non-"value" value column is still a double (issue #6)
  expect_type(ice$trade_elasticity$theta, "double")
  expect_identical(ice$trade_elasticity$theta, components$trade_elasticity$theta)

  # character keys with leading zeros survive (issue #11)
  expect_identical(ice$sector_share$sector, c("01", "02"))

  # scalars round-trip with type (issue #3)
  expect_identical(ice$consumption, 0.35)
  expect_identical(ice$n_iterations, 25L)
  expect_identical(ice$calibration_tag, "gtap11")

  expect_s3_class(ice, "ice")
  expect_identical(attr(ice, "filename"), path)
  expect_identical(attr(ice, "metadata")$description, "test artifact")
})

test_that("numeric round-trip is ulp-exact incl. NaN/Inf/denormals (issue #10)", {
  hostile <- c(
    0.12345678901234567, 9007199254740992, 0.1, 1 / 3, pi,
    .Machine$double.xmax, .Machine$double.xmin, 5e-324, 2e-300, 1e300,
    -1.5e-8, 0, -1, NaN, Inf, -Inf,
    # parser-hostile trailing-zero mantissas (hex-column trigger on ARM)
    { set.seed(1); runif(50) }
  )
  components <- list(x = data.frame(k = sprintf("k%03d", seq_along(hostile)),
                                    value = hostile))
  path <- tempfile(fileext = ".ice")
  md <- create_metadata(components)
  suppressMessages(generate_ice(components, md, path))
  back <- read_ice(path)$x$value
  expect_identical(back, hostile)
})

test_that("NA values round-trip as NA; NaN stays NaN", {
  components <- list(x = data.frame(k = c("a", "b", "c"), value = c(1.5, NA, NaN)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  back <- read_ice(path)$x$value
  expect_identical(back, c(1.5, NA, NaN))
})

test_that("character value columns survive unmangled (issue #11)", {
  components <- list(labels = data.frame(code = c("A", "B"),
                                         value = c("apple", "banana")))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_identical(read_ice(path)$labels$value, c("apple", "banana"))
})

test_that("factor columns are stored and read as character", {
  components <- list(f = data.frame(k = factor(c("b", "a")), value = c(1, 2)))
  md <- create_metadata(components)
  expect_identical(md$tables$f$keys[[1]]$type, "character")
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, md, path))
  expect_identical(read_ice(path)$f$k, c("b", "a"))
})

test_that("integer keys stay integer; integer value columns become double", {
  components <- list(t = data.frame(year = c(2020L, 2021L), value = c(1L, 2L)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  back <- read_ice(path)$t
  expect_type(back$year, "integer")
  expect_identical(back$value, c(1, 2))  # always-double guarantee
})

test_that("logical columns round-trip", {
  components <- list(t = data.frame(k = c("a", "b"), flag = c(TRUE, FALSE)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_identical(read_ice(path)$t$flag, c(TRUE, FALSE))
})

test_that("zero-row tables round-trip", {
  components <- list(z = data.frame(k = character(0), value = numeric(0)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  back <- read_ice(path)$z
  expect_identical(nrow(back), 0L)
  expect_identical(names(back), c("k", "value"))
})

test_that("single-column (value-only) tables round-trip", {
  components <- list(world_gdp = data.frame(value = 84.5))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_identical(read_ice(path)$world_gdp$value, 84.5)
})

test_that("non-ASCII key values survive", {
  components <- list(t = data.frame(city = c("Kiel", "Zürich", "北京"),
                                    value = c(1, 2, 3)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_identical(read_ice(path)$t$city, c("Kiel", "Zürich", "北京"))
})

test_that("scalar NA and negative/extreme scalars round-trip", {
  components <- list(
    a = -0.12345678901234567,
    b = NA_real_,
    c = FALSE,
    t = data.frame(k = "x", value = 1)
  )
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  ice <- read_ice(path)
  expect_identical(ice$a, -0.12345678901234567)
  expect_identical(ice$b, NA_real_)
  expect_identical(ice$c, FALSE)
})
