# trade_elasticity sign convention (format 1.2, #15)

test_that("generate_ice auto-stamps positive_theta from the data", {
  path <- write_example_ice()   # theta = 4.5, 6.0
  md <- attr(read_ice(path), "metadata")
  expect_identical(md$conventions$trade_elasticity, "positive_theta")
  expect_identical(md$format_version, "1.2")
  expect_true(suppressMessages(validate_ice(path)))  # no warning expected
})

test_that("legacy negative values are stamped legacy_negative", {
  components <- list(trade_elasticity = data.frame(sector = c("A", "B"),
                                                   theta = c(-4.5, -6.0)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  md <- attr(read_ice(path), "metadata")
  expect_identical(md$conventions$trade_elasticity, "legacy_negative")
  expect_true(suppressMessages(validate_ice(path, check_complete = FALSE)))
})

test_that("an explicit conventions declaration is not overridden", {
  components <- list(trade_elasticity = data.frame(sector = "A", theta = 5))
  md <- create_metadata(components,
                        conventions = list(trade_elasticity = "positive_theta",
                                           units = "USD_mn"))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, md, path))
  back <- attr(read_ice(path), "metadata")$conventions
  expect_identical(back$trade_elasticity, "positive_theta")
  expect_identical(back$units, "USD_mn")
})

test_that("mixed signs warn at write time and stamp nothing", {
  components <- list(trade_elasticity = data.frame(sector = c("A", "B"),
                                                   theta = c(4, -6)))
  path <- tempfile(fileext = ".ice")
  expect_warning(
    suppressMessages(generate_ice(components, create_metadata(components), path)),
    "mixed or zero signs"
  )
  expect_null(attr(suppressWarnings(read_ice(path)), "metadata")$conventions)
})

test_that("scalar trade_elasticity is stamped too", {
  components <- list(trade_elasticity = 5.5,
                     t = data.frame(k = "a", value = 1))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  md <- attr(read_ice(path), "metadata")
  expect_identical(md$conventions$trade_elasticity, "positive_theta")
})

test_that("validate_ice errors when the sign contradicts the declaration", {
  components <- list(trade_elasticity = data.frame(sector = c("A", "B"),
                                                   theta = c(-4.5, -6.0)))
  md <- create_metadata(components,
                        conventions = list(trade_elasticity = "positive_theta"))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, md, path))
  expect_error(suppressMessages(validate_ice(path, check_complete = FALSE)),
               "contradicts the declared convention")
})

test_that("validate_ice warns on pre-1.2 files carrying trade_elasticity", {
  # the 0.1.2 fixture has trade_elasticity and no conventions block
  expect_warning(
    expect_true(suppressMessages(validate_ice(fixture_path("v012_fixture.ice")))),
    "No trade_elasticity sign convention"
  )
})

test_that("files without trade_elasticity get no convention machinery", {
  components <- list(t = data.frame(k = c("a", "b"), value = c(1, 2)))
  path <- tempfile(fileext = ".ice")
  suppressMessages(generate_ice(components, create_metadata(components), path))
  expect_null(attr(read_ice(path), "metadata")$conventions)
  expect_no_warning(suppressMessages(validate_ice(path)))
})

test_that("the .icc carries the source conventions", {
  path <- write_example_ice()
  compiled <- compile_ice(path)
  expect_identical(compiled$meta$source$conventions$trade_elasticity, "positive_theta")
  expect_identical(compiled$meta$icc_format_version, "1.1")
})
