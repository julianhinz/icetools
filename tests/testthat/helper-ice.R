# shared test helpers

# canonical component set exercising the format surface
example_components <- function() {
  list(
    trade_share = data.frame(
      origin = c("USA", "USA", "CHN", "CHN"),
      destination = c("USA", "CHN", "USA", "CHN"),
      value = c(0.75, 0.25, 0.4, 0.6)
    ),
    trade_elasticity = data.frame(sector = c("A", "B"), theta = c(4.5, 6.0)),
    sector_share = data.frame(sector = c("01", "02"), value = c(0.4, 0.6)),
    consumption = 0.35,          # scalar (double)
    n_iterations = 25L,          # scalar (integer)
    calibration_tag = "gtap11"   # scalar (character)
  )
}

write_example_ice <- function(path = tempfile(fileext = ".ice"), components = example_components(), ...) {
  md <- create_metadata(components, description = "test artifact",
                        version = "1.0", timestamp = "2026-07-12 12:00:00")
  suppressMessages(generate_ice(components, md, path, ...))
  path
}

fixture_path <- function(name) testthat::test_path("fixtures", name)
