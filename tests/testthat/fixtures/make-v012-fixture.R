###
# Generate the v0.1.2 backward-compatibility fixture
# 260712
###
# This script was run ONCE against the icetools 0.1.2 sources vendored in
# KITE-data (code/dependencies/icetools_0.1.2.tar.gz) — the exact code that
# wrote every published KITE-data release artifact. The resulting
# v012_fixture.ice is committed and must never be regenerated with newer
# icetools: it exists to prove that files written by 0.1.2 keep reading
# correctly (hard backward-compatibility requirement).
#
# Usage (from the repo root, with the 0.1.2 sources unpacked somewhere):
#   Rscript tests/testthat/fixtures/make-v012-fixture.R /path/to/icetools-0.1.2

args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- if (length(args) >= 1) args[1] else stop("pass path to 0.1.2 sources")
for (f in list.files(file.path(pkg_dir, "R"), full.names = TRUE)) source(f)
`%||%` <- function(x, y) if (!is.null(x)) x else y

tables <- list(
  # canonical long table: two character keys + numeric value
  trade_share = data.frame(
    origin = c("USA", "USA", "CHN", "CHN"),
    destination = c("USA", "CHN", "USA", "CHN"),
    value = c(0.75, 0.25, 0.4, 0.6)
  ),
  # integral-looking numeric values (fread would guess integer)
  tariff = data.frame(
    origin = c("USA", "CHN"),
    destination = c("CHN", "USA"),
    value = c(1, 2)
  ),
  # character keys with leading zeros (fread would guess integer)
  sector_share = data.frame(
    sector = c("01", "02", "10"),
    value = c(0.2, 0.3, 0.5)
  ),
  # value column not named "value"
  trade_elasticity = data.frame(
    sector = c("A", "B"),
    theta = c(4, 6)
  ),
  # single-column table (value only, no keys)
  world_gdp = data.frame(value = 84.5)
)

descriptions <- list(
  trade_share = "Bilateral trade shares",
  tariff = "Bilateral tariffs",
  sector_share = "Sectoral expenditure shares",
  trade_elasticity = "Trade elasticities by sector",
  world_gdp = "World GDP, trillion USD"
)
desc_file <- tempfile(fileext = ".yaml")
yaml::write_yaml(descriptions, desc_file)

metadata <- create_metadata(
  tables,
  description = "icetools 0.1.2 backward-compatibility fixture",
  version = "1.0",
  timestamp = "2026-07-12 12:00:00",
  descriptions_file = desc_file,
  non_interactive = TRUE
)
# KITE-data's release script attaches these ad hoc (postprocess/04-release.R)
metadata$coordinates <- list(source = "fixture", year = 2026L, contract = "test")

out <- file.path("tests", "testthat", "fixtures", "v012_fixture.ice")
generate_ice(tables, metadata, out)
cat("fixture written:", out, "\n")
