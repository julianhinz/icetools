# icetools 0.2.1

Two fixes surfaced by the KITE-dev 26.07 consolidation. `.ice` format
version 1.2, `.icc` format version 1.1 (both additive).

* `compile_ice()` no longer silently corrupts incomplete tables (#14):
  `cast_ice_table()` was ported from KITE-dev's pre-#175
  `cast_variable()` and inherited its recycling bug — base-R `array()`
  recycles the value column into any larger full grid, misaligning
  every cell after the first missing row (the BACI trade-only
  corruption KITE-dev root-caused as #175: ×189 income ratios from a
  valid-looking `.icc`). Casting now aborts with a classed error
  (`icetools_incomplete_grid`) naming the table and row/cell counts
  when rows don't form the exact key grid or key combinations are
  duplicated. (`validate_ice()` has caught this at authoring time since
  0.2.0; the compile path is now guarded too.)
* The `trade_elasticity` sign convention is recorded in the metadata
  (#15, format 1.2): KITE-dev #167 standardized on positive θ, but
  nothing in a `.ice` file said which convention its values use —
  a double-conversion already caused a 106% zero-shock divergence
  downstream. `generate_ice()` now auto-detects and stamps
  `conventions: {trade_elasticity: positive_theta | legacy_negative}`
  (explicit declaration via `create_metadata(conventions = )` wins;
  mixed signs warn and stamp nothing). `validate_ice()` errors when
  the data contradicts the declaration and warns on pre-1.2 files that
  carry `trade_elasticity` without the field — readers of such files
  must assume `legacy_negative`. The `.icc` carries the source's
  conventions in `meta$source$conventions` (icc format 1.1). Published
  KITE-data v0.2.0 artifacts all predate the field: they are
  legacy-negative and should be re-stamped with icetools ≥ 0.2.1.

# icetools 0.2.0

.ice format version 1.1 (additive; 0.1.2-written files keep reading
correctly and their behavior is pinned by a fixture generated with the
exact 0.1.2 sources). New .icc format, version 1.0. Formal specs now
live in `docs/ice-schema.md` and `docs/icc-schema.md`.

## New: .icc compiled initial conditions (#4)

Ported from KITE-dev's fast-solver sprint: `compile_ice()` casts a
`.ice`'s long tables into dense named arrays once and serializes the
result (qs2 when installed, uncompressed RDS otherwise; magic-byte
sniffing on read). `read_icc()`, `validate_icc()` (structure +
staleness: the `.icc` records the SHA-256, coordinates and format
version of its source `.ice`), `icc_tables()` (melt back). KITE-dev
should consume these instead of its own copy — see the migration note
in the release description.

## New: scalar components (#3)

Length-1 numeric/character/logical components (e.g. Baqaee–Farhi nest
elasticities) are first-class: `scalars.csv` member + `scalars:`
metadata section, symmetric support in `create_metadata()`,
`generate_ice()`, `read_ice()`, `validate_ice()`, `list_tables()`,
`extract_table()`. A component list round-trips shape-identically.
This unblocks Baqaee–Farhi artifacts in KITE-data releases.

## New: provenance, checksums, manifests (#2)

* `create_metadata()` gains optional artifact-level fields
  (`identifier`, `title`, `license`, `contact`, `citation`, `model`,
  `scenario`, `sources`, `assumptions`, `generator`, `coordinates`,
  `provenance`).
* `generate_ice()` writes per-member SHA-256 checksums into
  `metadata.yaml` (`checksums = TRUE` default), stamps
  `format_version` and `generator.icetools_version`, and offers
  read-after-write validation (`validate = TRUE`).
* New `build_manifest()`: scan directories of `.ice` archives into a
  manifest (data.table/CSV/YAML) with archive-level SHA-256, sizes,
  key metadata, and per-file error status. New `sha256_file()`.

## Fixed

* `validate_ice()`, `list_tables()`, `extract_table()` were dead code:
  they read `$tables`/`$metadata` off `read_ice()`'s return, which
  stores metadata as an attribute — validation silently passed
  anything, listing returned NULL, extraction always errored (#7).
  `validate_ice()` now actually checks member/metadata drift, exact
  column sets, declared-vs-read types, NAs (`check_na`), Cartesian key
  completeness without materializing the grid (`check_complete`),
  checksums, and scalar drift, reporting all problems at once.
* The numeric round-trip guarantee now actually holds to the last ulp
  (#10): doubles are written as shortest round-trip decimals, verified
  at write time against the reader's own parser, with a whole-column
  C99 hex-float fallback where platform decimal parsers are provably
  lossy (observed on ARM even for exact decimal expansions); subnormals
  use the `0x0.<frac>p-1022` spelling R parses exactly. `NaN` and `NA`
  are now distinct on round-trip. Previously fwrite's ~15 significant
  digits silently corrupted 16th/17th-digit values.
* Metadata is authoritative for column types on read (#6, #11):
  declared character keys survive integer-looking values (`"01"` keeps
  its zero), declared character value columns are no longer destroyed
  by unconditional `as.numeric()`, the always-double guarantee applies
  to any declared numeric/integer value column whatever its name, and
  integer64 never leaks from fread inference.
* `generate_ice()` no longer reports success when the output copy
  failed (missing directories are now created, copy failure errors,
  #8); component names are validated (duplicates, path separators,
  reserved names) instead of silently losing data or writing outside
  the staging directory (#9); `setwd()` replaced by exception-safe
  scoped directory handling (#6).
* `read_ice()` errors cleanly on nonexistent files, non-tar input,
  truncated archives, and archives without `metadata.yaml`; components
  return in metadata order, with warnings on archive/metadata drift
  (#12). An untar connection leak on truncated archives is plugged.
* `create_metadata()`: clear errors naming invalid components instead
  of `-ncol(NULL)` crashes on scalars (#3) and zero-column tables
  (#13); `%||%` is defined internally so the declared R dependency is
  honest (#13); factors are recorded as character.

## Changed

* `create_metadata(non_interactive =)` now defaults to `TRUE`;
  prompting requires an interactive session (#6). Pipelines passing
  `non_interactive = TRUE` explicitly (e.g. KITE-data) are unaffected.
* `generate_ice()` returns the output path invisibly.
* `list_tables()` returns a data.table with `name`, `kind`,
  `description`, `keys`, `value_name`, `value_type` (it returned NULL
  before, so nothing could depend on the old shape).
* New test suite (250+ assertions incl. a full-range double fuzz and a
  0.1.2 fixture), GitHub Actions R CMD check (ubuntu + ARM macOS),
  `R CMD check` clean.

# icetools 0.1.2

* Numeric round-trip fix: `fwrite(scipen = 999)` on write, `as.numeric`
  coercion of `value` columns on read. (Vendored in KITE-data; this
  state is tagged `v0.1.2` retroactively.)

# icetools 0.1.1 and earlier

* Initial implementations of `generate_ice()`, `read_ice()`,
  `validate_ice()`, `create_metadata()`, `list_tables()`,
  `extract_table()`, YAML descriptions, `convert_to_ice.R` script.
