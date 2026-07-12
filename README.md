<img src="man/figures/logo.svg" align="right" alt="icetools logo" width="140"/>

# `icetools` — tools for `.ice` files

**Shareable calibration files for quantitative trade models.
Readable, reproducible, portable.**

`icetools` is the reference implementation of the
[**`.ice`**](docs/ice-schema.md) format (**I**nitial **C**onditions
**E**quilibrium: long-format tables + scalars + self-describing YAML
metadata in one archive) and its compiled companion
[**`.icc`**](docs/icc-schema.md) (the long-table → dense-array cast
done once and cached, for ~30–40× faster model setup). The format's
home is **[icefil.es](https://icefil.es)** — browse the library of
published calibration artifacts there.

## Installation

```r
# install.packages("devtools")
devtools::install_github("julianhinz/icetools")
```

## Quickstart: `.ice`

```r
library(icetools)

# components: data tables (last column = value column) and scalars
components <- list(
  trade_share = data.frame(
    origin      = c("USA", "USA", "CHN", "CHN"),
    destination = c("USA", "CHN", "USA", "CHN"),
    value       = c(0.75, 0.25, 0.40, 0.60)
  ),
  trade_elasticity = data.frame(sector = c("A", "B"), theta = c(4.5, 6.0)),
  consumption = 0.35   # scalar parameter, first-class since 0.2.0
)

metadata <- create_metadata(
  components,
  description = "Toy two-country calibration",
  version     = "1.0"
)

generate_ice(components, metadata, "toy.ice", validate = TRUE)

ice <- read_ice("toy.ice")     # tables as data.tables, scalars as vectors
ice$trade_share
ice$consumption                # 0.35

validate_ice("toy.ice")        # structure, types, completeness, checksums
list_tables("toy.ice")         # one row per component
extract_table("toy.ice", "trade_share")
build_manifest(".", output_csv = "manifest.csv")   # library overview
```

What the format guarantees (spec: [`docs/ice-schema.md`](docs/ice-schema.md)):

- **Numeric round-trip to the last ulp** — every double you write is
  the double you read back, verified at write time.
- **Metadata-driven types** — declared column types are authoritative:
  sector codes like `"01"` keep their zeros; a table's value column is
  always a double.
- **Self-describing provenance** — descriptions, versions, sources,
  coordinates, per-member SHA-256 checksums inside the file.

## Quickstart: `.icc`

```r
compile_ice("toy.ice", "toy.icc")        # cast once, cache (qs2 or RDS)

icc <- read_icc("toy.icc")
icc$arrays$trade_share                   # dense named array
#      destination
# origin  CHN  USA
#    CHN 0.60 0.40
#    USA 0.25 0.75

validate_icc("toy.icc", "toy.ice")       # errors with STALE if the .ice changed
```

An `.icc` is a cache: it records the SHA-256 and coordinates of its
source `.ice`, so stale caches are always detectable
(spec: [`docs/icc-schema.md`](docs/icc-schema.md)).

## The pipeline these formats live in

The KITE ecosystem is the worked example of the intended workflow:

```
KITE-data  ──builds & validates──▶  *.ice artifacts (GitHub Releases)
                                        │
                     ┌──────────────────┼──────────────────┐
                     ▼                                     ▼
                ricardo-aas                            KITE / KITE-dev
             (fetches artifacts)              read_ice() → compile_ice()
                                                   │
                                              *.icc cache → fast repeated solves
```

[KITE-data](https://github.com/julianhinz/KITE-data) stages initial
conditions with `create_metadata()` + `generate_ice()` and publishes
them as release artifacts with a checksum manifest; consumers fetch an
artifact, `read_ice()` it, and compile an `.icc` next to it so every
subsequent model run skips the cast. Any other producer/consumer pair
can use the same pattern — the formats carry everything needed.

## Documentation

- [`.ice` format specification](docs/ice-schema.md) (versioned, currently 1.1)
- [`.icc` format specification](docs/icc-schema.md) (versioned, currently 1.0)
- [icefil.es](https://icefil.es) — format overview, artifact library, contribution guide
- Function reference: `?generate_ice`, `?read_ice`, `?validate_ice`,
  `?create_metadata`, `?list_tables`, `?extract_table`,
  `?build_manifest`, `?sha256_file`, `?compile_ice`, `?read_icc`,
  `?validate_icc`, `?icc_tables`

## Converting existing data

A named list in an `.rds` converts in one call:

```bash
Rscript $(Rscript -e 'cat(system.file("scripts", "convert_to_ice.R", package = "icetools"))') \
  input.rds output.ice descriptions.yaml
```

The optional `descriptions.yaml` maps component names to descriptions
(otherwise generic placeholders are used):

```yaml
trade_share: "Bilateral trade shares by origin and destination."
trade_elasticity: "Trade elasticities by sector."
```

## How to cite

```bibtex
@Misc{Hinz2026icetools,
  author = {Julian Hinz},
  title  = {icetools: Tools for Managing Initial Conditions for Quantitative Trade Models},
  year   = {2026},
  url    = {https://icefil.es},
  note   = {R package version 0.2.0}
}
```

## License

MIT — see [LICENSE.md](LICENSE.md).

## Contributing

Issues and pull requests welcome at
[github.com/julianhinz/icetools](https://github.com/julianhinz/icetools).
Schema changes must bump the format version and update the schema
documents — see [CLAUDE.md](CLAUDE.md) for repo conventions.
