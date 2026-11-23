# The `.ice` file format — specification

**Format version: 1.2** · maintained in [`icetools`](https://github.com/julianhinz/icetools) · reference site: [icefil.es](https://icefil.es)

`.ice` (**I**nitial **C**onditions **E**quilibrium) files are shareable
calibration artifacts for quantitative trade models: readable,
reproducible, portable. One `.ice` file carries a complete set of
initial conditions — long-format data tables plus scalar parameters —
together with self-describing metadata.

This document is the normative specification. The implementation of
record is the `icetools` R package; if the two disagree, that is a bug
in one of them — file an issue.

## Version history

| Format version | icetools | Changes |
|---|---|---|
| 1.0 | ≤ 0.1.2 | Implicit format: tar.gz of per-table CSVs + `metadata.yaml`. No `format_version` field. |
| 1.1 | ≥ 0.2.0 | Additive: `format_version` field, scalar components (`scalars.csv` + `scalars:` metadata section), per-member `checksums:`, `generator:` stamp, formalized provenance fields, verified numeric round-trip cell syntax (incl. hex-float columns). |
| 1.2 | ≥ 0.2.1 | Additive: `conventions:` block; writers auto-record the `trade_elasticity` sign convention (section 3.1). Files without the block are assumed `legacy_negative`. |

**Compatibility policy.** Format changes are additive. A file without a
`format_version` field is a 1.0 file. Readers MUST accept files of the
same major version with any minor version (unknown fields are ignored);
writers stamp the version they write. Anything that would break a 1.x
reader bumps the major version and gets a migration note here.

## 1. Container

A `.ice` file is a **gzip-compressed tar archive** (any tar flavor a
POSIX tar or R's internal tar can read; `icetools` writes R's internal
ustar). Members live at the archive root:

```
baseline.ice
├── metadata.yaml        # REQUIRED: self-description (section 3)
├── <table>.csv          # one per data table (section 2)
└── scalars.csv          # OPTIONAL: scalar components (section 4)
```

`metadata.yaml` and `scalars.csv` are **reserved member names**.
Everything else ending in `.csv` is a data table whose name is the
filename without extension. Component names MUST match
`^[A-Za-z0-9][A-Za-z0-9._-]*$` (archive-safe: no path separators, no
leading dots) and MUST be unique.

## 2. Data tables

Each table is a UTF-8 CSV with a header row, in **long format**:

- **The last column is the value column.** Every column before it is a
  key column. This positional rule is the format's core convention; the
  value column may have any name (`value` by convention, but e.g.
  `theta` is legal).
- Key columns SHOULD form a complete Cartesian grid: every combination
  of observed key values occurs exactly once (`validate_ice()` checks
  this; `check_complete = FALSE` relaxes it for legitimately sparse
  tables).
- Tables SHOULD NOT contain missing values (`check_na = FALSE`
  relaxes).
- A single-column table (value only, no keys) is legal.
- Zero-row tables are legal.

### 2.1 Numeric cell syntax and the round-trip guarantee

The format guarantees that **doubles survive a write→read round-trip
bit-for-bit**. Writers MUST emit, for every double cell, a string the
reader parses back to the identical IEEE-754 double. `icetools`
enforces this at write time by re-parsing what it is about to write and
widening until exact:

1. shortest round-trip decimal (15 → 22 significant digits, `%g`
   style; scientific notation allowed) — virtually every column in
   practice, and `0.1` stays `0.1`;
2. if any cell of a column cannot be verified as a decimal on the
   writing platform (R's and data.table's decimal parsers can be one
   ulp off near parser-hostile mantissas; observed on ARM), the whole
   column is written as **C99 hexadecimal floating-point literals**
   (e.g. `0x1.91eb851eb851fp-1`), which parse exactly everywhere.
   Subnormals use the `0x0.<frac>p-1022` spelling (R's parser reads
   normalized subnormal forms as zero).

Readers MUST accept both syntaxes. In R: `fread()` parses decimal
columns natively; a column containing hex floats arrives as character
and is converted with `as.numeric()`, which parses hex floats exactly.
Consumers in other languages should use `float.fromhex`-equivalents for
`0x`-prefixed cells.

A writer may switch between decimal and hex-float syntax between row blocks of one column; readers MUST accept both syntaxes within a column.

Non-finite cells are the literals `NaN`, `Inf`, `-Inf`; missing values
are empty cells. (`NA` and `NaN` are distinct and both round-trip.)

### 2.2 Types

Column types are declared in the metadata (section 3) and are
**authoritative on read**:

- declared `character` (or `factor`) columns are read as character —
  codes like `"01"` keep their leading zeros;
- the value column of a table declared `numeric` or `integer` is
  ALWAYS a double after reading (the *always-double guarantee*:
  downstream consumers never defend against integer inference);
- integer *key* columns stay integer; `logical` columns round-trip;
- `factor` columns are stored as character and come back as character;
- 64-bit integers are not a format type (values are doubles).

## 3. `metadata.yaml`

Top-level fields (▲ = added in 1.1, all optional unless marked):

| Field | | Meaning |
|---|---|---|
| `format_version` ▲ | string | `"1.1"`; absent = 1.0 |
| `description` | string | what this artifact is |
| `version` | string | the *artifact's* version (semantic versioning recommended) |
| `timestamp` | string | creation time |
| `identifier`, `title`, `license`, `contact`, `citation` ▲ | strings | artifact-level provenance |
| `model`, `scenario` ▲ | maps | model id/spec-ref; base year, target year, scenario id |
| `sources` ▲ | list | upstream source entries (role, description, path/URI, sha256, …) |
| `assumptions` ▲ | list | free-form notes |
| `generator` ▲ | map | producing pipeline (repo, commit, …); `generator.icetools_version` is stamped automatically |
| `conventions` ▲▲ | map | data conventions (1.2), section 3.1 |
| `coordinates` | map | position of this artifact in its library (e.g. KITE-data: source, year, contract, variant) |
| `provenance` | map | upstream provenance carried over from the input data |
| `checksums` ▲ | map | member filename → SHA-256 hex of the member's bytes |
| `tables` | map | **required**: per-table schema, below |
| `scalars` ▲ | map | per-scalar type + description (section 4) |

Free-form fields are written verbatim; readers ignore fields they don't
know (forward compatibility). ▲ = added in 1.1, ▲▲ = added in 1.2.

### 3.1 Conventions (1.2)

The `conventions` map declares how the data is to be interpreted where
the numbers alone are ambiguous. Recognized key:

```yaml
conventions:
  trade_elasticity: positive_theta   # or: legacy_negative
```

KITE standardized on **positive θ** (KITE-dev #167, 2026-07-12); files
published before format 1.2 carry legacy *negative* values with nothing
recording which — a double-conversion of θ has already produced a 106%
zero-shock divergence downstream. Rules:

- **Writers** record the convention. `generate_ice()` auto-detects it
  from the sign of the `trade_elasticity` component (table's value
  column or scalar) unless declared explicitly via
  `create_metadata(conventions = ...)`; mixed/zero signs warn and stamp
  nothing.
- **Readers** of files *without* the field MUST assume
  `legacy_negative` (everything published as format ≤ 1.1 is
  negative-θ).
- **Validation** errors when the data's sign contradicts the declared
  convention, and warns when a file carrying `trade_elasticity` lacks
  the field.

Producers may add further free-form convention keys (e.g. `units`);
readers ignore keys they don't know.

Per-table schema under `tables.<name>`:

```yaml
tables:
  trade_share:
    description: Bilateral trade shares
    keys:
    - name: origin
      type: character
      description: Key column for origin
    - name: destination
      type: character
      description: Key column for destination
    value:
      name: value          # any name; matches the CSV's last column
      type: numeric
      description: Value column
```

`keys` lists every key column in CSV order; `value` describes the last
column. Recognized `type` values: `character`, `numeric`, `integer`,
`logical` (plus `factor` and `integer64` in legacy 1.0 files, mapped to
`character` and `numeric` respectively on read).

The `checksums` map covers data members only (`<table>.csv`,
`scalars.csv`), not `metadata.yaml` itself. The archive-level SHA-256
lives *outside* the archive (a file cannot embed its own hash) — in
release manifests, see `build_manifest()`.

## 4. Scalar components (1.1)

Model-wide scalar parameters (e.g. Baqaee–Farhi nest elasticities) are
first-class components: a `scalars.csv` member with header `name,value`
and one row per scalar, plus a metadata section:

```yaml
scalars:
  consumption:
    description: Consumption nest elasticity
    type: numeric
```

Scalar values follow the same cell syntax as table cells (section 2.1).
Recognized types: `numeric`, `integer`, `character`, `logical`. Readers
return scalars as bare length-1 vectors, so a component list
round-trips: `list(trade_share = <table>, consumption = 0.35)` in,
same shape out.

## 5. Reader/writer obligations

Writers:
- stage members, verify numeric round-trip (2.1), write `metadata.yaml`
  last with checksums of the staged members;
- MUST fail loudly if the output cannot be written (no partial/absent
  files behind a success message).

Readers:
- MUST error clearly on: missing file, non-tar input, truncated
  archive, missing `metadata.yaml`;
- MUST honor declared column types (2.2) and return components in
  metadata order (tables, then scalars);
- SHOULD warn on archive↔metadata drift (undeclared members, declared
  but missing members) and leave strictness to `validate_ice()`.

Validation (`validate_ice()`) additionally checks: exact declared
column sets, declared-vs-read types, missing values, Cartesian
completeness incl. duplicate key detection, checksum match, scalar
drift. Everything is reported in one pass.

## 6. Reference implementation quick reference

```r
library(icetools)
md  <- create_metadata(components, description = "…", version = "1.0")
generate_ice(components, md, "artifact.ice", validate = TRUE)
ice <- read_ice("artifact.ice")
validate_ice("artifact.ice")
list_tables("artifact.ice")
extract_table("artifact.ice", "trade_share")
build_manifest("library/", output_csv = "manifest.csv")
```

For the compiled companion format, see [`icc-schema.md`](icc-schema.md).
