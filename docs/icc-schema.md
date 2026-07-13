# The `.icc` file format — specification

**Format version: 1.1** · maintained in [`icetools`](https://github.com/julianhinz/icetools) · reference site: [icefil.es](https://icefil.es)

| Format version | icetools | Changes |
|---|---|---|
| 1.0 | 0.2.0 | Initial: arrays/scalars/dimensions/meta, qs2-or-RDS container, staleness fields. |
| 1.1 | ≥ 0.2.1 | Additive: `meta$source$conventions` carried over from the source `.ice` (e.g. the `trade_elasticity` sign convention, `.ice` spec §3.1); compilation aborts on incomplete key grids (classed error `icetools_incomplete_grid`). |

`.icc` (**I**nitial **C**onditions, **C**ompiled) is the compiled
companion of [`.ice`](ice-schema.md): the expensive cast from
long-format initial-condition tables into the dense named arrays a
solver actually consumes, done **once** at compile time and serialized.
In KITE this cut model setup time ~30–40× (measured 39×/31× on GTAP and
a 200×500 synthetic world, bit-identical results; KITE-dev fast-solver
sprint, 2026-07-09).

An `.icc` is a **cache, not a source of truth**: it records the
identity of the `.ice` it was compiled from, and consumers MUST be able
to detect staleness (section 3).

## 1. Container

An `.icc` file is a single serialized R object in one of two formats,
distinguished by magic bytes:

| First bytes | Format |
|---|---|
| `0x0b 0x0e` | [qs2](https://cran.r-project.org/package=qs2) (fast, multithreaded zstd) |
| `0x58 0x0a` ("X\n") | uncompressed RDS |
| gzip/bzip2/xz header | compressed RDS (accepted on read) |

Writers use qs2 when available and fall back to uncompressed RDS
(near disk speed for numeric arrays); readers sniff the magic bytes, so
files travel between machines regardless of which serializer either
end has. Reading a qs2-written file requires qs2.

## 2. Object structure

A list of class `"icc"`:

```
icc
├── arrays        # named list: one dense array per source table
│                 #   dimnames = key values, dimension names = key column
│                 #   names; keyless (value-only) tables stay plain vectors
├── scalars       # named list of length-1 vectors (scalar components)
├── dimensions    # named list: dimension name -> sorted unique values
│                 #   (first table using a dimension name defines it)
└── meta
    ├── icc_format_version   # "1.0"
    ├── icetools_version     # compiler version
    ├── created              # compile timestamp
    ├── source               # identity of the source .ice:
    │   ├── file             #   basename
    │   ├── sha256           #   SHA-256 of the .ice file bytes
    │   ├── ice_format_version
    │   ├── description / version / timestamp
    │   ├── coordinates      #   copied from the .ice metadata
    │   └── conventions      #   copied from the .ice metadata (1.1)
    ├── compile_options
    │   ├── variable_order   # dimension ordering used by the cast
    │   └── serializer       # "qs2" | "rds"
    ├── variables            # names(arrays)
    └── value_names          # original value-column name per table
```

### 2.1 Cast semantics (ported from KITE)

For each table (last column = value, per the `.ice` convention):

1. reorder columns: dimensions named in `variable_order` first (in that
   order), remaining key columns in table order, value last. The
   default order is KITE's canonical
   `country, headquarter, factor, origin, destination, sector, input, output`;
2. **guard**: the rows must form the exact Cartesian grid of the
   observed key values — one row per combination, explicit zeros
   included, no duplicates. Otherwise compilation aborts with a classed
   error (`icetools_incomplete_grid`) naming the table and row/cell
   counts. Without this, base-R `array()` silently *recycles* the value
   column into the larger grid, misaligning every cell after the first
   missing row (the KITE-dev #175 BACI trade-only corruption: ×189
   income ratios from an apparently valid `.icc`);
3. sort rows by the **reversed** key columns;
4. fill `array(value, dim = unique-counts, dimnames = row-order unique
   key values)`.

`dimensions` is the model-wide inventory: for every key-column name,
the sorted unique values from the first table using it. Sorting is
load-bearing — arrays initialized from `dimensions` must align
positionally with cast arrays (KITE #142).

Deviations from the KITE-dev original, on purpose: the value column is
the table's last column rather than one literally named `value` (the
`.ice` general rule); keyless tables pass through as plain vectors;
KITE's model-specific elasticity nesting and scenario materialization
are solver concerns and stay in KITE.

## 3. Staleness

`meta$source$sha256` is the SHA-256 of the source `.ice` at compile
time. `validate_icc(icc_file, ice_file)` recomputes the hash and
errors with `STALE` on mismatch. Pipelines SHOULD validate against the
source before trusting a cached `.icc`, and recompile on staleness,
icetools version change (a warning on read), or `variable_order`
mismatch with what the consumer expects.

## 4. Compatibility policy

Same as `.ice`: additive changes bump the minor version and readers
ignore unknown fields; breaking changes bump the major version with a
migration note here. `icc_format_version` is checked by
`validate_icc()`.

## 5. Reference implementation quick reference

```r
library(icetools)
compile_ice("baseline.ice", "baseline.icc")   # compile + serialize
icc <- read_icc("baseline.icc")               # magic-byte sniffing load
validate_icc("baseline.icc", "baseline.ice")  # structure + staleness
icc$arrays$trade_share                        # dense named array
icc_tables(icc)                               # melt back (debugging)
```
