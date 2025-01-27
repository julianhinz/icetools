# `icetools`: Tools for Managing Initial Conditions for Quantitative Trade Models

<img src="man/figures/logo.svg" alt="icetools logo" width="200"/>

`icetools` is an R package designed for creating, reading, validating, and managing `.ice` files, a custom file format used for storing initial conditions for quantitative trade models. The `.ice` format ensures consistency, readability, and extensibility for data and metadata handling in trade simulations.

---

## **Features**

- **`generate_ice`:** Create a `.ice` file from data tables and metadata.
- **`read_ice`:** Read and extract data and metadata from a `.ice` file.
- **`validate_ice`:** Validate the structure and contents of a `.ice` file.
- **`create_metadata`:** Automatically generate metadata for data tables, with options for user-defined descriptions.
- **`list_tables`:** List all tables in a `.ice` file with descriptions.
- **`extract_table`:** Extract individual tables from a `.ice` file.

---

## **Installation**

You can install the development version of `icetools` directly from GitHub:

```R
# install.packages("devtools")
devtools::install_github("julianhinz/icetools")
```

---

## **Quick Start**

```R
# Load the package
library(icetools)

# Example data
tables <- list(
  trade_flows = data.frame(country = c("USA", "CHN"), value = c(100, 200)),
  tariffs = data.frame(country = c("USA", "CHN"), value = c(0.1, 0.2))
)

# Create metadata
metadata <- create_metadata(tables, description = "Example initial conditions")

# Generate a .ice file
generate_ice(tables, metadata, "example.ice")

# Read the .ice file
ice <- read_ice("example.ice")
print(ice)

# Validate the .ice file
validate_ice("example.ice")
```

---

## **How to Cite**

If you use `icetools` in your research, please cite it as follows:

```bibtex
@Misc{Hinz2025icetools,
  author = {Julian Hinz},
  title = {icetools: Tools for Managing Initial Conditions for Quantitative Trade Models},
  year = {2025},
  url = {https://github.com/julianhinz/icetools},
  note = {Version 0.1.0}
}
```

---

## **License**

`icetools` is licensed under the MIT License.

---

## **Contributing**

Contributions are welcome! Please open an issue or submit a pull request on GitHub.
