#!/usr/bin/env Rscript

library(icetools)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: convert_to_ice.R input.rds output.ice [optional_description.yaml]")
}

input_file <- args[1]
output_file <- args[2]
yaml_file <- if (length(args) >= 3) args[3] else NULL

# Load the old initial conditions list
tables <- readRDS(input_file)

# Convert all to data.table if needed
tables <- lapply(tables, data.table::as.data.table)

# Create metadata interactively or from YAML
metadata <- create_metadata(
  tables,
  description = paste("Converted from", input_file),
  descriptions_file = yaml_file,
  non_interactive = !is.null(yaml_file)
)

# Generate the .ice file
generate_ice(tables, metadata, output_file)

cat("Conversion complete: ", output_file, "\n")
