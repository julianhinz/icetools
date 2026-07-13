#' Create metadata for the components of a .ice file
#'
#' Builds the `metadata.yaml` structure for a list of initial-condition
#' components. Two component kinds are supported (see `docs/ice-schema.md`):
#'
#' * **Tables** (data.frames/data.tables): the *last column is the value
#'   column*, every column before it is a key column. Column names and
#'   types are recorded and are authoritative when the file is read back.
#' * **Scalars** (length-1 numeric/character/logical vectors), e.g.
#'   model-wide nest elasticities: recorded in a `scalars` section and
#'   stored in a `scalars.csv` archive member by [generate_ice()].
#'
#' @param tables Named list of components (data.frames/data.tables and/or
#'   scalars). Names must be unique and archive-safe
#'   (`^[A-Za-z0-9][A-Za-z0-9._-]*$`; `metadata` and `scalars` are
#'   reserved).
#' @param description A general description of the .ice file.
#' @param version Artifact version string recorded in the metadata
#'   (default `"1.0"`). This is *your* dataset's version, not the .ice
#'   format version, which is stamped separately by [generate_ice()].
#' @param timestamp Timestamp recorded in the metadata (default
#'   [Sys.time()]).
#' @param non_interactive If `TRUE` (default), missing component
#'   descriptions fall back to a generic placeholder. Set to `FALSE` to be
#'   prompted for each missing description; this requires an interactive
#'   session and errors in scripts (the pre-0.2.0 default prompted by
#'   default, which hung pipelines).
#' @param descriptions_file Optional path to a YAML file mapping component
#'   names to descriptions.
#' @param identifier,title,license,contact,citation Optional artifact-level
#'   fields recorded verbatim in the metadata.
#' @param model,scenario,sources,assumptions,generator,coordinates,provenance
#'   Optional free-form lists recorded verbatim: model description (id,
#'   spec source/ref), scenario coordinates (base year, target year,
#'   scenario id), source provenance entries, free-form assumptions,
#'   generator info (package/repo/commit), artifact coordinates, and
#'   upstream provenance. See `docs/ice-schema.md` for the conventions
#'   used by the KITE pipeline.
#' @param conventions Optional named list declaring data conventions
#'   (format 1.2), e.g. `list(trade_elasticity = "positive_theta")`.
#'   When absent and a `trade_elasticity` component is present,
#'   [generate_ice()] auto-detects the sign convention from the data
#'   (`"positive_theta"` or `"legacy_negative"`); files without the
#'   field are assumed `legacy_negative` by convention. See
#'   `docs/ice-schema.md` section 3.1.
#' @return A list ready to be passed to [generate_ice()].
#' @examples
#' components <- list(
#'   trade_share = data.frame(origin = c("USA", "CHN"),
#'                            destination = c("CHN", "USA"),
#'                            value = c(0.4, 0.6)),
#'   consumption = 0.35  # scalar nest elasticity
#' )
#' md <- create_metadata(components, description = "Example")
#' @export
create_metadata <- function(tables,
                            description = "",
                            version = "1.0",
                            timestamp = Sys.time(),
                            non_interactive = TRUE,
                            descriptions_file = NULL,
                            identifier = NULL,
                            title = NULL,
                            license = NULL,
                            contact = NULL,
                            citation = NULL,
                            model = NULL,
                            scenario = NULL,
                            sources = NULL,
                            assumptions = NULL,
                            generator = NULL,
                            coordinates = NULL,
                            provenance = NULL,
                            conventions = NULL) {
  if (!is.list(tables) || is.data.frame(tables)) {
    cli::cli_abort("'tables' must be a list of components (data.frames and/or scalars).")
  }
  components <- split_components(tables)

  if (!non_interactive && !interactive()) {
    cli::cli_abort(c(
      "non_interactive = FALSE requires an interactive session.",
      "i" = "In scripts, provide descriptions via 'descriptions_file' or accept the defaults with non_interactive = TRUE."
    ))
  }

  # optional description YAML input
  table_descriptions <- list()
  if (!is.null(descriptions_file)) {
    table_descriptions <- yaml::read_yaml(descriptions_file)
  }

  get_description <- function(name, fallback) {
    provided <- table_descriptions[[name]]
    if (!is.null(provided)) return(provided)
    if (!non_interactive) {
      return(readline(paste0("Description for '", name, "': ")))
    }
    fallback
  }

  metadata <- list(
    format_version = ICE_FORMAT_VERSION,
    description = description,
    version = version,
    timestamp = as.character(timestamp)
  )

  # optional artifact-level fields, recorded verbatim when supplied
  optional <- list(
    identifier = identifier, title = title, license = license,
    contact = contact, citation = citation, model = model,
    scenario = scenario, sources = sources, assumptions = assumptions,
    generator = generator, coordinates = coordinates, provenance = provenance,
    conventions = conventions
  )
  for (field in names(optional)) {
    if (!is.null(optional[[field]])) metadata[[field]] <- optional[[field]]
  }

  metadata$tables <- list()
  for (table_name in names(components$tables)) {
    table <- components$tables[[table_name]]

    # last column is the value column, everything before it is a key;
    # factors are stored as character in the CSV, so record them as such
    column_type <- function(col) {
      if (is.factor(col)) "character" else class(col)[1]
    }
    keys <- lapply(names(table)[-ncol(table)], function(col) {
      list(name = col,
           type = column_type(table[[col]]),
           description = paste("Key column for", col))
    })
    value <- list(
      name = names(table)[ncol(table)],
      type = column_type(table[[ncol(table)]]),
      description = "Value column"
    )

    metadata$tables[[table_name]] <- list(
      description = get_description(table_name, paste("Table for", table_name)),
      keys = keys,
      value = value
    )
  }

  if (length(components$scalars)) {
    metadata$scalars <- list()
    for (scalar_name in names(components$scalars)) {
      metadata$scalars[[scalar_name]] <- list(
        description = get_description(scalar_name, paste("Scalar", scalar_name)),
        type = class(components$scalars[[scalar_name]])[1]
      )
    }
  }

  metadata
}
