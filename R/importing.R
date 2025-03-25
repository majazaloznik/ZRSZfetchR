#' Import structural metadata for a ZRSZ table
#'
#' Umbrella function that prepares and import all the metadata tables into
#' the database.
#'
#' @param meta row with metadata for the table, see raw_data
#' @param con connection to database
#' @param schema schema name, defaults to platform
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @returns nothing
#' @export
#'
ZRSZ_import_structure <- function(meta, con, schema = "platform", keep_vintage = FALSE) {
  message("Importing structure data for: ", meta$code, " into schema ", schema)
  # Create list to store all results
  insert_results <- list()
  # prepare and insert table
  table_table <- prepare_table_table(meta, keep_vintage, con, schema)
  insert_results$table <- UMARimportR::insert_new_table_table(con, table_table, schema)
  message("Table insert: ", insert_results$table$count, " rows")
  # preapre and insert category table
  category_table <- prepare_category_table(meta, con, schema)
  insert_results$category <- UMARimportR::insert_new_category(con, category_table, schema)
  message("Category insert: ", insert_results$category$count, " rows")
  # prepare and insert category relationship table
  category_relationship_table <- prepare_category_relationship_table(meta, con, schema)
  insert_results$category_relantionship <- UMARimportR::insert_new_category_relationship(
    con, category_relationship_table, schema)
  message("Category relationship insert: ", insert_results$category_relantionship$count, " rows")
  # prepare and insert category table table
  category_table_table <- prepare_category_table_table(meta, con, schema)
  insert_results$category_table <- UMARimportR::insert_new_category_table(
    con, category_table_table, schema)
  message("Category table insert: ", insert_results$category_table$count, " rows")
  # prepare and insert table dimension table
  table_dimension_table <- prepare_table_dimensions_table(meta, con, schema)
  insert_results$table_dimensions <- UMARimportR::insert_new_table_dimensions(
    con, table_dimension_table, schema)
  message("Table dimensions insert: ", insert_results$table_dimensions$count, " rows")
  # prepare and select dimension levels before inserting them
  dimension_levels_table <- prepare_dimension_levels_table(meta[1,], con, schema)
  insert_results$dimension_levels <- UMARimportR::insert_new_dimension_levels(
    con, dimension_levels_table, schema)
  message("Dimension levels insert: ", insert_results$dimension_levels$count, " rows")
  # prepare and insert series table
  series_table <- prepare_series_table(meta, con, schema)
  insert_results$series <- UMARimportR::insert_new_series(con, series_table, schema)
  message("Series insert: ", insert_results$series$count, " rows")
  # prepare and insert series levels table
  series_levels_table <- prepare_series_levels_table(meta, con, schema)
  insert_results$series_levels <- UMARimportR::insert_new_series_levels(
    con, series_levels_table, schema)
  message("Series levels insert: ", insert_results$series_levels$count, " rows")
  invisible(insert_results)
}


#' Insert data points from ZRSZ
#'
#' Function to prepare and insert ZRSZ data points. The function first prepares
#' the required vintages and inserts them, then prepares the data points
#' table and isnerts it. The function returns the results invisibly.
#'
#' This is a ZRSZ specific function, which should be followed by the generic
#' UMARimportR function to write the vintage hashes and clean up redundant
#' vintages.
#'
#' @param meta ZRSZ row with metadata for the table, see raw_data
#' @param con Database connection
#' @param schema Schema name
#'
#' @return Insertion results (invisibly)
#' @export
ZRSZ_import_data_points <- function(meta, con, schema = "platform") {
  message("Importing data points from: ", meta$code, " into schema ", schema)
  # collect outputs from the funcitons into one result list
  result <- list()
  # prepare datapoints
  df <- get_datapoints(meta)
  # prepare ZRSZ vintage table
  vintages <- prepare_vintage_table(meta, df, con, schema)
  if (is.null(vintages)) {
    warning("No new vintages to insert, exiting.")
    return(invisible(NULL))
  }
  # import vintages
  result$vintages <- UMARimportR::insert_new_vintage(con, vintages, schema)
  # prepare datapoint table for insertion
  prep_data <- prepare_datapoint_table(df, meta, con, schema)
  # Insert the prepared data points
  result$data <- UMARimportR::insert_prepared_data_points(prep_data, con, schema)
  # Return results invisibly
  invisible(result)
}
