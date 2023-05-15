#' Insert new source.
#'
#' This is a one off, but is here for completeness. Used to insert the ZRSZ source
#' into the database. Also adds the cover category for the source.
#'
#' @param con connection to the database.
#'
#' @return nothing
#' @export
insert_new_source <- function(con) {
  SURSfetchR::sql_function_call(con,
                                "insert_new_source",
                                as.list(prepare_source_table(con)))
  source_id <- UMARaccessR::get_source_code_from_source_name("ZRSZ", con)
  SURSfetchR::sql_function_call(con,
                                "insert_new_category",
                                list(id = 0,
                                     name = "ZRSZ",
                                     source_id = source_id[1,1]))
}



#' Insert table structure data for the tables
#'
#' This is here for the record/reprodcibility, because it was only done once.
#' WHen the five tables were first added to the database, a set of nine
#' tables needed to be populated with appropriate metadata about those tables.
#' This umbrella function calls the respective SQL functions for each
#' of the nine tables and the table preparation functions to insert the data.
#'
#' @param meta is the dataframe in the package with the table names
#' @param con connection to the database.
#'
#' @return nothing
#' @export
#'
insert_new_table_structures <- function(meta, con) {
  res <- list()
  res[[1]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_table",
                                as.list(prepare_table_table(meta, con)))
  res[[2]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_category",
                                as.list(prepare_category_table(meta, con)))
  res[[3]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_category_relationship",
                                as.list(prepare_category_relationship_table(meta, con)))
  res[[4]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_category_table",
                                as.list(prepare_category_table_table(meta, con)))
  res[[5]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_table_dimensions",
                                as.list(prepare_table_dimensions_table(meta, con)))
  res[[6]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_dimension_levels",
                                as.list(prepare_dimension_levels_table(meta, con)))
  res[[7]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_unit",
                                list(name = "\u0161tevilo"))
  res[[8]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_series",
                                unname(as.list(prepare_series_table(meta, con))))
  res[[9]] <- SURSfetchR::sql_function_call(con,
                                "insert_new_series_levels",
                                unname(as.list(prepare_series_levels_table(meta, con))))
 res
}




#' Insert new data for a table i.e. a vintage
#'
#' When new data for a table (one of the Excel's) is added, these are new
#' vintages. This function inserts a set of new vintages and their corresponding
#' data points to the database. It is possible to only have new monthly not annual
#' vintages.
#'

#' @param meta is the dataframe in the package with the table names
#' @param df dataframes with the data_points
#' @param con connection to database
#'
#' @return list of tables with counts for each inserted row.
#' @export
#'
#' @examples
#' \dontrun{
#' purrr::walk(master_list_surs$code, ~insert_new_data(.x, con))
#' }
insert_new_data <- function(meta, df, con) {
  vintages <- prepare_vintage_table(meta, df, con)
  # insert monthly data
  res <- SURSfetchR::sql_function_call(con,
                                            "insert_new_vintage",
                                            as.list(vintages))

  insert_data_points(meta, df, con)

  lapply(res, sum)
}



#' Insert datapoints into data_point table
#'
#'
#' So, the function extracts and preps the data with \link[ZRSZfetchR]{prepare_data_table}
#' and writes it to a temporary table in the database.
#'
#' It inserts any new periods into the period table,
#' adds the data points to the data point table.
#' @param df dataframes with the data_points
#' output of \link[ZRSZfetchR]{zrsz_bo_excel_parser} or similar.
#' @param con connection to database
#' @param meta is the dataframe in the package with the table names
#' @return nothing, just some printing along the way
#' @export
#'
insert_data_points <- function(meta, df, con){
  on.exit(dbExecute(con, sprintf("drop table tmp")))

  df <- prepare_data_table(meta, df, con)
  df |>
    dplyr::mutate(interval_id = "M") -> df

  dbWriteTable(con,
               "tmp",
               df,
               temporary = TRUE,
               overwrite = TRUE)

  # insert into period table periods that are not already in there.
  x <- dbExecute(con, sprintf("insert into %s.period
                        SELECT tmp.period_id, tmp.interval_id
                        FROM tmp
                        LEFT JOIN %s.period
                        ON tmp.period_id = period.id
                        AND tmp.interval_id = period.interval_id
                        WHERE period.id IS NULL
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform"),
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows inserted into the period table"))

  # insert data into main data_point table
  x <- dbExecute(con, sprintf("insert into %s.data_points
                       select vintage_id, period_id, value from tmp
                       on conflict do nothing",
                       dbQuoteIdentifier(con, "test_platform")))
  print(paste(x, "new rows inserted into the data_points table"))

}
