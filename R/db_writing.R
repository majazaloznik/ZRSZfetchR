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
#' #' @export
#'
insert_new_table_structures <- function(meta, con) {

  SURSfetchR::sql_function_call(con,
                                "insert_new_table",
                                as.list(prepare_table_table(meta, con)))
  SURSfetchR::sql_function_call(con,
                                "insert_new_category",
                                as.list(prepare_category_table(meta, con)))
  SURSfetchR::sql_function_call(con,
                                "insert_new_category_relationship",
                                as.list(prepare_category_relationship_table(meta, con)))


}
