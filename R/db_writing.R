#' Insert new source.
#'
#' This is a one off, but is here for completeness. Used to insert the ZRSZ source
#' into the database
#'
#' @param con connection to the database.
#'
#' @return number of rows inserted
#' @export
insert_new_source <- function(con) {
  SURSfetchR::sql_function_call(con,
                                "insert_new_source",
                                as.list(prepare_source_table(con)))
}
