#' Prepare table to insert into `source` table

#' Helper function that manually prepares the new line for the source table.
#'
#' @param con connection to the database.
#' @return a dataframe with the `name`, `name_long`, and `url`, columns.
#' for this table.
#' @export
prepare_source_table <- function(con){
  id <- dplyr::tbl(con, "source") |>
  dplyr::summarise(max = max(id, na.rm = TRUE)) |>
    dplyr::pull() + 1
  data.frame(id = id,
             name = "ZRSZ",
             name_long = "Zavod Republike Slovenije za Zaposlovanje",
             url = "https://www.ess.gov.si/")
}

#' Prepare table to insert into `table` table
#'
#' Helper function that manually prepares the table table.
#' Returns table ready to insert into the `table` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#'
#' @param con connection to the database
#' @param meta dataframe with code, name, url and note columns
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(meta,
                                con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("ZRSZ", con)[1,1]
  data.frame(code = meta$code,
             name = meta$name,
             source_id = source_id,
             url = meta$url,
             notes = meta$notes)
}



#' Prepare table to insert into `category` table
#'
#' Helper function that manually prepares the category table with field ids and
#' their names. Returns table ready to insert into the `category` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#'
#' @param con connection to the database
#' @param meta dataframe with code, name, url and note columns
#'
#' @return a dataframe with the `id`, `name`, `source_id` for each category that
#' the table is a member of.
#' @export
#'
prepare_category_table <- function(meta, con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("ZRSZ", con)[1,1]
  max_cat <- UMARaccessR::get_max_category_id_for_source(source_id, con)[1,1]
  data.frame(id = max_cat + 1,
             name = meta$category,
             source_id = source_id)
}


#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that manually prepares the category_relationship table.
#' Returns table ready to insert into the `category_relationship` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#'
#' @param con connection to the database
#' @param meta dataframe with code, name, url and note columns
#'
#' @return a dataframe with the `id`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(meta, con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("ZRSZ", con)[1,1]
  x <- meta$category
  id <- dplyr::tbl(con, "category") |>
    dplyr::filter(name == x) |>
    dplyr::pull(id)

  data.frame(id = id,
             parent_id = 0,
             source_id = source_id)
}



#' Prepare table to insert into `category_table` table
#'
#' Helper function that manually prepares the category_table table.
#' Returns table ready to insert into the `category_table` table with the db_writing family
#' of functions from SURSfetchR using \link[SURSfetchR]{sql_function_call}
#' A single table can have multiple parents - meaning
#' it is member of several categories (usually no more than two tho). .
#'
#' @param meta dataframe with code, name, url and note columns
#' @param con connection to the database
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#'
prepare_category_table_table <- function(meta, con) {
  source_id <- UMARaccessR::get_source_code_from_source_name("ZRSZ", con)[1,1]
  x <- meta$category
  id <- dplyr::tbl(con, "category") |>
    dplyr::filter(name == x) |>
    dplyr::pull(id)
  data.frame(code = meta$code,
             category_id = id,
             source_id = source_id) |>
    dplyr::rowwise() |>
    dplyr::mutate(table_id = UMARaccessR::get_table_id_from_table_code(code, con)) |>
    dplyr::select(-code) |>
    na.omit()
}


#' Prepare table to insert into `table_dimensions` table
#'
#' Helper function that manually prepares the table_dimensions table.
#' Returns table ready to insert into the `table_dimensions`table with the
#' db_writing family of functions.
#'
#' @param con connection to the database
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export
#'
prepare_table_dimensions_table <- function(meta, con){
  x <- meta$code
  data.frame(code = x)  |>
    dplyr::mutate(table_id = UMARaccessR::get_table_id_from_table_code(code, con)) |>
    dplyr::mutate(dimension = "Vrednost",
                  is_time = rep(0)) |>
    dplyr::select(-code) |>
    dplyr::arrange(table_id)
}
