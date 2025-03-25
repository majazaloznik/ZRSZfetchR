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
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param meta dataframe with code, name, url and note columns
#' @param schema schema name
#' @param keep_vintage logical indicating whether to keep vintages, defaults to F
#'
#' @return a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
#' for this table.
#' @export
prepare_table_table <- function(meta, keep_vintage = FALSE, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "ZRSZ", schema)
  data.frame(code = meta$code,
             name = meta$name,
             source_id = source_id,
             url = meta$url,
             notes = meta$notes,
             keep_vintage = keep_vintage)
}



#' Prepare table to insert into `category` table
#'
#' Helper function that manually prepares the category table with field ids and
#' their names. Returns table ready to insert into the `category` table with the db_writing family
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param meta dataframe with code, name, url and note columns
#' @param schema schema name
#'
#' @return a dataframe with the `id`, `name`, `source_id` for each category that
#' the table is a member of.
#' @export
#'
prepare_category_table <- function(meta, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "ZRSZ", schema)
  max_cat <- UMARaccessR::sql_get_max_category_id_for_source(source_id, con, schema)
  data.frame(id = max_cat + 1,
             name = meta$category,
             source_id = source_id)
}


#' Prepare table to insert into `category_relationship` table
#'
#' Helper function that manually prepares the category_relationship table.
#' Returns table ready to insert into the `category_relationship` table with the db_writing family
#' of functions from `UMARimportR`.
#'
#' @param con connection to the database
#' @param meta dataframe with code, name, url and note columns
#' @param schema schema name
#'
#' @return a dataframe with the `id`, `parent_id`, `source_id` for each relationship
#' betweeh categories
#' @export
#'
prepare_category_relationship_table <- function(meta, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "ZRSZ", schema)
  cat_name <- meta$category
  id <- UMARaccessR::sql_get_category_id_from_name(cat_name, con, source_id, schema)
  data.frame(id = id,
             parent_id = 0,
             source_id = source_id)
}



#' Prepare table to insert into `category_table` table
#'
#' Helper function that manually prepares the category_table table.
#' Returns table ready to insert into the `category_table` table with the db_writing family
#' of functions from `UMARimportR`.
#' A single table can have multiple parents - meaning
#' it is member of several categories (usually no more than two tho). .
#'
#' @param meta dataframe with code, name, url and note columns
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the `category_id` `table_id` and `source_id` columns for
#' each table-category relationship.
#' @export
#' @importFrom stats na.omit
prepare_category_table_table <- function(meta, con, schema = "platform") {
  source_id <- UMARaccessR::sql_get_source_code_from_source_name(con, "ZRSZ", schema)
  cat_name <- meta$category
  id <- UMARaccessR::sql_get_category_id_from_name(cat_name, con, source_id, schema)
  data.frame(code = meta$code,
             category_id = id,
             source_id = source_id) |>
    dplyr::rowwise() |>
    dplyr::mutate(table_id = UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)) |>
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
#' @param meta dataframe with code, name, url and note columns
#' @param schema schema name
#'
#' @return a dataframe with the `table_id`, `dimension_name`, `time` columns for
#' each dimension of this table.
#' @export
#' @importFrom stats na.omit
prepare_table_dimensions_table <- function(meta, con, schema = "platform"){
  x <- meta$code
  data.frame(code = x)  |>
    dplyr::mutate(table_id = UMARaccessR::sql_get_table_id_from_table_code(con, code, schema)) |>
    dplyr::mutate(dimension = "Vrednost",
                  is_time = rep(0)) |>
    dplyr::select(-code) |>
    dplyr::arrange(table_id)|>
    na.omit()
}



#' Prepare table to insert into `dimension_levels` table
#'
#' Helper function that manually prepares the dimension_levels for each
#' table and get their codes and text.
#' Returns table ready to insert into the `dimension_levels`table with the
#' db_writing family of functions.
#'
#' @param meta dataframe with code, name, url and note columns
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the `dimension_id`, `values` and `valueTexts`
#' columns for this table.
#' @export
#' @importFrom stats na.omit
prepare_dimension_levels_table <- function(meta, con, schema = "platform") {
  table_id <- UMARaccessR::sql_get_table_id_from_table_code(con, meta$code, schema)
  dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(table_id, "Vrednost", con, schema)
  data.frame(tab_dim_id = dim_id,
             level_value = "0",
             level_text = meta$name)|>
    na.omit()
}




#' Prepare table to insert into `series` table
#'
#' This currently only works for a single series table, since that is the only
#' case i have right now, but will expand if necessary.
#'
#' @param meta dataframe with code, name, url and note columns
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the following columns: `name_long`, `code`,
#' `unit_id`, `table_id` and `interval_id`for each series in the table
#' well as the same number of rows as there are series
#' @export


prepare_series_table <- function(meta, con, schema){
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, meta$code, schema)
  dim_id <- UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Vrednost", con, schema)
  dim_level <- UMARaccessR::sql_get_dimension_levels_from_table_id(tbl_id, con, schema) |>
    dplyr::pull(level_value)
  data.frame(table_id = tbl_id,
             name_long = meta$name,
             unit_id = UMARaccessR::sql_get_unit_id_from_unit_name("\u0161tevilo", con, schema),
             code = paste0("ZRSZ--", meta$code, "--", dim_level, "--M"),
             interval_id = "M")
}


#' Prepare table to insert into `series_levels` table
#'
#' Helper function that extracts the individual levels for each series and
#' gets the correct dimension id for each one and the correct series id to
#' keep with the constraints.
#' Returns table ready to insert into the `series_levels`table with the
#' db_writing family of functions.
#'
#'
#' @param meta dataframe with code, name, url and note columns
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a dataframe with the `series_id`, `tab_dim_id`, `value` columns
#' all the series-level combinatins for this table.
#' @export
#'
prepare_series_levels_table <- function(meta, con, schema) {
  tbl_id <- UMARaccessR::sql_get_table_id_from_table_code(con, meta$code, schema)

  dimz <- UMARaccessR::sql_get_dimensions_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(is_time != TRUE) |>
    dplyr::pull(id)

  UMARaccessR::sql_get_series_from_table_id(tbl_id, con, schema) |>
    dplyr::filter(table_id == tbl_id) |>
    dplyr::select(table_id, id, code) |>
    tidyr::separate(code, into = c("x1", "x2",paste0(dimz), "int"), sep = "--") |>
    dplyr::select(series_id = id,  paste0(dimz)) |>
    tidyr::pivot_longer(-series_id, names_to = "tab_dim_id") |>
    dplyr::rename(level_value = value) |>
    as.data.frame()
}
