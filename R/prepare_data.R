#' Prepare vintage table
#'
#' Helper function preparing for the vintage table for a specific table.
#' Uses current time as `published`
#'
#' @param tbl_id numeric table id
#' @param con connection to the database.
#' @param meta dataframe with code, name, url and note columns
#'
#' @return data frame with `series_id` and `published` columns
#' @keywords internal

vintage_table <- function(tbl_id, con) {
  dplyr::tbl(con, "series")  |>
    dplyr::filter(table_id == tbl_id) |>
    dplyr::select(series_id=id) |>
    dplyr::collect() |>
    dplyr::mutate(published = Sys.time())
}


#' Prepare table to insert into `vintage` table
#'
#' Helper function that prepares the vintage table with the new vintages.
#' First checks if new vintages are even necessary, getting
#' to get the most recent month from the parsed data, and then checking with the most
#' recent period in the database. If there is new data,
#' Prepare a dataframe with the `series_id`s of the table and the current time
#' as their publication date - since we don't have anything better.
#'
#' Returns table ready to insert into the `vintage`table with the
#' db_writing family of functions.
#'
#' @param df parsed data (with at least a period column)
#' @param con connection to database
#' @param meta dataframe with code, name, url and note columns
#' @return vintage table ready to insert
#' @export
#'

prepare_vintage_table <- function(meta, df, con){
  new_month <- dplyr::arrange(df, period) |>
    dplyr::summarise(max = max(period)) |> dplyr::pull()
  tbl_id <-  UMARaccessR::get_table_id_from_table_code(meta$code, con)
  # get first two series last vintages if they exist. (M & A)
  series_ids <- dplyr::tbl(con, "series")  |>
    dplyr::filter(table_id == tbl_id)   |>
    dplyr::slice_min(id, n = 1)   |>
    dplyr::select(id)   |> dplyr::pull()   |>
    as.numeric()
  vint_id <- UMARaccessR::get_vintage_from_series(series_ids, con)[1,1]

  if(is.na(vint_id)){
    vintages <- vintage_table(tbl_id, con)} else {
      # get latest period from latest vintage
      max_month <- dplyr::tbl(con, "data_points")   |>
        dplyr::filter(vintage_id == vint_id)   |>
        dplyr::slice_max(period_id)   |>
        dplyr::pull(period_id)
      if(identical(max_month, new_month)) {
        stop(paste0("These monthly vintages for table ", table_name,
                    " are not new, they will not be inserted again."))
      } else {
        vintages <- vintage_table(tbl_id, con)
      }
    }
  vintages
}


#' Get and prepare data for import
#'
#' Prepares the timeseries data for importing into the database.
#'

#' @param con connection to database
#' @param df ataframe with the data_points output of \link[ZRSZfetchR ]{zrsz_bo_excel_parser}.
#' @param meta dataframe with code, name, url and note columns
#' @return a dataframe with the period_id, value and id values for all the vintages in the table.
#'
#' @export
prepare_data_table <- function(meta, df, con){
  tbl_id <-  UMARaccessR::get_table_id_from_table_code(meta$code, con)

  vintage_lookup <- dplyr::tbl(con, "series") |>
    dplyr::filter(table_id == tbl_id) |>
    dplyr::select(series_id = id, code) |>
    dplyr::left_join(dplyr::tbl(con, "vintage"), by = "series_id") |>
    dplyr::collect() |>
    dplyr::group_by(series_id) |>
    dplyr::slice_max(published)

  df |>
    dplyr::mutate(vintage_id = vintage_lookup$id) |>
    dplyr::relocate(vintage_id, period_id = period)
}
