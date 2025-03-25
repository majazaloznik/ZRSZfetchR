#' Prepare vintage table
#'
#' Helper function preparing for the vintage table for a specific table.
#' Uses current time as `published`
#'
#' @param tbl_id numeric table id
#' @param con connection to the database.
#' @param schema schema name
#'
#' @return data frame with `series_id` and `published` columns
#' @keywords internal

vintage_table <- function(tbl_id, con, schema){
  # Use a fixed timestamp for testing environments
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    published_time <- as.POSIXct("2023-01-01 12:00:00")
  } else {
    published_time <- Sys.time()
  }
  
  UMARaccessR::sql_get_series_ids_from_table_id(tbl_id, con, schema) |>
     dplyr::select(series_id=id) |>
    dplyr::mutate(published = published_time)
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
#' @param schema schema name
#'
#' @return vintage table ready to insert
#' @export
#'
prepare_vintage_table <- function(meta, df, con, schema){
  new_month <- dplyr::arrange(df, period) |>
    dplyr::summarise(max = max(period)) |> dplyr::pull()
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, meta$code, schema)
  # get first series last vintages if they exist. (M & A)
  series_ids <- UMARaccessR::sql_get_series_ids_from_table_id(tbl_id,con, schema) |>
    dplyr::slice_min(id, n = 1) |>
    dplyr::pull()|>
    as.numeric()
  vint_id <- UMARaccessR::sql_get_vintage_from_series(con, series_ids, schema = schema)

  if(is.null(vint_id)){
    vintages <- vintage_table(tbl_id, con, schema)} else {
      # get latest period from latest vintage
      max_month <- UMARaccessR::sql_get_data_points_from_vintage(con, vint_id, schema)|>
        dplyr::slice_max(period_id)   |>
        dplyr::pull(period_id)
      if(identical(max_month, new_month)) {
        warning(paste0("These monthly vintages for table ", meta$code,
                    " are not new, they will not be inserted again."))
        return(NULL)  # Return NULL instead of stopping
      } else {
        vintages <- vintage_table(tbl_id, con, schema)
      }
    }
  vintages
}


#' Get datapoints
#'
#' Get the datapoints by downloading and parsing the appropriate excel file
#'
#' @param meta dataframe row with the table metadata, with columns `code`,
#' `partial_file_url`, `extension`, `excelling_function`
#'
#' @return a dataframe with the period_id, value and id values for all the vintages in the table.
#'
#' @export
get_datapoints <- function(meta){
  # get urls
  current_year <- format(Sys.Date(), "%Y")
  url <- paste0(meta$partial_file_url, current_year, meta$extension)
  url_alt <- paste0(meta$partial_file_url, as.numeric(current_year)-1, meta$extension)

  # download the data
  temp_file <- tempfile(fileext = meta$extension)
  tryCatch(
    {
      download.file(url, destfile = temp_file, mode = "wb")
    },
    error = function(e) {
      # Download failed, try alternative URL
      download.file(url_alt, destfile = temp_file, mode = "wb")
    }
  )
  # parse the data
  parser_func <- get(meta$excelling_function, envir = asNamespace("ZRSZfetchR"))
  # Parse the data using the appropriate function
  df <- parser_func(temp_file)
  file.remove(temp_file)
  df |>
    dplyr::mutate(value = as.numeric(value))
}


#' Prepare datapoint table for insertion
#'
#' Prepare the data points for insertion into the database. This is after
#' the vintages have been inserted already, all of which happens in
#' \link[ZRSZfetchR]{ZRSZ_import_data_points}
#'
#' @param df data frame with the data points from \link[ZRSZfetchR]{get_datapoints}
#' @param meta dataframe row with the table metadata, with columns `code`
#' @param con connection to the database
#' @param schema schema name
#'
#' @return a list with data, table_id, dimension_names and dimension_ids
#' @export
prepare_datapoint_table <- function(df, meta, con, schema){
  # get vintage id
  tbl_id <-  UMARaccessR::sql_get_table_id_from_table_code(con, meta$code, schema)

  df <- df |>
    dplyr::mutate(Vrednost = "0",
                  flag = "") |>
    dplyr::rename(time = period)

  list(data = df,
       table_id = tbl_id,
       dimension_names = "Vrednost",
       dimension_ids = UMARaccessR::sql_get_dimension_id_from_table_id_and_dimension(tbl_id, "Vrednost", con, schema),
       interval_id = "M")
}
