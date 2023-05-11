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


