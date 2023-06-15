#' Script for the BO table
#'
#' this is an automation script that does the following: downloads the xlsx file
#' on the hardcoded which changes with the year. (in January, the tryCatch construct
#' should use the alternative url instead, to get the previous year).
#' The file is downloaded to a tempfile, then the data is parsed and written into
#' the database.
#'
#' @param con connection to database
#'
#' @return number of rows inserted
#' @export
zrsz_bo_script <- function(con){
  # get urls
  current_year <- format(Sys.Date(), "%Y")
  url_part <- "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/Mesecno_gibanje_BO_1992-"
  url <- paste0(url_part, current_year, ".xlsx")
  url_alt <- paste0(url_part, as.numeric(current_year)-1, ".xlsx")

  meta <- ZRSZfetchR:::meta[1,]
  temp_file <- tempfile(fileext = ".xlsx")

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
  df <- zrsz_bo_excel_parser(temp_file)
  # write to
  out <- insert_new_data(meta, df, con)

  file.remove(temp_file)
  out
}
