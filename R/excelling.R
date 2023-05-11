#' ZRSZ BO Execl table parser
#'
#' Parsing function to extract the data in the Gibanje registrirane brezposelnosti
#' po mesecih Excel file, usually published [here](https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/),
#' The parser joins the tables from separate sheets and should work indefinitely
#' even if more sheets are added as long as the strucutre of the tables remains the
#' same. The data is cleaned of missing values and perpared in long format ready for
#' importing into the database.
#'
#' Hardcoded failure points:
#' * The strucutre of the table
#' * Skipping a single line at the top
#'
#' @param file_path path to the excel file
#'
#' @return dataframe with period and value ready to import into database
#' @export
zrsz_bo_excel_parser <- function(file_path){
  # Get the sheet names
  sheet_names <- openxlsx::getSheetNames(file_path)
  # Read the data from the sheet and transform
  output_list <- lapply(sheet_names, function(sheet_name) {
    openxlsx::read.xlsx(file_path, sheet = sheet_name, startRow = 2) |>
      dplyr::filter(mesec != "povpre\u010dje leta") |>
      dplyr::mutate(period =   sprintf("%02d", dplyr::row_number())) |>
      dplyr::select(-mesec) |>
      tidyr::pivot_longer(!period, names_to="year", values_to = "value") |>
      dplyr::mutate(period = paste0(year, "M", period)) |>
      dplyr::arrange(period) |>
      dplyr::select(-year) |>
      dplyr::filter(!is.na(value))
  })
  # join all sheets together
  do.call(rbind, output_list)
}
