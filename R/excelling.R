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

#' ZRSZ BO OS Excel table parser
#'
#' Parsing function to extract the data in the Stopnja registrirane brezposelnosti
#' po občinah in statističnih regijah Excel file. The parser extracts data for
#' Slovenia from each sheet and combines them into a single time series.
#'
#' @param file_path path to the excel file
#'
#' @return dataframe with period and value ready to import into database
#' @export
zrsz_bo_os_excel_parser <- function(file_path) {
  # Get the sheet names
  sheet_names <- readxl::excel_sheets(file_path)

  # Read the data from each sheet and transform
  output_list <- lapply(sheet_names, function(sheet_name) {
    # Read the first 20 rows to find header and data positions
    df <- readxl::read_excel(file_path, sheet = sheet_name, range = "A1:M15",
                           col_names = FALSE,.name_repair = "minimal")  # Don't use first row as column names

    # Find header row (looking for "Območne službe")
    header_row <- which(df[[1]] == "Obmo\u010dne slu\u017ebe")
    if (length(header_row) == 0) {
      stop(paste("Could not find header row with 'Obmocne sluzbe' in sheet", sheet_name))
    }

    # Find Slovenia row (looking for "Slovenija")
    slovenia_row_num <- which(df[[1]] == "Slovenija")
    if (length(slovenia_row_num) == 0) {
      stop(paste("Could not find row with 'Slovenija' in sheet", sheet_name))
    }

    # Now read the actual data with correct row positions
    df <- suppressMessages(readxl::read_excel(file_path, sheet = sheet_name,
                           range = paste0("A", header_row, ":M", slovenia_row_num),
                           col_names = FALSE,.name_repair = "minimal"))  # Don't use first row as column names

    # Get the header row (first row after reading)
    header <- df[1,]

    # Get the Slovenia row
    slovenia_row <- df[nrow(df),]

    colnames(slovenia_row) <- header

    # Convert to long format, excluding the first column (which contains row names)
    slovenia_data <- slovenia_row[-1] |>
      tidyr::pivot_longer(everything(), names_to = "month", values_to = "value") |>
      dplyr::mutate(
        # Convert month abbreviations or Roman numerals to numbers
        month_num = dplyr::case_when(
          # Handle three-letter abbreviations with period
          tolower(month) == "jan." ~ "01",
          tolower(month) == "feb." ~ "02",
          tolower(month) == "mar." ~ "03",
          tolower(month) == "apr." ~ "04",
          tolower(month) == "maj" ~ "05",
          tolower(month) == "jun." ~ "06",
          tolower(month) == "jul." ~ "07",
          tolower(month) == "avg." ~ "08",
          tolower(month) == "sep." ~ "09",
          tolower(month) == "sept." ~ "09",
          tolower(month) == "sep" ~ "09",
          tolower(month) == "okt." ~ "10",
          tolower(month) == "okt" ~ "10",
          tolower(month) == "nov." ~ "11",
          tolower(month) == "nov" ~ "11",
          tolower(month) == "dec." ~ "12",
          tolower(month) == "dec" ~ "12",
          # Handle Roman numerals
          month == "I" ~ "01",
          month == "II" ~ "02",
          month == "III" ~ "03",
          month == "IV" ~ "04",
          month == "V" ~ "05",
          month == "VI" ~ "06",
          month == "VII" ~ "07",
          month == "VIII" ~ "08",
          month == "IX" ~ "09",
          month == "X" ~ "10",
          month == "XI" ~ "11",
          month == "XII" ~ "12",
          TRUE ~ NA_character_
        ),
        # Create period string in format YYYYMM
        period = paste0(sheet_name, "M", month_num)
      ) |>
      dplyr::select(period, value) |>
      dplyr::filter(!is.na(value))  # Remove any NA values
  })

  # Combine all sheets
  do.call(rbind, output_list) |>
    dplyr::arrange(period)  # Sort by period
}

