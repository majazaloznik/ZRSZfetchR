#' Extract single sseries from existing MIN-6 files
#'
#' This currently extracts column B, but i can extend to add the column
#' parameter as well. The whole thing depends on the year-month folder
#' structure, so this is a one off for these particular files. just saving
#' it to the package to be more organized.
#'
#' @param base_path where the files are hidden
#' @param target_row text expected in first column or target row
#'
#' @returns dataframe with period and value columns, period being 2025M12 format
#' @export
#'
extract_min6_series <- function(base_path, target_row = "2\\. ISKALEC PONOVNE ZAPOSLITVE") {
  month_map <- c(
    "januar" = 1, "februar" = 2, "marec" = 3, "april" = 4,
    "maj" = 5, "junij" = 6, "julij" = 7, "avgust" = 8,
    "september" = 9, "oktober" = 10, "november" = 11, "december" = 12
  )

  year_dirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  year_dirs <- year_dirs[grepl("\\d{4}$", basename(year_dirs))]

  results <- lapply(year_dirs, \(year_dir) {
    year <- as.integer(basename(year_dir))
    month_dirs <- list.dirs(year_dir, recursive = FALSE, full.names = TRUE)

    lapply(month_dirs, \(month_dir) {
      folder_name <- tolower(basename(month_dir))
      month_match <- month_map[names(month_map)[
        vapply(names(month_map), \(m) grepl(m, folder_name), logical(1))
      ]]
      if (length(month_match) == 0) return(NULL)
      month <- month_match[1]

      files <- list.files(month_dir, pattern = "MIN-6",
                          full.names = TRUE, ignore.case = TRUE)
      files <- files[grepl("\\.(xlsx?|xls)$", files, ignore.case = TRUE)]

      if (length(files) == 0) return(NULL)
      if (length(files) > 1) {
        files <- files[order(file.mtime(files), decreasing = TRUE)][1]
      }

      tryCatch({
        df <- readxl::read_excel(files, sheet = 1, col_names = FALSE, .name_repair = "minimal")
        target_row <- which(grepl(target_row,
                                  df[[1]], ignore.case = TRUE))
        if (length(target_row) == 0) return(NULL)

        value <- as.numeric(df[[2]][target_row[1]])

        data.frame(
          period = as.Date(sprintf("%d-%02d-01", year, month)),
          # year = year,
          # month = month,
          value = value#,
          # source_file = files
        )
      }, error = \(e) {
        warning(sprintf("Error reading %s: %s", files, e$message))
        NULL
      })
    }) |> Filter(Negate(is.null), x = _) |> do.call(rbind, args = _)
  }) |> Filter(Negate(is.null), x = _) |> do.call(rbind, args = _)

  results[order(results$period), ]
}




#' Extract single sseries from existing MIN-1 files
#'
#' This currently extracts four specific cells. The whole thing depends on the year-month folder
#' structure, so this is a one off for these particular files. just saving
#' it to the package to be more organized.
#'
#' @param base_path where the files are hidden
#' @param target_row text expected in first column or target row
#'
#' @returns dataframe with period and value columns, period being 2025M12 format
#' @export
#'
extract_min1_series <- function(base_path, target_row = "8. BREZPOSELNI NA KONCU MESECA") {
  month_map <- c(
    "januar" = 1, "februar" = 2, "marec" = 3, "april" = 4,
    "maj" = 5, "junij" = 6, "julij" = 7, "avgust" = 8,
    "september" = 9, "oktober" = 10, "november" = 11, "december" = 12
  )

  year_dirs <- list.dirs(base_path, recursive = FALSE, full.names = TRUE)
  year_dirs <- year_dirs[grepl("\\d{4}$", basename(year_dirs))]

  results <- lapply(year_dirs, \(year_dir) {
    year <- as.integer(basename(year_dir))
    month_dirs <- list.dirs(year_dir, recursive = FALSE, full.names = TRUE)

    lapply(month_dirs, \(month_dir) {
      folder_name <- tolower(basename(month_dir))
      month_match <- month_map[names(month_map)[
        vapply(names(month_map), \(m) grepl(m, folder_name), logical(1))
      ]]
      if (length(month_match) == 0) return(NULL)
      month <- month_match[1]

      files <- list.files(month_dir, pattern = "MIN-1",
                          full.names = TRUE, ignore.case = TRUE)
      files <- files[grepl("\\.(xlsx?|xls)$", files, ignore.case = TRUE)]

      if (length(files) == 0) return(NULL)
      if (length(files) > 1) {
        files <- files[order(file.mtime(files), decreasing = TRUE)][1]
      }

      tryCatch({
        df <- readxl::read_excel(files, sheet = 1, col_names = FALSE, .name_repair = "minimal")
        target_row <- which(grepl(target_row,
                                  df[[1]], ignore.case = TRUE))
        if (length(target_row) == 0) return(NULL)

        value1 <- as.numeric(df[[2]][target_row[1]])
        value2 <- as.numeric(df[[3]][target_row[1]])
        value3 <- as.numeric(df[[2]][target_row[1] + 4])
        value4 <- as.numeric(df[[2]][target_row[1] + 7])

        data.frame(
          period = as.Date(sprintf("%d-%02d-01", year, month)),
          # year = year,
          # month = month,
          bp = value1,
          zen = value2,
          stari = value3,
          dtbp = value4
          # source_file = files
        )
      }, error = \(e) {
        warning(sprintf("Error reading %s: %s", files, e$message))
        NULL
      })
    }) |> Filter(Negate(is.null), x = _) |> do.call(rbind, args = _)
  }) |> Filter(Negate(is.null), x = _) |> do.call(rbind, args = _)

  results[order(results$period), ]
}



#' Upsert data from MIN6 to umar data
#'
#' checks most recent period in the database and then if there is new data
#' available in the data folder. if su updates the umar-data table.
#'
#' @param con database connection
#'
#' @returns invisibly the updated table
#' @export
#'
update_min6_series <- function(con) {
  base_path <- "O:/Avtomatizacija/umar-automation-scripts/data/brezposelni/MIN tabele"
  data_file_path <- "O:/Avtomatizacija/umar-data/DR/umar_serije_podatki_DR.xlsx"

  series_code <- "UMAR-ZRSZ--DR010--2--S--S--M"

  # Get last period from database
  vintage_id <- UMARaccessR::sql_get_vintage_from_series_code(con, series_code)
  last_db_period <- UMARaccessR::sql_get_last_period_from_vintage(con, vintage_id)
  last_db_date <- as.Date(paste0(
    substr(last_db_period, 1, 4), "-",
    substr(last_db_period, 6, 7), "-01"
  ))

  # Calculate next expected period
  next_date <- seq(last_db_date, by = "month", length.out = 2)[2]
  next_year <- as.integer(format(next_date, "%Y"))
  next_month <- as.integer(format(next_date, "%m"))

  month_names <- c("Januar", "Februar", "Marec", "April", "Maj", "Junij",
                   "Julij", "Avgust", "September", "Oktober", "November", "December")

  expected_folder <- file.path(base_path, next_year,
                               sprintf("%s %d", month_names[next_month], next_year))

  if (!dir.exists(expected_folder)) {
    message("No new data available. Next expected: ", format(next_date, "%Y/%m/01"))
    return(invisible(NULL))
  }

  files <- list.files(expected_folder, pattern = "MIN-6",
                      full.names = TRUE, ignore.case = TRUE)
  files <- files[grepl("\\.(xlsx?|xls)$", files, ignore.case = TRUE)]

  if (length(files) == 0) {
    message("No new data available. Next expected: ", format(next_date, "%Y/%m/01"))
    return(invisible(NULL))
  }

  if (length(files) > 1) {
    files <- files[order(file.mtime(files), decreasing = TRUE)][1]
  }

  # Extract value
  df <- readxl::read_excel(files, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  target_row <- which(grepl("2\\. ISKALEC PONOVNE ZAPOSLITVE", df[[1]], ignore.case = TRUE))

  if (length(target_row) == 0) {
    stop("Target row not found in file: ", files)
  }

  value <- as.numeric(df[[2]][target_row[1]])

  # Create new row
  new_data <- data.frame(period = next_date)
  new_data[[series_code]] <- value

  # Read existing Excel file
  existing <- readxl::read_excel(data_file_path, sheet = "M", .name_repair = "minimal")

  updated <- existing |>
    dplyr::rows_upsert(new_data, by = "period")

  # Write back
  openxlsx2::write_xlsx(updated, data_file_path, sheet = "M")

  message("Added period ", format(next_date, "%Y/%m/01"), " with value ", value)
  invisible(updated)
}



#' Upsert data from MIN1 to umar data
#'
#' checks most recent period in the database and then if there is new data
#' available in the data folder. if so updates the umar-data table.
#'
#' @param con database connection
#'
#' @returns invisibly the updated table
#' @export
#'

update_min1_series <- function(con) {
  base_path <- "O:/Avtomatizacija/umar-automation-scripts/data/brezposelni/MIN tabele"
  data_file_path <- "O:/Avtomatizacija/umar-data/DR/umar_serije_podatki_DR.xlsx"

  series_codes <- c("UMAR-ZRSZ--DR011--8--S--S--M",
                    "UMAR-ZRSZ--DR011--8--F--S--M",
                    "UMAR-ZRSZ--DR011--850P--S--S--M",
                    "UMAR-ZRSZ--DR011--DBP--S--S--M")


  # Get last period from database
  vintage_id <- UMARaccessR::sql_get_vintage_from_series_code(con, series_codes[1])
  last_db_period <- UMARaccessR::sql_get_last_period_from_vintage(con, vintage_id)
  last_db_date <- as.Date(paste0(
    substr(last_db_period, 1, 4), "-",
    substr(last_db_period, 6, 7), "-01"
  ))

  # Calculate next expected period
  next_date <- seq(last_db_date, by = "month", length.out = 2)[2]
  next_year <- as.integer(format(next_date, "%Y"))
  next_month <- as.integer(format(next_date, "%m"))

  month_names <- c("Januar", "Februar", "Marec", "April", "Maj", "Junij",
                   "Julij", "Avgust", "September", "Oktober", "November", "December")

  expected_folder <- file.path(base_path, next_year,
                               sprintf("%s %d", month_names[next_month], next_year))

  if (!dir.exists(expected_folder)) {
    message("No new data available. Next expected: ", format(next_date, "%Y/%m/01"))
    return(invisible(NULL))
  }

  files <- list.files(expected_folder, pattern = "MIN-1",
                      full.names = TRUE, ignore.case = TRUE)
  files <- files[grepl("\\.(xlsx?|xls)$", files, ignore.case = TRUE)]

  if (length(files) == 0) {
    message("No new data available. Next expected: ", format(next_date, "%Y/%m/01"))
    return(invisible(NULL))
  }

  if (length(files) > 1) {
    files <- files[order(file.mtime(files), decreasing = TRUE)][1]
  }

  # Extract value
  df <- readxl::read_excel(files, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  target_row <- which(grepl("8. BREZPOSELNI NA KONCU MESECA",
                            df[[1]], ignore.case = TRUE))
  if (length(target_row) == 0) {
    message("8. BREZPOSELNI NA KONCU MESECA not found in MIN 1")
    return(invisible(NULL))
  }

  value1 <- as.numeric(df[[2]][target_row[1]])
  value2 <- as.numeric(df[[3]][target_row[1]])
  value3 <- as.numeric(df[[2]][target_row[1] + 4])
  value4 <- as.numeric(df[[2]][target_row[1] + 7])

  new_data <- data.frame(
    period = next_date)
  new_data[[series_codes[1]]] <- value1
  new_data[[series_codes[2]]] <- value2
  new_data[[series_codes[3]]] <- value3
  new_data[[series_codes[4]]] <- value4



  # Read existing Excel file
  existing <- readxl::read_excel(data_file_path, sheet = "M", .name_repair = "minimal")

  updated <- existing |>
    dplyr::rows_upsert(new_data, by = "period")

  # Write back
  openxlsx2::write_xlsx(updated, data_file_path, sheet = "M")

  message("Added period ", format(next_date, "%Y/%m/01"))
  invisible(updated)
}
