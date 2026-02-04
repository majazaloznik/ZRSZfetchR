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
          period = sprintf("%dM%02d", year, month),
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
