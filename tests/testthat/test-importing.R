 dittodb::with_mock_db({
  con <- make_test_connection()
  test_that("import strucutre function works", {
    meta <- ZRSZfetchR:::meta[2,]
    x <- ZRSZ_import_structure(meta, con, schema = "test_platform")
    expect_true(is.list(x))
    expect_true(all(names(x) == c("table", "category", "category_relantionship",
                                  "category_table", "table_dimensions",
                                  "dimension_levels", "series", "series_levels")))
  })

  test_that("import data function works", {
    meta <- ZRSZfetchR:::meta[2,]
    options(dittodb.debug= TRUE)
    x <- ZRSZ_import_data_points(meta, con, schema = "test_platform")
    expect_true(is.list(x))
    expect_true(all(names(x) == c( "vintages", "data")))
    expect_true(x$data$datapoints_inserted == 228)
  })
})
