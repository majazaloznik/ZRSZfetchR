dittodb::with_mock_db({
  con <- make_test_connection()
  test_that("prepare vintage table works", {
    filepath <- test_path("testdata","Stopnja_BO_OS_2005-2025.xls")
    df <- zrsz_bo_os_excel_parser(filepath)
    meta <- ZRSZfetchR:::meta[2,]
    vintages <- prepare_vintage_table(meta, df, con, schema = "test_platform")
    expect_true(is.data.frame(vintages))
    expect_true(all(colnames(vintages) == c("series_id", "published")))
  })
  test_that("get datapoints works", {
    meta <- ZRSZfetchR:::meta[2,]
    df <- get_datapoints(meta)
    expect_true(is.data.frame(df))

  })
  test_that("get datapoints works", {
    meta <- ZRSZfetchR:::meta[1,]
    df <- get_datapoints(meta)
    expect_true(is.data.frame(df))
    expect_true(all(colnames(df) == c("period", "value")))
    expect_true(is.numeric(df$value))
  })

})

