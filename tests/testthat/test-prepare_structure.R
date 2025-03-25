
dittodb::with_mock_db({
  con <- make_test_connection()
  test_that("prepare functions work", {
    meta <- ZRSZfetchR:::meta[1,]
    # x <- prepare_source_table(con)
    # expect_equal(dim(x), c(1,4))
    x <- prepare_table_table(meta, FALSE, con, schema = "test_platform")
    expect_equal(dim(x), c(1,6))
    x <- prepare_category_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,3))
    x <- prepare_category_relationship_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,3))
    x <- prepare_category_table_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,3))
    x <- prepare_table_dimensions_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,3))
    x <- prepare_dimension_levels_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,3))
    x <- prepare_series_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,5))
    x <- prepare_series_levels_table(meta, con, schema = "test_platform")
    expect_equal(dim(x), c(1,3))

  })
})



