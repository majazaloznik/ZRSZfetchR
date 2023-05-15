dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "sandbox",
                        host = "localhost",
                        port = 5432,
                        user = "mzaloznik",
                        password = Sys.getenv("PG_local_MAJA_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("prepare functions work", {
    x <- prepare_source_table(con)
    expect_equal(dim(x), c(1,4))
    x <- prepare_table_table(meta, con)
    expect_equal(dim(x), c(1,5))
    x <- prepare_category_table(meta, con)
    expect_equal(dim(x), c(1,3))
    x <- prepare_category_relationship_table(meta, con)
    expect_equal(dim(x), c(1,3))
    x <- prepare_category_table_table(meta, con)
    expect_equal(dim(x), c(1,3))
  })
})



