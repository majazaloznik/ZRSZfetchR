dittodb::with_mock_db({
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5432,
                        user = "postgres",
                        password = Sys.getenv("PG_PG_PSW"))
  DBI::dbExecute(con, "set search_path to test_platform")

  test_that("bo script works", {
    x <- zrsz_bo_script(con)
    expect_equal(x$count, 1)
  })
})



