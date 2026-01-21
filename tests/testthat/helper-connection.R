library(testthat)
library(dittodb)

make_test_connection <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "platform",
                        host = "localhost",
                        port = 5433,
                        user = "postgres",
                        password = Sys.getenv("PG_local_15_PG_PSW"),
                        client_encoding = "utf8")

}

make_test2_connection <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        dbname = "production_backup",
                        host = "192.168.38.21",
                        port = 5432,
                        user = "postgres",
                        password = Sys.getenv("PG_PG_PSW"),
                        client_encoding = "utf8")

}
