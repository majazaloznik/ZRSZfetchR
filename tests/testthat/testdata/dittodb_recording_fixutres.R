library(dittodb)

start_db_capturing()
con <- dbConnect(RPostgres::Postgres(),
                 dbname = "sandbox",
                 host = "localhost",
                 port = 5432,
                 user = "mzaloznik",
                 password = Sys.getenv("PG_local_MAJA_PSW"))
dbExecute(con, "set search_path to test_platform")
on.exit(dbDisconnect)
dplyr::tbl(con, "source") |>
  dplyr::summarise(max = max(id, na.rm = TRUE)) |>
  dplyr::pull()
stop_db_capturing()
