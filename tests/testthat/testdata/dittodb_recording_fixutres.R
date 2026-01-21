# This code was run once and is here for archival purposes.
#
source("tests/testthat/helper-connection.R")

# Enable verbose debugging to see query hashes
options(dittodb.debug = FALSE)


# start_db_capturing()
# con <- make_test_connection()
# DBI::dbExecute(con, "set search_path to test_platform")
# on.exit(dbDisconnect)
# dplyr::tbl(con, "source") |>
#   dplyr::summarise(max = max(id, na.rm = TRUE)) |>
#   dplyr::pull()
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_table_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_category_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_category_relationship_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_category_table_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_table_dimensions_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_dimension_levels_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_series_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# start_db_capturing()
# on.exit(dbDisconnect)
# con <- make_test_connection()
# meta <- ZRSZfetchR:::meta[1,]
# x <- prepare_series_levels_table(meta, con, schema = "test_platform")
# stop_db_capturing()
#
start_db_capturing()
on.exit(dbDisconnect)
con <- make_test_connection()
meta <- ZRSZfetchR:::meta[2,]
x <- ZRSZ_import_structure(meta, con, schema = "test_platform")
stop_db_capturing()
#
# # Set the TESTTHAT environment variable to ensure fixed timestamp is used
# Sys.setenv(TESTTHAT = "true")
#
# start_db_capturing()
# con <- make_test_connection()
# on.exit(dbDisconnect)
# meta <- ZRSZfetchR:::meta[2,]
# x <- ZRSZ_import_data_points(meta, con, schema = "test_platform")
# stop_db_capturing()
#
# # Reset the environment variable
# Sys.setenv(TESTTHAT = "")

start_db_capturing()
on.exit(dbDisconnect)
con <- make_test_connection()
meta <- ZRSZfetchR:::meta[3,]
x <- ZRSZ_import_structure(meta, con, schema = "test_platform")
stop_db_capturing()

# UMARimportR::delete_vintage(con,116213, "test_platform")
UMARimportR::vintage_cleanup(con, "test_platform")
