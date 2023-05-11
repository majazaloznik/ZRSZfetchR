test_that("BO excelreading works", {
  filepath <- test_path("testdata","Mesecno_gibanje_BO_1992-2023.xlsx")
  df <- zrsz_bo_excel_parser(filepath)
  expect_equal(ncol(df), 2)
  expect_true(any(!is.na(df$value)))
})
