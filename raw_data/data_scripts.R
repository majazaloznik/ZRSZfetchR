meta <- data.frame(
  code = "BO",
  name = "Gibanje registrirane brezposelnosti po mesecih",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/Mesecno_gibanje_BO_1992-",
  extension = ".xlsx"
)

# Add the new BO OS table
meta <- rbind(meta, data.frame(
  code = "BO_OS",
  name = "Stopnja registrirane brezposelnosti po občinah in statističnih regijah",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_os_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Stopnje/Stopnja_BO_OS_2005-",
  extension = ".xls"
))

usethis::use_data(meta,
                  internal = TRUE,
                  overwrite = TRUE)
