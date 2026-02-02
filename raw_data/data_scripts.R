meta <- data.frame(
  code = "BO",
  name = "Gibanje registrirane brezposelnosti po mesecih",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/Mesecno_gibanje_BO_1992-",
  extension = ".xlsx",
  dimensions = NA
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
  extension = ".xls",
  dimensions = NA
))


# Add the new BO izobrazba table
meta <- rbind(meta, data.frame(
  code = "BO_SLO_izob",
  name = "Gibanje registrirane brezposelnosti po mesecih - izobrazba",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_izob_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/BO_SLO_izob_2005-",
  extension = ".xls",
  dimensions = "izobrazba"
))

# Add the new BO starost table
meta <- rbind(meta, data.frame(
  code = "BO_SLO_starost",
  name = "Gibanje registrirane brezposelnosti po mesecih - starost",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_starost_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/BO_SLO_starost_2005-",
  extension = ".xls",
  dimensions = "starost"
))

# Add the new BO trajanje table
meta <- rbind(meta, data.frame(
  code = "BO_SLO_trajanje",
  name = "Gibanje registrirane brezposelnosti po mesecih - trajanje",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_trajanje_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/BO_SLO_trajanje_2005-",
  extension = ".xls",
  dimensions = "trajanje"
))

# Add the new BO trajanje table
meta <- rbind(meta, data.frame(
  code = "BO_SLO_spol",
  name = "Gibanje registrirane brezposelnosti po mesecih - spol",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_bo_spol_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Registrirana_brezposelnost/BO_SLO_spol_2005-",
  extension = ".xls",
  dimensions = "spol"
))

# Add the new DN table
meta <- rbind(meta, data.frame(
  code = "DN",
  name = "Število prejemnikov denarnega nadomestila",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_dn_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Pravica_iz_zavarovanja/Prejemniki_DN_2007-",
  extension = ".xls",
  dimensions = NA
))

# Add the new Tokovi table
meta <- rbind(meta, data.frame(
  code = "TOK",
  name = "Novoprijavljene in odjavljene brezposelne osebe",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/",
  notes = NA,
  category = "Brezposelnost",
  excelling_function = "zrsz_tok_excel_parser",
  partial_file_url = "https://www.ess.gov.si/fileadmin/user_upload/Trg_dela/Dokumenti_TD/Trg_dela_v_stevilkah/Prijavljeni_in_odjavljeni/Prijave_odjave_2005-",
  extension = ".xls",
  dimensions = "razlog"
))


dimension_levels <- data.frame(
  table_code = character(),
  dimension = character(),
  level_value = character(),
  level_text = character(),
  stringsAsFactors = FALSE
)

# BO_SLO_izob dimensions
dimension_levels <- rbind(dimension_levels, data.frame(
  table_code = "BO_SLO_izob",
  dimension = "izobrazba",
  level_value = c("I1", "I2", "I3", "I4"),
  level_text = c(
    "OŠ ali manj",
    "nižje, srednje poklicno izob.",
    "srednje tehniško, strokovno, splošno izob.",
    "visokošolsko izob. prve, druge, tretje stopnje"
  ),
  stringsAsFactors = FALSE
))

# BO_SLO_starost dimensions
dimension_levels <- rbind(dimension_levels, data.frame(
  table_code = "BO_SLO_starost",
  dimension = "starost",
  level_value = c("S1", "S2", "S3", "S4", "S5", "S6"),
  level_text = c("15-24 let",
                 "25-29 let",
                 "30-39 let",
                 "40-49 let",
                 "50-54 let",
                 "55 let ali več"),
  stringsAsFactors = FALSE
))

# BO_SLO_starost dimensions
dimension_levels <- rbind(dimension_levels, data.frame(
  table_code = "BO_SLO_trajanje",
  dimension = "trajanje",
  level_value = c("T1", "T2", "T3", "T4", "T5", "T6", "T7"),
  level_text = c("Do 2 mesecev",
                 "3 do 5 mesecev",
                 "6 do 8 mesecev",
                 "9 do 11 mesecev",
                 "12 do 23 mesecev",
                 "24 do 35 mesecev",
                 "36 mesecev ali več"),
  stringsAsFactors = FALSE
))

# BO_SLO_spol dimensions
dimension_levels <- rbind(dimension_levels, data.frame(
  table_code = "BO_SLO_spol",
  dimension = "spol",
  level_value = c("M", "F"),
  level_text = c("moški",
                 "ženske"),
  stringsAsFactors = FALSE
))

# TOK dimensions
dimension_levels <- rbind(dimension_levels, data.frame(
  table_code = "TOK",
  dimension = "razlog",
  level_value = c("pTot", "pPZ", "pPD", "pDČ", "pOR",
                  "oTot", "oV", "oPZ", "oPD", "oOS", "oNZ", "oDE", "oZN"),
  level_text = c("Novoprijavljene osebe",
                 "Prijava - iskalci prve zaposlitve",
                 "Prijava - presežni delavci in stečajniki",
                 "Prijava - iztek zaposlitve za določen čas",
                 "Prijava - ostali razlogi",
                 "Odjavljene osebe",
                 "Odjava - Vključeni v zaposlitev",
                 "Odjava - prva zaposlitev",
                 "Odjava - presežni delavci in stečajniki",
                 "Odjava - ostale vključitve",
                 "Odjava - Odjave iz razlogov, ki ne pomenijo zaposlitve",
                 "Odjava - Prijava v druge evidence",
                 "Odjava - prijava v evidenco začasno nezaposljivih"  ),
  stringsAsFactors = FALSE
))



usethis::use_data(meta, dimension_levels,
                  internal = TRUE,
                  overwrite = TRUE)
