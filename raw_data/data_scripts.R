meta <- data.frame(
  code = "BO",
  name = "Gibanje registrirane brezposelnosti po mesecih",
  url = "https://www.ess.gov.si/partnerji/trg-dela/trg-dela-v-stevilkah/registrirana-brezposelnost/",
  notes = NA,
  category = "Brezposelnost"
)


usethis::use_data(meta,
                  internal = TRUE,
                  overwrite = TRUE)
