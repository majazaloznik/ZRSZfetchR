source("renv/activate.R")
options(renv.config.auto.snapshot = TRUE)
Sys.setenv(http_proxy="http://proxy.gov.si:80")
Sys.setenv(http_proxy_user="http://proxy.gov.si:80")
Sys.setenv(https_proxy="http://proxy.gov.si:80")
Sys.setenv(https_proxy_user="http://proxy.gov.si:80")

cat("UMAR proxy is set!\n")
options(continue = " ")
if (interactive()) {
  suppressMessages(require(devtools))
}
Sys.setenv(PATH = paste("C:\\Program Files\\qpdf 11.4.0\\bin", Sys.getenv("PATH"), sep=";"))
