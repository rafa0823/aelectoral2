# Configuration for data-raw paths
# This file centralizes all external paths to ensure portability and ISO 27001 compliance.
# Replace these with environment variables or consistent mount points in production.

PATHS <- list(
  google_drive = Sys.getenv("AELECTORAL_GOOGLE_DRIVE", "~/Google Drive"),
  dropbox = Sys.getenv("AELECTORAL_DROPBOX", "~/Dropbox (Selva)"),
  local_resources = Sys.getenv("AELECTORAL_RESOURCES", "~/Documents/Git/aelectoral2/external-resources")
)

get_path <- function(source = c("google", "dropbox", "local"), subpath = "") {
  source <- match.arg(source)
  base <- switch(source,
    google = PATHS$google_drive,
    dropbox = PATHS$dropbox,
    local = PATHS$local_resources
  )
  file.path(base, subpath)
}
