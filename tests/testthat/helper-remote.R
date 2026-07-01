# Shared skip helper for integration tests that need network access and
# Google Drive authentication plus remote electoral/shapefile data.
# These cannot run in CI or offline, so they skip cleanly instead of failing.
skip_if_no_drive <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    testthat::skip("googledrive not installed")
  }
  if (!isTRUE(googledrive::drive_has_token())) {
    testthat::skip("Google Drive not authenticated (googledrive::drive_auth())")
  }
}
