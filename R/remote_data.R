#' Pilot function to fetch data from Google Drive
#'
#' @description
#' This is a pilot function to test remote data access for the 'ags' (Aguascalientes) entity.
#' It handles authentication and downloads the requested RDA file to a local cache.
#'
#' @param eleccion Character. The election identifier (e.g., "pm_21").
#' @param entidad Character. The entity abbreviation. Currently only "ags" is supported for remote pilot.
#' @param subfolder Character. The subfolder within the drive (e.g., "electoral", "shp").
#'
#' @return Character string. The path to the downloaded local file.
#' @keywords internal
drive_fetch_pilot <- function(eleccion, entidad, subfolder = "electoral") {
  if (entidad != "ags") {
    stop("Remote data pilot is currently only enabled for entity 'ags' (Aguascalientes).")
  }

  # Ensure googledrive is authenticated
  # In a production environment, we might want to manage this more robustly
  if (!googledrive::drive_has_token()) {
    message("Authenticating with Google Drive for remote data access...")
    googledrive::drive_auth()
  }

  # Define the remote path (this will eventually be driven by a centralized config)
  # For now, we assume a specific structure on the Drive
  remote_filename <- paste0(eleccion, ".rda")
  
  # Temporary local path for the pilot
  local_dir <- file.path(tempdir(), "aelectoral2_cache", subfolder, entidad)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  local_path <- file.path(local_dir, remote_filename)

  # Check if we already have it in the temp cache for this session
  if (file.exists(local_path)) {
    return(local_path)
  }

  # Search for the file on Drive
  # Note: This requires the user to have access to the file/folder
  # We should eventually use a specific Folder ID or Shared Drive
  message(glue::glue("Fetching {remote_filename} for {entidad} from Google Drive..."))
  
  # Search by name (this is a pilot, so we keep it simple)
  # In production, we'd use folder IDs to avoid name collisions
  drive_file <- googledrive::drive_find(
    pattern = remote_filename,
    type = "file",
    n_max = 1
  )

  if (nrow(drive_file) == 0) {
    stop(glue::glue("File {remote_filename} not found on Google Drive."))
  }

  # Download the file
  googledrive::drive_download(
    file = drive_file,
    path = local_path,
    overwrite = TRUE
  )

  return(local_path)
}
