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

  if (!googledrive::drive_has_token()) {
    message("Authenticating with Google Drive for remote data access...")
    googledrive::drive_auth()
  }

  remote_filename <- paste0(eleccion, ".rda")
  
  # Local cache path
  local_dir <- file.path(tempdir(), "aelectoral2_cache", subfolder, entidad)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  local_path <- file.path(local_dir, remote_filename)

  if (file.exists(local_path)) {
    return(local_path)
  }

  message(glue::glue("Locating {remote_filename} for {entidad} in Google Drive..."))

  # 1. Find the root data folder
  current_parent <- googledrive::drive_find(
    pattern = "aelectoral_data",
    type = "folder",
    corpus = "allDrives",
    n_max = 1
  )
  
  if (nrow(current_parent) == 0) {
    stop("Root folder 'aelectoral_data' not found in Google Drive (checked all accessible drives).")
  }

  # 2. Navigate nested subfolders (e.g., "shp/secc_21")
  path_parts <- unlist(strsplit(subfolder, "/"))
  for (part in path_parts) {
    parent_name <- current_parent$name
    current_parent <- googledrive::drive_find(
      q = sprintf("'%s' in parents and name = '%s' and mimeType = 'application/vnd.google-apps.folder'", 
                  current_parent$id, part),
      corpus = "allDrives",
      n_max = 1
    )
    if (nrow(current_parent) == 0) {
      stop(glue::glue("Subfolder '{part}' not found inside '{parent_name}' folder."))
    }
  }

  # 3. Find the entity folder (e.g., 'ags') inside last subfolder
  entidad_folder <- googledrive::drive_find(
    q = sprintf("'%s' in parents and name = '%s' and mimeType = 'application/vnd.google-apps.folder'", 
                current_parent$id, entidad),
    corpus = "allDrives",
    n_max = 1
  )

  if (nrow(entidad_folder) == 0) {
    stop(glue::glue("Entity folder '{entidad}' not found inside '{current_parent$name}' folder."))
  }

  # 4. Find the specific file inside the entity folder
  drive_file <- googledrive::drive_find(
    q = sprintf("'%s' in parents and name = '%s'", 
                entidad_folder$id, remote_filename),
    corpus = "allDrives",
    n_max = 1
  )

  if (nrow(drive_file) == 0) {
    stop(glue::glue("File '{remote_filename}' not found in Google Drive folder '{entidad}'."))
  }

  # Download
  googledrive::drive_download(
    file = drive_file,
    path = local_path,
    overwrite = TRUE
  )

  return(local_path)
}
