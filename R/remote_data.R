.onLoad <- function(libname, pkgname) {
  op <- options()
  op.aelectoral2 <- list(
    aelectoral2.drive_root = "aelectoral_data",
    aelectoral2.cache_dir = rappdirs::user_cache_dir("aelectoral2")
  )
  toset <- !(names(op.aelectoral2) %in% names(op))
  if (any(toset)) options(op.aelectoral2[toset])
  
  invisible()
}

#' Get the local cache directory for aelectoral2
#'
#' @return Character string. The path to the local cache directory.
#' @keywords internal
get_aelectoral_cache <- function() {
  cache_dir <- getOption("aelectoral2.cache_dir", 
                         rappdirs::user_cache_dir("aelectoral2"))
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  return(cache_dir)
}

#' Fetch data from Google Drive
#'
#' @description
#' Generalized function to fetch data from the company Google Drive.
#' It handles hierarchical navigation and persistent local caching.
#'
#' @param filename Character. The filename to download (e.g., "pm_21.rda").
#' @param entidad Character. The entity abbreviation (e.g., "ags", "mex").
#' @param subfolder Character. The subfolder within the drive (e.g., "electoral", "shp/secc_21").
#' @param force Logical. If TRUE, forces re-download even if the file exists in cache.
#'
#' @return Character string. The path to the downloaded local file.
#' @export
fetch_remote_data <- function(filename, entidad, subfolder = "electoral", force = FALSE) {
  
  # Ensure googledrive is authenticated
  if (!googledrive::drive_has_token()) {
    message("Authenticating with Google Drive for remote data access...")
    googledrive::drive_auth()
  }

  # Local cache path
  local_dir <- file.path(get_aelectoral_cache(), subfolder, entidad)
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  local_path <- file.path(local_dir, filename)

  if (file.exists(local_path) && !force) {
    return(local_path)
  }

  root_name <- getOption("aelectoral2.drive_root", "aelectoral_data")
  message(glue::glue("Locating '{filename}' for '{entidad}' in Google Drive root '{root_name}'..."))

  # 1. Find the root data folder
  current_parent <- googledrive::drive_find(
    pattern = root_name,
    type = "folder",
    corpus = "allDrives",
    n_max = 1
  )
  
  if (nrow(current_parent) == 0) {
    stop(glue::glue("Root folder '{root_name}' not found in Google Drive."))
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

  # 3. Find the entity folder OR find file directly (flexible structure)
  entidad_folder <- googledrive::drive_find(
    q = sprintf("'%s' in parents and name = '%s' and mimeType = 'application/vnd.google-apps.folder'", 
                current_parent$id, entidad),
    corpus = "allDrives",
    n_max = 1
  )

  if (nrow(entidad_folder) > 0) {
    target_parent_id <- entidad_folder$id
    target_parent_name <- entidad_folder$name
  } else {
    target_parent_id <- current_parent$id
    target_parent_name <- current_parent$name
  }

  # 4. Find the specific file
  drive_file <- googledrive::drive_find(
    q = sprintf("'%s' in parents and name = '%s'", 
                target_parent_id, filename),
    corpus = "allDrives",
    n_max = 1
  )

  if (nrow(drive_file) == 0) {
    stop(glue::glue("File '{filename}' not found in Google Drive folder '{target_parent_name}'."))
  }

  # Download
  googledrive::drive_download(
    file = drive_file,
    path = local_path,
    overwrite = TRUE
  )

  return(local_path)
}
