# Package load and attach hooks

.onAttach <- function(libname, pkgname) {
  # Check for stale lock files and warn user
  lib_path <- .libPaths()[1]
  lock_dir <- file.path(lib_path, paste0("00LOCK-", pkgname))


  if (dir.exists(lock_dir)) {
    packageStartupMessage(
      "Note: A stale lock directory was detected: ", lock_dir, "\n",
      "This may cause installation issues. Run ggpubr::clean_lock_files() to remove it."
    )
  }
}

#' Clean up stale package lock files
#'
#' @description Removes stale lock directories that can prevent package installation.
#'
#' Lock files are created during package installation and should be automatically
#' removed when installation completes. If installation is interrupted (e.g., by
#' closing R or a crash), these lock files may remain and block future installations.
#'
#' @param package Character string specifying which package lock to remove.
#'   Default is "ggpubr". Use "all" to remove all lock files.
#' @param lib Library path to check. Default is the first library in .libPaths().
#' @param ask Logical. If TRUE (default), asks for confirmation before removing.
#'
#' @return Invisibly returns TRUE if files were removed, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' # Remove ggpubr lock file
#' clean_lock_files()
#'
#' # Remove all lock files
#' clean_lock_files("all")
#'
#' # Remove without confirmation
#' clean_lock_files(ask = FALSE)
#' }
#'
#' @export
clean_lock_files <- function(package = "ggpubr", lib = .libPaths()[1], ask = TRUE) {
  if (package == "all") {
    # Find all lock directories
    lock_dirs <- list.dirs(lib, recursive = FALSE, full.names = TRUE)
    lock_dirs <- lock_dirs[grepl("^00LOCK-", basename(lock_dirs))]
  } else {
    lock_dirs <- file.path(lib, paste0("00LOCK-", package))
    lock_dirs <- lock_dirs[dir.exists(lock_dirs)]
  }


  if (length(lock_dirs) == 0) {
    message("No lock files found.")
    return(invisible(FALSE))
  }


  message("Found lock directories:")
  for (d in lock_dirs) {
    message("  ", d)
  }

  if (ask) {
    response <- readline("Remove these directories? (y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      message("Aborted.")
      return(invisible(FALSE))
    }
  }

  # Remove lock directories
  success <- TRUE
  for (d in lock_dirs) {
    tryCatch(
      {
        unlink(d, recursive = TRUE, force = TRUE)
        if (dir.exists(d)) {
          # unlink doesn't always error on failure, check if still exists
          warning(
            "Failed to remove ", d, ": directory still exists. ",
            "You may need to remove it manually or check permissions."
          )
          success <- FALSE
        } else {
          message("Removed: ", d)
        }
      },
      error = function(e) {
        warning(
          "Failed to remove ", d, ": ", e$message,
          "\nOn Linux/macOS, you may need: sudo rm -rf '", d, "'"
        )
        success <<- FALSE
      }
    )
  }

  if (success) {
    message("Lock files cleaned up successfully.")
    message("You can now reinstall the package.")
  }

  invisible(success)
}
