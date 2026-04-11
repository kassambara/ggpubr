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
  stale_cutoff_mins <- 10

  if (package == "all") {
    # Find all lock directories
    lock_dirs <- list.dirs(lib, recursive = FALSE, full.names = TRUE)
    lock_dirs <- lock_dirs[grepl("^00LOCK-", basename(lock_dirs))]
  } else {
    lock_dirs <- file.path(lib, paste0("00LOCK-", package))
    lock_dirs <- lock_dirs[dir.exists(lock_dirs)]
  }


  if (length(lock_dirs) == 0) {
    rlang::inform("No lock files found.")
    return(invisible(FALSE))
  }

  # In non-interactive all-lock cleanup, skip very recent locks
  # to reduce risk of removing active package installation locks.
  if (package == "all" && !ask) {
    lock_info <- file.info(lock_dirs)
    lock_age_mins <- as.numeric(difftime(Sys.time(), lock_info$mtime, units = "mins"))
    recent_locks <- is.na(lock_age_mins) | lock_age_mins < stale_cutoff_mins
    if (any(recent_locks)) {
      rlang::warn(
        c(
          paste0(
            "Skipping lock directories modified within the last ", stale_cutoff_mins,
            " minutes to avoid removing active installation locks."
          ),
          "i" = "This applies to `clean_lock_files(package = \"all\", ask = FALSE)`."
        ),
        call = rlang::caller_env()
      )
      lock_dirs <- lock_dirs[!recent_locks]
    }
    if (length(lock_dirs) == 0) {
      rlang::inform("No stale lock files found for non-interactive all-lock cleanup.")
      return(invisible(FALSE))
    }
  }


  rlang::inform(c("Found lock directories:", paste0("  ", lock_dirs)))

  if (ask) {
    response <- readline("Remove these directories? (y/n): ")
    if (!tolower(response) %in% c("y", "yes")) {
      rlang::inform("Aborted.")
      return(invisible(FALSE))
    }
  }

  # Remove lock directories
  success <- TRUE
  for (d in lock_dirs) {
    tryCatch(
      {
        unlink(d, recursive = TRUE, force = FALSE)
        if (dir.exists(d)) {
          # unlink doesn't always error on failure, check if still exists
          rlang::warn(
            c(
              paste0("Failed to remove ", d, ": directory still exists."),
              "i" = "You may need to remove it manually or check permissions."
            ),
            call = rlang::caller_env()
          )
          success <- FALSE
        } else {
          rlang::inform(paste0("Removed: ", d))
        }
      },
      error = function(e) {
        rlang::warn(
          c(
            paste0("Failed to remove ", d, ": ", e$message),
            "i" = paste0(
              "If this is a stale lock from a stopped install, remove it manually ",
              "after confirming no package installation is running: ",
              "'rm -rf \"", d, "\"'"
            )
          ),
          call = rlang::caller_env()
        )
        success <<- FALSE
      }
    )
  }

  if (success) {
    rlang::inform(c(
      "Lock files cleaned up successfully.",
      "i" = "You can now reinstall the package."
    ))
  }

  invisible(success)
}
