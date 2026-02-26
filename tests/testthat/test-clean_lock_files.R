test_that("clean_lock_files skips recent locks in non-interactive all mode", {
  lib <- file.path(tempdir(), "ggpubr-clean-lock-test")
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)

  old_lock <- file.path(lib, "00LOCK-oldpkg")
  recent_lock <- file.path(lib, "00LOCK-recentpkg")
  dir.create(old_lock, showWarnings = FALSE)
  dir.create(recent_lock, showWarnings = FALSE)

  # Emulate stale and active-ish lock timestamps.
  Sys.setFileTime(old_lock, Sys.time() - 20 * 60)
  Sys.setFileTime(recent_lock, Sys.time() - 60)

  expect_warning(
    suppressMessages(clean_lock_files(package = "all", lib = lib, ask = FALSE)),
    "Skipping lock directories modified within the last"
  )

  expect_false(dir.exists(old_lock))
  expect_true(dir.exists(recent_lock))

  unlink(lib, recursive = TRUE, force = TRUE)
})
