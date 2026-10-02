# Directory links for tests. On Windows a junction is used: R's unlink()
# removes junctions, while removing a directory symbolic link there fails
# with "cannot delete reparse point" and leaves it in tempdir().
bb_dir_link <- function(target, link) {
  made <- if (identical(.Platform$OS.type, "windows")) {
    suppressWarnings(Sys.junction(target, link))
  } else {
    suppressWarnings(file.symlink(target, link))
  }
  isTRUE(made)
}
