.update_journal_format <- "bigbang-update-journal"
.update_journal_version <- 1L
.update_journal_tombstone_name <- "tombstone.rds"
.update_journal_digest_name <- "tombstone.md5"
.update_journal_runtime <- new.env(parent = emptyenv())
.update_journal_runtime$current <- NULL

.update_journal_path <- function(project_dir) {
  file.path(dirname(project_dir), paste0(".", basename(project_dir),
                                         ".bigbang-update"))
}

.update_journal_sibling_paths <- function(project_dir, name, kind) {
  prefix <- paste0(".", name, ".bigbang-update.", kind, "-")
  siblings <- list.files(dirname(project_dir), full.names = TRUE,
                         all.files = TRUE, no.. = TRUE)
  siblings <- siblings[startsWith(basename(siblings), prefix)]
  suffix <- substring(basename(siblings), nchar(prefix) + 1L)
  siblings <- siblings[nzchar(suffix) & grepl("^[[:alnum:]-]+$", suffix)]
  sort(normalizePath(siblings, winslash = "/", mustWork = FALSE))
}

.update_journal_backup_path <- function(path) file.path(path, "backup")

.update_journal_kind_label <- function(kind) {
  switch(kind,
    armed = .bb_tr("armed"),
    unarmed = .bb_tr("unarmed"),
    discarded = .bb_tr("discarded"),
    .bb_tr("unknown")
  )
}

.update_journal_problem <- function(class, project_dir, path, kind, detail,
                                    has_backup = FALSE, next_step) {
  contents <- if (isTRUE(has_backup)) {
    .bb_trf("It contains a backup at %s.", .update_journal_backup_path(path))
  } else {
    .bb_tr("It does not contain a recognized backup.")
  }
  message <- .bb_trf(
    "Cannot update %s because %s is an %s update-journal folder. %s %s Next step: %s",
    project_dir, path, .update_journal_kind_label(kind), detail, contents,
    next_step
  )
  fields <- list(
    class = class, message = message, path = project_dir,
    journal = path, journal_kind = kind
  )
  if (identical(class, "bigbang_error_update_in_progress")) {
    fields$recover <- TRUE
  }
  do.call(.bigbang_abort, fields)
}

.update_journal_tombstone_path <- function(path) {
  file.path(path, .update_journal_tombstone_name)
}

.update_journal_digest_path <- function(path) {
  file.path(path, .update_journal_digest_name)
}

.update_journal_apart_path <- function(path, name, project_dir = NULL) {
  parent <- if (!is.null(project_dir)) {
    dirname(project_dir)
  } else {
    cursor <- if (dir.exists(path)) path else dirname(path)
    found <- FALSE
    repeat {
      if (grepl(
        paste0("^\\.", .escape_regex_literal(name),
               "\\.bigbang-update(?:\\.|$)"),
        basename(cursor), perl = TRUE
      )) {
        found <- TRUE
        break
      }
      next_cursor <- dirname(cursor)
      if (identical(next_cursor, cursor)) break
      cursor <- next_cursor
    }
    if (found) dirname(cursor) else dirname(path)
  }
  stamp <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
                  Sys.getpid())
  base <- file.path(parent, paste0(
    ".", name, ".bigbang-apartado-", stamp
  ))
  destination <- base
  suffix <- 0L
  while (file.exists(destination) || dir.exists(destination)) {
    suffix <- suffix + 1L
    destination <- paste0(base, "-", suffix)
  }
  destination
}

.apart_update_journal <- function(path, name, project_dir = NULL,
                                  deleted_count = 0L) {
  destination <- .update_journal_apart_path(path, name, project_dir)
  if (!file.rename(path, destination)) {
    stop(.bb_trf("Could not set aside the update-journal entry: %s", path),
         call. = FALSE)
  }
  if (identical(as.integer(deleted_count), 0L)) {
    message(.bb_trf(
      paste0(
        "Set aside unverified update-journal content at %s; no bytes from ",
        "the set-aside entry were deleted and the update continues."
      ),
      destination
    ))
  } else {
    message(.bb_trf(
      paste0(
        "Set aside remaining update-journal content at %s; only verified ",
        "inventory files were deleted before unverified content was preserved ",
        "and the update continues."
      ),
      destination
    ))
  }
  destination
}

.update_lock_apart_path <- function(path, name) {
  parent <- dirname(path)
  stamp <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
                  Sys.getpid())
  base <- file.path(parent, paste0(".", name, ".bigbang-apartado-", stamp))
  destination <- base
  suffix <- 0L
  while (file.exists(destination) || dir.exists(destination)) {
    suffix <- suffix + 1L
    destination <- paste0(base, "-", suffix)
  }
  destination
}

.apart_update_lock <- function(path, name) {
  destination <- .update_lock_apart_path(path, name)
  if (!file.rename(path, destination)) {
    stop(.bb_trf("Could not set aside the update-lock entry: %s", path),
         call. = FALSE)
  }
  message(.bb_trf(
    "Set aside update-lock entry at %s; no bytes from the set-aside entry were deleted and the update continues.",
    destination
  ))
  destination
}

.update_journal_digest_valid <- function(path) {
  digest_path <- .update_journal_digest_path(path)
  tombstone_path <- .update_journal_tombstone_path(path)
  if (!file.exists(digest_path) || dir.exists(digest_path) ||
        .path_is_symlink(digest_path)) return(FALSE)
  recorded <- tryCatch(
    readLines(digest_path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  length(recorded) == 1L &&
    identical(recorded, .file_digest(tombstone_path))
}

.update_journal_temps <- function(path) {
  entries <- list.files(path, all.files = TRUE, no.. = TRUE,
                        include.dirs = TRUE)
  full <- file.path(path, entries)
  entries[!dir.exists(full) & grepl(
    "^\\.(?:tombstone|tombstone\\.md5)-[[:alnum:]]+$",
    basename(entries), perl = TRUE
  )]
}

.update_journal_entries <- function(path) {
  if (!dir.exists(path) || .path_is_symlink(path)) {
    return(structure(character(), bigbang_unreadable = FALSE))
  }
  pending <- list(list(path = path, relative = ""))
  entries <- character()
  unreadable <- FALSE
  while (length(pending) > 0L) {
    current <- pending[[1L]]
    pending <- pending[-1L]
    if (file.access(current$path, 4L) != 0L) {
      unreadable <- TRUE
      next
    }
    children <- tryCatch(
      withCallingHandlers(
        list.files(current$path, all.files = TRUE, no.. = TRUE,
                   include.dirs = TRUE, full.names = TRUE),
        warning = function(e) {
          unreadable <<- TRUE
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        unreadable <<- TRUE
        character()
      }
    )
    if (length(children) == 0L) next
    for (child in children) {
      relative <- if (nzchar(current$relative)) {
        paste(current$relative, basename(child), sep = "/")
      } else {
        basename(child)
      }
      relative <- gsub("\\\\", "/", relative)
      entries <- c(entries, relative)
      if (dir.exists(child) && !.path_is_symlink(child)) {
        pending[[length(pending) + 1L]] <- list(
          path = child, relative = relative
        )
      }
    }
  }
  answer <- sort(unique(entries))
  attr(answer, "bigbang_unreadable") <- unreadable
  answer
}

.journal_entries_unreadable <- function(entries) {
  isTRUE(attr(entries, "bigbang_unreadable", exact = TRUE))
}

.journal_has_link_ancestor <- function(path, relative) {
  parts <- strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) return(FALSE)
  current <- path
  for (part in utils::head(parts, -1L)) {
    current <- file.path(current, part)
    if (.path_is_symlink(current)) return(TRUE)
  }
  FALSE
}

.journal_unreadable_ancestor <- function(path, relative) {
  parts <- strsplit(gsub("\\\\", "/", relative), "/", fixed = TRUE)[[1L]]
  if (length(parts) < 2L) return(FALSE)
  current <- path
  for (part in utils::head(parts, -1L)) {
    current <- file.path(current, part)
    if (file.access(current, 4L) != 0L) return(TRUE)
  }
  FALSE
}

.update_journal_inventory <- function(path) {
  entries <- .update_journal_entries(path)
  full <- file.path(path, entries)
  symlinks <- vapply(full, .path_is_symlink, logical(1L))
  is_dir <- dir.exists(full) & !symlinks
  files <- setdiff(
    entries[!is_dir],
    c(.update_journal_tombstone_name, .update_journal_digest_name)
  )
  if (any(symlinks)) {
    stop(.bb_trf("Could not write the update-journal tombstone: %s", path),
         call. = FALSE)
  }
  hashes <- if (length(files) == 0L) {
    character()
  } else {
    vapply(file.path(path, files), .file_digest, character(1L))
  }
  names(hashes) <- files
  list(
    files = hashes,
    directories = entries[is_dir]
  )
}

.update_journal_tombstone <- function(path, name, marker) {
  manifest_hash <- unname(marker$backup_hashes[[.generation_manifest_name]])
  inventory <- .update_journal_inventory(path)
  owner <- .update_marker_owner(marker)
  tombstone <- c(list(
    format = .update_journal_format,
    version = .update_journal_version,
    kind = "discarded",
    name = name,
    manifest_hash = manifest_hash,
    inventory = inventory$files,
    inventory_directories = inventory$directories,
    created_utc = format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  ), if (is.null(owner)) list() else owner)
  destination <- .update_journal_tombstone_path(path)
  temporary <- tempfile(pattern = ".tombstone-", tmpdir = path)
  digest_temporary <- tempfile(pattern = ".tombstone.md5-", tmpdir = path)
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  on.exit(unlink(digest_temporary, force = TRUE), add = TRUE)
  saveRDS(tombstone, temporary)
  .write_utf8(.file_digest(temporary), digest_temporary)
  if (!file.rename(digest_temporary,
                   .update_journal_digest_path(path))) {
    stop(.bb_trf("Could not write the update-journal tombstone: %s", path),
         call. = FALSE)
  }
  if (!file.rename(temporary, destination)) {
    stop(.bb_trf("Could not write the update-journal tombstone: %s", path),
         call. = FALSE)
  }
  invisible(tombstone)
}

.read_update_tombstone <- function(path, name = NULL) {
  tombstone_path <- .update_journal_tombstone_path(path)
  if (!file.exists(tombstone_path) || dir.exists(tombstone_path) ||
        .path_is_symlink(tombstone_path)) return(NULL)
  tombstone <- tryCatch(readRDS(tombstone_path), error = function(e) NULL)
  valid <- is.list(tombstone) &&
    identical(tombstone$format, .update_journal_format) &&
    identical(tombstone$version, .update_journal_version) &&
    identical(tombstone$kind, "discarded") &&
    is.character(tombstone$name) && length(tombstone$name) == 1L &&
    (is.null(name) || identical(tombstone$name, name)) &&
    is.character(tombstone$manifest_hash) &&
    length(tombstone$manifest_hash) == 1L &&
    grepl("^[0-9a-f]{32}$", tombstone$manifest_hash) &&
    is.character(tombstone$inventory) &&
    (length(tombstone$inventory) == 0L ||
       (!is.null(names(tombstone$inventory)) &&
          !anyDuplicated(names(tombstone$inventory)) &&
          all(vapply(names(tombstone$inventory), .valid_update_relative_path,
                     logical(1L))) &&
          all(grepl("^[0-9a-f]{32}$", unname(tombstone$inventory))))) &&
    is.character(tombstone$inventory_directories) &&
    !anyDuplicated(tombstone$inventory_directories) &&
    all(vapply(tombstone$inventory_directories, .valid_update_relative_path,
               logical(1L)))
  if (isTRUE(valid)) tombstone else NULL
}

.update_tombstone_owner <- function(tombstone) {
  fields <- c("pid", "host", "started_utc", "process_start")
  if (!is.list(tombstone) || !all(fields %in% names(tombstone))) return(NULL)
  if ("process_start_source" %in% names(tombstone)) {
    fields <- c(fields, "process_start_source")
  }
  tombstone[fields]
}

.discard_update_entry <- function(path, root = NULL, relative = NULL,
                                  expected_digest = NULL) {
  if (!is.null(root)) {
    if (.path_is_symlink(root) || .journal_has_link_ancestor(root, relative) ||
          .journal_unreadable_ancestor(root, relative) ||
          .path_is_symlink(path)) {
      return(FALSE)
    }
    if (!is.null(expected_digest)) {
      actual <- .file_digest(path)
      if (is.na(actual) || !identical(actual, expected_digest)) {
        return(FALSE)
      }
      if (.path_is_symlink(root) || .journal_has_link_ancestor(root, relative) ||
            .journal_unreadable_ancestor(root, relative) ||
            .path_is_symlink(path)) {
        return(FALSE)
      }
    }
  }
  status <- unlink(path, recursive = TRUE, force = TRUE)
  !isTRUE(status) && !file.exists(path) && !dir.exists(path)
}

.remove_journal_tombstones <- function(path) {
  unlink(.update_journal_digest_path(path), force = TRUE)
  unlink(.update_journal_tombstone_path(path), force = TRUE)
  invisible(NULL)
}

.update_journal_after_rename <- function(path) {
  invisible(path)
}

.rename_discarded_private <- function(path, name) {
  if (!dir.exists(path) || .path_is_symlink(path)) {
    stop(.bb_trf("Could not set aside the update-journal entry: %s", path),
         call. = FALSE)
  }
  token <- basename(tempfile(
    pattern = paste0(".", name, ".bigbang-update.descartado-en-proceso-"),
    tmpdir = dirname(path)
  ))
  destination <- file.path(dirname(path), token)
  if (!file.rename(path, destination)) {
    stop(.bb_trf("Could not set aside the update-journal entry: %s", path),
         call. = FALSE)
  }
  if (.path_is_symlink(destination) || !dir.exists(destination)) {
    stop(.bb_trf("Could not set aside the update-journal entry: %s", path),
         call. = FALSE)
  }
  .update_journal_after_rename(destination)
  destination
}

.finish_discarded_journal <- function(path, name, tombstone = NULL,
                                      project_dir = NULL,
                                      verify_project = FALSE) {
  tombstone <- .or_null(tombstone, .read_update_tombstone(path, name))
  if (is.null(tombstone)) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", path, path, "unknown",
      .bb_tr("Its tombstone is missing or invalid; nothing was deleted."),
      has_backup = FALSE,
      next_step = .bb_tr("keep the folder intact, preserve its contents, and inspect it before retrying.")
    )
  }
  if (isTRUE(verify_project)) {
    current_manifest <- .file_digest(
      file.path(project_dir, .generation_manifest_name)
    )
    if (!identical(tombstone$name, name) ||
          !identical(tombstone$manifest_hash, current_manifest)) {
      return(invisible(list(apart = .apart_update_journal(path, name))))
    }
  }
  entries <- .update_journal_entries(path)
  if (.journal_entries_unreadable(entries)) {
    return(invisible(list(
      apart = .apart_update_journal(path, name, deleted_count = 0L)
    )))
  }
  full <- file.path(path, entries)
  symlinks <- vapply(full, .path_is_symlink, logical(1L))
  is_dir <- dir.exists(full) & !symlinks
  symlink_entries <- entries[symlinks]
  files <- setdiff(
    entries[!is_dir],
    c(.update_journal_tombstone_name, .update_journal_digest_name)
  )
  expected_files <- tombstone$inventory
  expected_dirs <- tombstone$inventory_directories
  unexpected_files <- files[!files %in% names(expected_files)]
  matching_files <- intersect(files, names(expected_files))
  matching_files <- setdiff(matching_files, symlink_entries)
  matching_files <- matching_files[!vapply(
    matching_files,
    function(entry) .journal_has_link_ancestor(path, entry),
    logical(1L)
  )]
  matching_files <- matching_files[!vapply(
    matching_files,
    function(entry) .journal_unreadable_ancestor(path, entry),
    logical(1L)
  )]
  if (length(matching_files) > 0L) {
    actual <- vapply(file.path(path, matching_files), .file_digest, character(1L))
    verified_files <- matching_files[
      !is.na(actual) & actual == unname(expected_files[matching_files])
    ]
    unexpected_files <- c(
      unexpected_files,
      setdiff(matching_files, verified_files)
    )
    matching_files <- verified_files
  }
  unexpected_dirs <- setdiff(entries[is_dir], expected_dirs)
  unexpected <- unique(c(unexpected_files, unexpected_dirs, symlink_entries))
  path <- .rename_discarded_private(path, name)
  deleted_count <- 0L
  for (entry in matching_files) {
    removed <- .discard_update_entry(
      file.path(path, entry), root = path, relative = entry,
      expected_digest = unname(expected_files[[entry]])
    )
    if (isTRUE(removed)) {
      deleted_count <- deleted_count + 1L
    } else {
      unexpected <- c(unexpected, entry)
    }
  }
  if (deleted_count > 0L) {
    message(.bb_trf(
      "Deleted %d verified files from the update-journal inventory.",
      deleted_count
    ))
  }
  directory_failure <- FALSE
  directories <- sort(setdiff(expected_dirs, .update_journal_tombstone_name),
                      decreasing = TRUE)
  directories <- directories[order(nchar(directories), decreasing = TRUE)]
  for (entry in directories) {
    target <- file.path(path, entry)
    target_entries <- .update_journal_entries(target)
    if (dir.exists(target) && !.path_is_symlink(target) &&
          !.journal_has_link_ancestor(path, entry) &&
          !.journal_unreadable_ancestor(path, entry) &&
          !.journal_entries_unreadable(target_entries) &&
          length(target_entries) == 0L) {
      if (!isTRUE(.discard_update_entry(
        target, root = path, relative = entry
      ))) {
        directory_failure <- TRUE
      }
    }
  }
  remaining <- setdiff(
    .update_journal_entries(path),
    c(.update_journal_tombstone_name, .update_journal_digest_name)
  )
  remaining_entries <- .update_journal_entries(path)
  if (directory_failure && !.journal_entries_unreadable(remaining_entries)) {
    stop(.bb_trf("Could not remove completely: %s", path), call. = FALSE)
  }
  if (.journal_entries_unreadable(remaining_entries) ||
    length(setdiff(
      remaining_entries,
      c(.update_journal_tombstone_name, .update_journal_digest_name)
    )) > 0L) {
    return(invisible(list(
      apart = .apart_update_journal(
        path, name, deleted_count = deleted_count
      ),
      preserved = sort(unique(c(unexpected, remaining)))
    )))
  }
  .remove_journal_tombstones(path)
  unlink(path, recursive = TRUE, force = TRUE)
  if (dir.exists(path) || file.exists(path)) {
    stop(.bb_trf("Could not remove completely: %s", path), call. = FALSE)
  }
  invisible(NULL)
}

.discard_renamed_journal <- function(path, project_dir, name, marker) {
  .update_journal_tombstone(path, name, marker)
  stamp <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
                  Sys.getpid())
  discarded <- file.path(
    dirname(path), paste0(".", name, ".bigbang-update.descartado-", stamp)
  )
  suffix <- 0L
  while (file.exists(discarded) || dir.exists(discarded)) {
    suffix <- suffix + 1L
    discarded <- paste0(
      file.path(dirname(path), paste0(".", name,
                                      ".bigbang-update.descartado-", stamp)),
      "-", suffix
    )
  }
  if (!file.rename(path, discarded)) {
    stop(.bb_trf("Could not move the update journal to its discarded state: %s",
                 path), call. = FALSE)
  }
  .update_journal_after_rename(discarded)
  .finish_discarded_journal(discarded, name)
  invisible(NULL)
}

.discard_tombstoned_journal <- function(path, project_dir, name, tombstone) {
  stamp <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
                  Sys.getpid())
  discarded <- file.path(
    dirname(path), paste0(".", name, ".bigbang-update.descartado-", stamp)
  )
  suffix <- 0L
  while (file.exists(discarded) || dir.exists(discarded)) {
    suffix <- suffix + 1L
    discarded <- paste0(
      file.path(dirname(path), paste0(".", name,
                                      ".bigbang-update.descartado-", stamp)),
      "-", suffix
    )
  }
  if (!file.rename(path, discarded)) {
    stop(.bb_trf("Could not move the update journal to its discarded state: %s",
                 path), call. = FALSE)
  }
  .update_journal_after_rename(discarded)
  .finish_discarded_journal(discarded, name, tombstone)
  invisible(NULL)
}

.reconcile_update_siblings <- function(project_dir, name, dry_run = FALSE,
                                       recover = FALSE) {
  plan_environment <- new.env(parent = emptyenv())
  plan_environment$items <- list()
  add_plan <- function(path, action, marker = NULL, tombstone = NULL) {
    plan_environment$items <- c(plan_environment$items, list(list(
      path = path, action = action, marker = marker, tombstone = tombstone
    )))
  }
  block_live_owner <- function(path, marker = NULL) {
    owner <- .update_marker_owner(marker)
    status <- .update_owner_status(owner)
    detail <- if (status %in% c("alive", "live_token_conflict") &&
                    is.list(owner) && length(owner$pid) == 1L) {
      .bb_trf("An update is still running under pid %s; no mutation was performed.",
              owner$pid)
    } else {
      .bb_tr("An update may still be running; no mutation was performed.")
    }
    .bigbang_abort(
      "bigbang_error_update_in_progress",
      detail,
      path = project_dir, journal = path, journal_kind = "armed",
      recover = TRUE
    )
  }

  armando <- .update_journal_sibling_paths(project_dir, name, "armando")
  for (path in armando) {
    if (!dir.exists(path) || .path_is_symlink(path)) {
      add_plan(path, "apart_unarmed")
      next
    }
    marker_path <- file.path(path, "marker.rds")
    entries <- .update_journal_entries(path)
    if (!file.exists(marker_path)) {
      if (length(entries) == 0L &&
            !.journal_entries_unreadable(entries)) {
        add_plan(path, "discard_empty")
        next
      }
      add_plan(path, "apart_unarmed")
      next
    }
    marker <- tryCatch(.read_update_marker(path, project_dir, name),
                       error = function(e) NULL)
    if (is.null(marker)) {
      add_plan(path, "apart_unarmed")
      next
    }
    if (isTRUE(attr(marker, "bigbang_copy_journal"))) {
      add_plan(path, "apart_unarmed_copy", marker = marker)
      next
    }
    owner_status <- .update_owner_status(.update_marker_owner(marker))
    if (owner_status %in% c("alive", "live_token_conflict") ||
          (identical(owner_status, "uncertain") && !isTRUE(recover))) {
      add_plan(path, "blocked_live_owner", marker = marker)
    } else if (identical(owner_status, "uncertain")) {
      add_plan(path, "apart_uncertain_owner", marker = marker)
    } else {
      add_plan(path, "discard_armed", marker = marker)
    }
  }

  discarded <- .update_journal_sibling_paths(project_dir, name, "descartado")
  for (path in discarded) {
    if (!dir.exists(path) || .path_is_symlink(path)) {
      add_plan(path, "apart_discarded")
      next
    }
    tombstone <- .read_update_tombstone(path)
    if (is.null(tombstone)) {
      entries <- .update_journal_entries(path)
      if (length(entries) == 0L &&
            !.journal_entries_unreadable(entries)) {
        add_plan(path, "discard_empty")
        next
      }
      add_plan(path, "apart_discarded")
      next
    }
    if (!.update_journal_digest_valid(path)) {
      add_plan(path, "apart_discarded", tombstone = tombstone)
      next
    }
    marker <- tryCatch(.read_update_marker(path, project_dir, tombstone$name),
                       error = identity)
    if (inherits(marker, "error")) {
      add_plan(path, "apart_discarded", tombstone = tombstone)
      next
    }
    current_manifest <- .file_digest(
      file.path(project_dir, .generation_manifest_name)
    )
    if (!identical(tombstone$name, name) ||
          !identical(tombstone$manifest_hash, current_manifest)) {
      add_plan(path, "apart_discarded", marker = marker,
               tombstone = tombstone)
      next
    }
    owner_status <- .update_owner_status(.update_marker_owner(marker))
    if (owner_status %in% c("alive", "live_token_conflict") ||
          (identical(owner_status, "uncertain") && !isTRUE(recover))) {
      add_plan(path, "blocked_live_owner", marker = marker,
               tombstone = tombstone)
    } else if (identical(owner_status, "uncertain")) {
      add_plan(path, "apart_uncertain_owner", marker = marker,
               tombstone = tombstone)
    } else {
      add_plan(path, "finish_discarded", marker = marker,
               tombstone = tombstone)
    }
  }

  plan <- plan_environment$items
  if (isTRUE(dry_run)) {
    for (item in plan) {
      message(.bb_trf("Dry run: pending update journal action is %s at %s.",
                      item$action, item$path))
    }
    return(invisible(plan))
  }
  for (i in seq_along(plan)) {
    item <- plan[[i]]
    if (identical(item$action, "blocked_live_owner")) {
      block_live_owner(item$path, item$marker)
    } else if (identical(item$action, "discard_empty")) {
      unlink_status <- unlink(item$path, recursive = TRUE, force = TRUE)
      if (!identical(unlink_status, 0L) ||
            file.exists(item$path) || dir.exists(item$path)) {
        plan[[i]]$apart_path <- .apart_update_journal(item$path, name)
      }
    } else if (identical(item$action, "apart_unarmed") ||
                 identical(item$action, "apart_unarmed_copy") ||
                 identical(item$action, "apart_discarded") ||
                 identical(item$action, "apart_uncertain_owner")) {
      plan[[i]]$apart_path <- .apart_update_journal(item$path, name)
    } else if (identical(item$action, "discard_armed")) {
      owner_status <- .update_owner_status(.update_marker_owner(item$marker))
      if (owner_status %in% c("alive", "live_token_conflict") ||
            (identical(owner_status, "uncertain") && !isTRUE(recover))) {
        block_live_owner(item$path, item$marker)
      }
      .discard_update_journal(item$path, project_dir, name)
      message(.bb_trf("Discarded an orphaned armed-update folder at %s.",
                      item$path))
    } else if (identical(item$action, "finish_discarded")) {
      owner_status <- .update_owner_status(.update_marker_owner(item$marker))
      if (owner_status %in% c("alive", "live_token_conflict") ||
            (identical(owner_status, "uncertain") && !isTRUE(recover))) {
        block_live_owner(item$path, item$marker)
      }
      finished <- .finish_discarded_journal(
        item$path, name, item$tombstone, project_dir = project_dir,
        verify_project = TRUE
      )
      if (is.list(finished) && !is.null(finished$apart)) {
        plan[[i]]$apart_path <- finished$apart
      }
    }
  }
  invisible(plan)
}

# On Windows the process-termination helper always calls TerminateProcess(), whatever the
# signal, so a liveness probe there would kill the owner (or whatever process
# reused its pid). Windows owners are never probed; they are "uncertain".
.update_is_windows <- function() identical(.Platform$OS.type, "windows")

.update_signal_probe <- function(pid) if (.update_is_windows()) NA else tools::pskill(pid, 0L)

.update_process_stat <- function(pid) {
  if (.update_is_windows() || !dir.exists("/proc") ||
        length(pid) != 1L || is.na(pid) || pid <= 0) {
    return(NULL)
  }
  process_dir <- file.path("/proc", as.character(as.integer(pid)))
  if (!dir.exists(process_dir)) return("dead")
  stat <- tryCatch(
    readLines(file.path(process_dir, "stat"), warn = FALSE, n = 1L),
    error = function(e) character()
  )
  if (length(stat) != 1L) return("uncertain")
  closing <- regexpr("\\)[^)]*$", stat, perl = TRUE)
  if (closing[[1L]] < 1L) return("uncertain")
  fields <- strsplit(trimws(substring(stat, closing[[1L]] + 1L)),
                     "[[:space:]]+", perl = TRUE)[[1L]]
  if (length(fields) < 1L) return("uncertain")
  if (fields[[1L]] %in% c("Z", "X")) "dead" else "alive"
}

.update_ps_process_start <- function(pid) {
  ps <- Sys.which("ps")[[1L]]
  if (!nzchar(ps) || length(pid) != 1L || is.na(pid) || pid <= 0) {
    return(NA_character_)
  }
  output <- tryCatch(
    suppressWarnings(system2(
      ps,
      c("-o", "lstart=", "-p", as.character(as.integer(pid))),
      stdout = TRUE, stderr = FALSE, env = "LC_ALL=C"
    )),
    error = function(e) character()
  )
  status <- attr(output, "status", exact = TRUE)
  if (length(status) == 1L && !is.na(status) &&
        !identical(as.integer(status), 0L)) return(NA_character_)
  output <- trimws(output[nzchar(trimws(output))])
  if (length(output) != 1L || !nzchar(output[[1L]])) NA_character_ else output[[1L]]
}

.update_process_start <- function(pid = Sys.getpid()) {
  if (.Platform$OS.type == "windows" || length(pid) != 1L ||
        is.na(pid) || pid <= 0) return(NA_character_)
  process_state <- .update_process_stat(pid)
  if (identical(process_state, "alive") && dir.exists("/proc")) {
    stat <- tryCatch(
      readLines(sprintf("/proc/%d/stat", as.integer(pid)), warn = FALSE, n = 1L),
      error = function(e) character()
    )
    if (length(stat) == 1L) {
      closing <- regexpr("\\)[^)]*$", stat, perl = TRUE)
      if (closing[[1L]] >= 1L) {
        fields <- strsplit(trimws(substring(stat, closing[[1L]] + 1L)),
                           "[[:space:]]+", perl = TRUE)[[1L]]
        if (length(fields) >= 20L) return(fields[[20L]])
      }
    }
  }
  .update_ps_process_start(pid)
}

.update_process_start_source <- function(pid = Sys.getpid()) {
  if (.Platform$OS.type == "windows" || length(pid) != 1L ||
        is.na(pid) || pid <= 0) return(NA_character_)
  process_state <- .update_process_stat(pid)
  if (identical(process_state, "alive") && dir.exists("/proc")) {
    return("proc")
  }
  token <- .update_ps_process_start(pid)
  if (length(token) == 1L && !is.na(token) && nzchar(token)) "ps" else NA_character_
}

.update_host <- function() {
  host <- unname(Sys.info()[["nodename"]])
  if (is.null(host) || is.na(host) || !nzchar(host)) NA_character_ else host
}

.update_owner_record <- function() {
  list(
    pid = as.integer(Sys.getpid()),
    host = .update_host(),
    started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    process_start = .update_process_start(),
    process_start_source = .update_process_start_source()
  )
}

.update_marker_owner <- function(marker) {
  fields <- c("pid", "host", "started_utc", "process_start")
  if (!is.list(marker) || !all(fields %in% names(marker))) return(NULL)
  if ("process_start_source" %in% names(marker)) {
    fields <- c(fields, "process_start_source")
  }
  marker[fields]
}

.valid_update_relative_path <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    !grepl("(^/|^[A-Za-z]:[/\\\\]|^~|(^|[/\\\\])\\.\\.([/\\\\]|$)|[\\r\\n\\t])",
           path, perl = TRUE)
}

.read_update_marker <- function(journal_path, project_dir, name) {
  fail <- function() {
    .bigbang_abort(
      "bigbang_error_unrecognized_update_journal",
      .bb_trf(
        paste0(
          "Cannot update %s because %s is not a recognized bigbang update ",
          "journal for this project. Move it aside or restore the expected journal."
        ),
        project_dir, journal_path
      ),
      path = project_dir, journal = journal_path
    )
  }
  fail_name <- function(marker_name) {
    .bigbang_abort(
      "bigbang_error_unrecognized_update_journal",
      .bb_trf(
        paste0(
          "Cannot update %s because journal %s belongs to project '%s', not ",
          "project '%s'. Rename the project and its journal back to '%s'."
        ),
        project_dir, journal_path, marker_name, name, name
      ),
      path = project_dir, journal = journal_path,
      journal_project = marker_name
    )
  }
  if (!dir.exists(journal_path) || .path_is_symlink(journal_path)) fail()
  marker_path <- file.path(journal_path, "marker.rds")
  if (!file.exists(marker_path) || dir.exists(marker_path) ||
        .path_is_symlink(marker_path)) fail()
  marker <- tryCatch(readRDS(marker_path), error = function(e) NULL)
  owner_fields <- c("pid", "host", "started_utc", "process_start")
  owner_names <- intersect(owner_fields, names(marker))
  owner_valid <- if (!is.list(marker)) {
    FALSE
  } else if (length(owner_names) == 0L) {
    TRUE
  } else {
    length(owner_names) == length(owner_fields) &&
      (is.integer(marker$pid) || is.numeric(marker$pid)) &&
      length(marker$pid) == 1L && !is.na(marker$pid) && marker$pid > 0 &&
      is.character(marker$host) && length(marker$host) == 1L &&
      is.character(marker$started_utc) && length(marker$started_utc) == 1L &&
      is.character(marker$process_start) && length(marker$process_start) == 1L &&
      (!"process_start_source" %in% names(marker) ||
         (is.character(marker$process_start_source) &&
            length(marker$process_start_source) == 1L))
  }
  shape_valid <- is.list(marker) && identical(marker$format, .update_journal_format) &&
    identical(marker$version, .update_journal_version) &&
    is.character(marker$name) && length(marker$name) == 1L && nzchar(marker$name) &&
    is.character(marker$backup_hashes) && !is.null(names(marker$backup_hashes)) &&
    !anyDuplicated(names(marker$backup_hashes)) &&
    all(grepl("^[0-9a-f]{32}$", marker$backup_hashes)) &&
    is.character(marker$planned_files) &&
    !anyDuplicated(marker$planned_files) &&
    isTRUE(owner_valid) &&
    all(names(marker$backup_hashes) %in% marker$planned_files) &&
    all(vapply(names(marker$backup_hashes), .valid_update_relative_path,
               logical(1L))) && all(vapply(marker$planned_files,
                                           .valid_update_relative_path,
                                           logical(1L)))
  if (!isTRUE(shape_valid)) fail()
  if (identical(marker$name, name)) return(marker)
  source_project <- file.path(dirname(project_dir), marker$name)
  if (dir.exists(source_project)) {
    attr(marker, "bigbang_copy_journal") <- TRUE
    return(marker)
  }
  sibling <- tryCatch(
    identical(
      normalizePath(dirname(journal_path), winslash = "/", mustWork = TRUE),
      normalizePath(dirname(project_dir), winslash = "/", mustWork = TRUE)
    ),
    error = function(e) FALSE
  )
  current_manifest <- .file_digest(
    file.path(project_dir, .generation_manifest_name)
  )
  recorded_manifest <- unname(
    marker$backup_hashes[[.generation_manifest_name]]
  )
  backed_manifest <- .file_digest(file.path(
    journal_path, "backup", .generation_manifest_name
  ))
  if (isTRUE(sibling) && !is.na(current_manifest) &&
        identical(recorded_manifest, current_manifest) &&
        identical(backed_manifest, current_manifest)) {
    return(marker)
  }
  fail_name(marker$name)
}

.discard_update_journal <- function(journal, project_dir, name) {
  if (is.null(journal)) return(invisible(NULL))
  path <- if (is.character(journal)) journal else journal$path
  marker <- .read_update_marker(path, project_dir, name)
  state_path <- file.path(path, "state.rds")
  state <- if (file.exists(state_path) && !dir.exists(state_path)) {
    tryCatch(readRDS(state_path), error = function(e) NULL)
  } else {
    NULL
  }
  expected <- if (is.list(state) && isTRUE(state$armed)) {
    .armed_journal_is_expected(path, marker)
  } else {
    .unarmed_journal_is_expected(path, marker)
  }
  if (!expected) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", project_dir, path,
      if (is.list(state) && isTRUE(state$armed)) "armed" else "unarmed",
      .bb_tr("It contains unexpected files; nothing was deleted."),
      has_backup = isTRUE(length(marker$backup_hashes) > 0L),
      next_step = .bb_tr(
        "leave the folder intact, preserve the backup, and inspect the unexpected entries before retrying."
      )
    )
  }
  .discard_renamed_journal(path, project_dir, name, marker)
  invisible(NULL)
}

.journal_backup_copy <- function(source, destination) {
  parent <- dirname(destination)
  if (!dir.exists(parent) &&
        !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop(.bb_trf("Could not create temporary directory for %s", destination),
         call. = FALSE)
  }
  .atomic_copy(source, destination)
}

.create_update_journal <- function(project_dir, name, manifest,
                                   extra_files = character(), lock = NULL) {
  journal_path <- .update_journal_path(project_dir)
  .assert_update_lock_owner(lock, project_dir, "update journal creation")
  copy_journal <- FALSE
  if (file.exists(journal_path) || dir.exists(journal_path)) {
    existing <- tryCatch(
      .read_update_marker(journal_path, project_dir, name),
      error = identity
    )
    if (inherits(existing, "error") ||
          !isTRUE(attr(existing, "bigbang_copy_journal"))) {
      stop("Internal error: an update journal is already present", call. = FALSE)
    }
    copy_journal <- TRUE
  }
  owned <- unique(c(manifest$files, .generation_manifest_name))
  owned_paths <- file.path(project_dir, owned)
  missing <- owned[!file.exists(owned_paths) | dir.exists(owned_paths)]
  if (length(missing) > 0L) {
    stop(.bb_trf("Could not back up generated file: %s",
                 file.path(project_dir, missing[[1L]])), call. = FALSE)
  }
  planned_files <- unique(c(owned, extra_files))
  extra_files <- extra_files[file.exists(file.path(project_dir, extra_files)) &
                               !dir.exists(file.path(project_dir, extra_files))]
  files <- unique(c(owned, extra_files))
  hashes <- vapply(file.path(project_dir, files), .file_digest, character(1L))
  names(hashes) <- files
  staging_path <- tempfile(
    pattern = paste0(".", basename(project_dir), ".bigbang-update.armando-"),
    tmpdir = dirname(project_dir)
  )
  if (!dir.create(staging_path, showWarnings = FALSE)) {
    stop(.bb_trf("Could not create temporary directory for %s", journal_path),
         call. = FALSE)
  }

  owner <- .update_owner_record()
  marker <- c(list(
    format = .update_journal_format,
    version = .update_journal_version,
    name = name,
    backup_hashes = hashes,
    planned_files = planned_files
  ), owner)
  .atomic_save_rds(marker, file.path(staging_path, "marker.rds"))

  for (relative in files) {
    .assert_update_lock_owner(lock, project_dir, "update journal backup")
    .journal_backup_copy(
      file.path(project_dir, relative),
      file.path(staging_path, "backup", relative)
    )
  }
  copied <- vapply(
    file.path(staging_path, "backup", files), .file_digest, character(1L)
  )
  if (!identical(unname(copied), unname(hashes[files]))) {
    stop(.bb_trf("Could not back up generated file: %s", project_dir),
         call. = FALSE)
  }

  if (!dir.create(file.path(staging_path, "staging"))) {
    stop(.bb_trf("Could not create temporary directory for %s", journal_path),
         call. = FALSE)
  }

  state <- c(list(
    armed = TRUE,
    bigbang_version = .bb_generator_version(),
    original_hashes = hashes,
    old_manifest_hash = unname(hashes[[.generation_manifest_name]])
  ), owner)
  .assert_update_lock_owner(lock, project_dir, "update journal publication")
  .atomic_save_rds(state, file.path(staging_path, "state.rds"))
  if (!copy_journal && (file.exists(journal_path) || dir.exists(journal_path))) {
    stop(.bb_trf("Could not arm the update journal because its final path already exists: %s",
                 journal_path), call. = FALSE)
  }
  .assert_update_lock_owner(lock, project_dir, "update journal publication")
  if (!copy_journal && !file.rename(staging_path, journal_path)) {
    stop(.bb_trf("Could not arm the update journal at %s", journal_path),
         call. = FALSE)
  }
  list(path = if (copy_journal) staging_path else journal_path,
       marker = marker, state = state, copy_journal = copy_journal)
}

.activate_update_journal <- function(journal, project_dir, name,
                                     record = TRUE, lock = NULL) {
  .update_journal_runtime$current <- list(
    path = journal$path,
    project = normalizePath(project_dir, winslash = "/", mustWork = TRUE),
    name = name,
    record = isTRUE(record),
    lock = lock
  )
  invisible(NULL)
}

.deactivate_update_journal <- function() {
  .update_journal_runtime$current <- NULL
  invisible(NULL)
}

.project_relative_destination <- function(destination, project_dir) {
  # A new file does not exist yet, so normalizePath() would leave it spelled
  # through an alias (macOS /var, a Windows short name) while the existing
  # project resolves to its physical path. A write that is not recognized as
  # inside the project is not recorded in the journal at all.
  destination <- .resolve_physical_path(destination)
  project_dir <- .resolve_physical_path(project_dir)
  prefix <- paste0(sub("/+$", "", project_dir), "/")
  inside <- if (identical(.Platform$OS.type, "windows")) {
    startsWith(tolower(destination), tolower(prefix))
  } else {
    startsWith(destination, prefix)
  }
  if (!inside) return(NULL)
  relative <- substring(destination, nchar(prefix) + 1L)
  if (!.valid_update_relative_path(relative)) return(NULL)
  relative
}

.append_update_intent <- function(operation, relative, hash = "-") {
  context <- .update_journal_runtime$current
  if (is.null(context) || !isTRUE(context$record)) return(invisible(NULL))
  .assert_update_lock_owner(context$lock, context$project,
                            "update intent recording")
  line <- paste(operation, hash, relative, sep = "\t")
  path <- file.path(context$path, "intent.log")
  old <- if (file.exists(path)) readLines(path, warn = FALSE, encoding = "UTF-8") else character()
  .write_utf8(c(old, line), path)
  invisible(NULL)
}

.record_update_write <- function(source, destination) {
  context <- .update_journal_runtime$current
  if (is.null(context)) return(invisible(NULL))
  relative <- .project_relative_destination(destination, context$project)
  if (is.null(relative)) return(invisible(NULL))
  hash <- .file_digest(source)
  if (is.na(hash)) stop("Internal error: update write has no digest", call. = FALSE)
  .append_update_intent("write", relative, hash)
}

.record_update_delete <- function(path) {
  context <- .update_journal_runtime$current
  if (is.null(context)) return(invisible(NULL))
  relative <- .project_relative_destination(path, context$project)
  if (is.null(relative)) return(invisible(NULL))
  .append_update_intent("delete", relative)
}

.read_update_intents <- function(journal_path, project_dir = journal_path) {
  path <- file.path(journal_path, "intent.log")
  if (!file.exists(path)) {
    return(data.frame(operation = character(), hash = character(),
                      path = character(), stringsAsFactors = FALSE))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  fields <- strsplit(lines, "\t", fixed = TRUE)
  valid <- lengths(fields) == 3L & vapply(fields, function(x) {
    x[[1L]] %in% c("write", "delete") &&
      (identical(x[[1L]], "delete") || grepl("^[0-9a-f]{32}$", x[[2L]])) &&
      .valid_update_relative_path(x[[3L]])
  }, logical(1L))
  if (!all(valid)) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal",
      project_dir, journal_path, "armed",
      .bb_tr("Its intention log is invalid; nothing was deleted."),
      has_backup = TRUE,
      next_step = .bb_tr("leave the folder intact, preserve the backup, and inspect the intention log before retrying.")
    )
  }
  if (length(fields) == 0L) {
    return(data.frame(operation = character(), hash = character(),
                      path = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    operation = vapply(fields, `[[`, character(1L), 1L),
    hash = vapply(fields, `[[`, character(1L), 2L),
    path = vapply(fields, `[[`, character(1L), 3L),
    stringsAsFactors = FALSE
  )
}

.staged_update_write_matches <- function(project_dir, relative, intended) {
  if (length(intended) == 0L) return(FALSE)
  staging <- file.path(.update_journal_path(project_dir), "staging")
  if (!dir.exists(staging)) return(FALSE)
  entries <- list.files(staging, all.files = TRUE, full.names = TRUE,
                        no.. = TRUE)
  prefix <- paste0(".", basename(relative), "-")
  candidates <- entries[
    startsWith(basename(entries), prefix) &
      grepl("-[[:alnum:]]+$", basename(entries), perl = TRUE) &
      file.exists(entries) & !dir.exists(entries)
  ]
  if (length(candidates) == 0L) return(FALSE)
  any(vapply(candidates, function(path) {
    digest <- .file_digest(path)
    !is.na(digest) && digest %in% intended
  }, logical(1L)))
}

.manifest_matches_project <- function(project_dir) {
  manifest <- .read_generation_manifest(project_dir)
  if (is.null(manifest) || !is.character(manifest$files) ||
        !is.character(manifest$hashes) || anyNA(manifest$files) ||
        !identical(manifest$schema, 2L) || anyDuplicated(manifest$files) ||
        is.null(names(manifest$hashes)) ||
        !all(manifest$files %in% names(manifest$hashes)) ||
        any(!vapply(manifest$files, .valid_update_relative_path, logical(1L)))) {
    return(FALSE)
  }
  paths <- file.path(project_dir, manifest$files)
  all(file.exists(paths) & !dir.exists(paths)) && all(vapply(
    seq_along(paths), function(i) {
      identical(.file_digest(paths[[i]]),
                unname(manifest$hashes[[manifest$files[[i]]]]))
    }, logical(1L)
  ))
}

.update_owner_liveness <- function(state) {
  if (!is.list(state) || length(state$pid) != 1L ||
        is.na(state$pid) || length(state$host) != 1L ||
        !is.character(state$host) || is.na(state$host) ||
        length(.update_host()) != 1L || is.na(.update_host())) {
    return("uncertain")
  }
  if (!identical(state$host, .update_host())) return("uncertain")
  if (.update_is_windows()) return("uncertain")
  process_state <- .update_process_stat(state$pid)
  if (identical(process_state, "dead")) return("dead")
  if (identical(process_state, "uncertain")) return("uncertain")
  if (is.null(process_state)) {
    probe <- tryCatch(
      .update_signal_probe(state$pid),
      error = function(e) NA
    )
    if (isTRUE(probe)) {
      process_state <- "alive"
    } else {
      ps <- Sys.which("ps")[[1L]]
      if (!nzchar(ps)) return("uncertain")
      # ps reports a missing pid with exit status 1, which system2() turns
      # into a translated warning; the status attribute is read below.
      listed <- tryCatch(
        suppressWarnings(
          system2(ps, c("-p", as.character(as.integer(state$pid)), "-o", "pid="),
                  stdout = TRUE, stderr = FALSE, env = "LC_ALL=C")
        ),
        error = function(e) character()
      )
      ps_status <- attr(listed, "status", exact = TRUE)
      if (length(ps_status) == 1L && !is.na(ps_status) &&
            !identical(as.integer(ps_status), 0L)) {
        if (identical(as.integer(ps_status), 1L)) return("dead")
        return("uncertain")
      }
      listed <- trimws(listed[nzchar(trimws(listed))])
      if (length(listed) == 0L) return("dead")
      process_state <- "alive"
    }
  }
  if (!identical(process_state, "alive")) return("uncertain")
  recorded <- state$process_start
  current <- .update_process_start(state$pid)
  recorded_source <- if ("process_start_source" %in% names(state)) {
    state$process_start_source
  } else if (identical(.update_process_start_source(state$pid), "proc")) {
    # Four-field records from 0.4/early 0.5 can only be compared safely on
    # the /proc path. A ps token without a recorded source remains uncertain.
    "proc"
  } else {
    NA_character_
  }
  current_source <- .update_process_start_source(state$pid)
  if (length(recorded) != 1L || is.na(recorded) ||
        length(current) != 1L || is.na(current) ||
        length(recorded_source) != 1L || is.na(recorded_source) ||
        length(current_source) != 1L || is.na(current_source)) return("uncertain")
  if (!identical(recorded_source, current_source)) return("live_token_conflict")
  if (!identical(recorded, current)) return("live_token_conflict")
  "alive"
}

.update_owner_may_be_alive <- function(state) {
  !identical(.update_owner_liveness(state), "dead")
}

.update_owner_status <- function(state) {
  raw <- .update_owner_liveness(state)
  # Keep the older helper as the testable compatibility seam: callers that
  # replace it to model a dead or uncertain owner still get that model here.
  observed <- .update_owner_may_be_alive(state)
  if (!isTRUE(observed)) return("dead")
  if (identical(raw, "dead")) return("uncertain")
  raw
}

.update_lock_path <- function(project_dir) {
  file.path(dirname(project_dir),
            paste0(".", basename(project_dir), ".bigbang-update.lock"))
}

.update_lock_owner_path <- function(lock_path) file.path(lock_path, "owner.rds")

.read_update_lock_owner <- function(lock_path) {
  if (!dir.exists(lock_path) || .path_is_symlink(lock_path)) return(NULL)
  owner_path <- .update_lock_owner_path(lock_path)
  if (!file.exists(owner_path) || dir.exists(owner_path) ||
        .path_is_symlink(owner_path)) return(NULL)
  owner <- tryCatch(readRDS(owner_path), error = function(e) NULL)
  if (!is.list(owner) || length(owner$pid) != 1L ||
        length(owner$host) != 1L || length(owner$started_utc) != 1L ||
        length(owner$process_start) != 1L ||
        ("process_start_source" %in% names(owner) &&
           (length(owner$process_start_source) != 1L ||
              !is.character(owner$process_start_source))) ||
        is.na(owner$pid) || is.na(owner$host) || is.na(owner$started_utc)) {
    return(NULL)
  }
  owner
}

.update_lock_problem <- function(project_dir, lock_path, owner = NULL,
                                 status = "uncertain", next_step = NULL) {
  message_text <- if (status %in% c("alive", "live_token_conflict") &&
                        is.list(owner) && length(owner$pid) == 1L) {
    .bb_trf("An update is still running under pid %s; no mutation was performed.",
            owner$pid)
  } else if (identical(status, "symbolic_link")) {
    .bb_trf(
      paste0(
        "The update-lock path %s is a symbolic link or unreadable entry; no ",
        "mutation was performed. Remove or rename that link, then retry."
      ),
      lock_path
    )
  } else if (identical(status, "lock_creation")) {
    next_step
  } else if (identical(status, "lost_lock")) {
    next_step
  } else if (identical(status, "lock_retry")) {
    next_step
  } else {
    paste(
      .bb_tr("An update may still be running; no mutation was performed."),
      .bb_tr("after confirming that no other update is running, call again with recover = TRUE."),
      sep = " "
    )
  }
  .bigbang_abort(
    "bigbang_error_update_in_progress",
    message_text,
    path = project_dir, lock = lock_path, recover = TRUE
  )
}

.update_lock_is_owner <- function(lock) {
  !is.null(lock) && is.list(lock) &&
    is.character(lock$path) && length(lock$path) == 1L &&
    isTRUE(identical(.read_update_lock_owner(lock$path), lock$owner))
}

.assert_update_lock_owner <- function(lock, project_dir, phase = "update") {
  if (is.null(lock) || isTRUE(.update_lock_is_owner(lock))) {
    return(invisible(TRUE))
  }
  .update_lock_problem(
    project_dir, .update_lock_path(project_dir), status = "lost_lock",
    next_step = .bb_trf(
      paste0(
        "Cannot continue the %s because the published update lock no longer ",
        "belongs to this process; no further mutation was performed."
      ),
      phase
    )
  )
}

.update_lock_temporary_paths <- function(project_dir) {
  prefix <- paste0(".", basename(project_dir),
                   ".bigbang-update.lock.armando-")
  entries <- list.files(dirname(project_dir), full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
  entries <- entries[startsWith(basename(entries), prefix)]
  suffix <- substring(basename(entries), nchar(prefix) + 1L)
  entries[nzchar(suffix) & grepl("^[[:alnum:]-]+$", suffix)]
}

.update_lock_temporary_path <- function(project_dir) {
  tempfile(
    pattern = paste0(".", basename(project_dir),
                     ".bigbang-update.lock.armando-"),
    tmpdir = dirname(project_dir)
  )
}

.update_lock_discard_path <- function(path, name) {
  stamp <- paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-",
                  Sys.getpid())
  base <- file.path(dirname(path), paste0(".", name,
                                          ".bigbang-update.lock.descartado-",
                                          stamp))
  destination <- base
  suffix <- 0L
  while (file.exists(destination) || dir.exists(destination)) {
    suffix <- suffix + 1L
    destination <- paste0(base, "-", suffix)
  }
  destination
}

.update_lock_discarded_paths <- function(project_dir) {
  prefix <- paste0(".", basename(project_dir),
                   ".bigbang-update.lock.descartado-")
  entries <- list.files(dirname(project_dir), full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
  entries <- entries[startsWith(basename(entries), prefix)]
  suffix <- substring(basename(entries), nchar(prefix) + 1L)
  entries[
    nzchar(suffix) & grepl("^[[:alnum:]-]+$", suffix) &
      dir.exists(entries)
  ]
}

.update_lock_claim_path <- function(path) file.path(path, "claim.rds")

.update_lock_record_digest <- function(path, record) {
  record_path <- if (identical(record, "owner")) {
    .update_lock_owner_path(path)
  } else {
    .update_lock_claim_path(path)
  }
  if (.path_is_symlink(record_path)) return(NA_character_)
  readable <- if (identical(record, "owner")) {
    !is.null(.read_update_lock_owner(path))
  } else {
    !is.null(.read_update_lock_claim(path))
  }
  if (!isTRUE(readable)) return(NA_character_)
  .file_digest(record_path)
}

.update_lock_value_digest <- function(value) {
  if (!is.list(value)) return(NA_character_)
  temporary <- tempfile("bigbang-lock-record-")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary)
  .file_digest(temporary)
}

.read_update_lock_claim <- function(path) {
  claim_path <- .update_lock_claim_path(path)
  if (!file.exists(claim_path) || dir.exists(claim_path)) return(NULL)
  claim <- tryCatch(readRDS(claim_path), error = function(e) NULL)
  if (!is.list(claim) || length(claim$pid) != 1L ||
        length(claim$host) != 1L || length(claim$started_utc) != 1L ||
        length(claim$process_start) != 1L || is.na(claim$pid) ||
        is.na(claim$host) || is.na(claim$started_utc) ||
        ("process_start_source" %in% names(claim) &&
           (length(claim$process_start_source) != 1L ||
              !is.character(claim$process_start_source)))) return(NULL)
  claim
}

.update_lock_inventory_discard <- function(path, owner, name,
                                           owner_digest = NULL,
                                           claim = owner, claim_digest = NULL) {
  owner_path <- .update_lock_owner_path(path)
  claim_path <- .update_lock_claim_path(path)
  expected_owner_digest <- if (is.null(owner_digest)) {
    .update_lock_value_digest(owner)
  } else {
    owner_digest
  }
  expected_claim_digest <- if (is.null(claim_digest)) {
    .update_lock_value_digest(claim)
  } else {
    claim_digest
  }
  removable <- c(owner_path, claim_path)
  expected <- c(expected_owner_digest, expected_claim_digest)
  unlink_failed <- FALSE
  for (index in seq_along(removable)) {
    candidate <- removable[[index]]
    if (.path_is_symlink(candidate) || is.na(expected[[index]]) ||
          !file.exists(candidate) || dir.exists(candidate)) next
    current <- .file_digest(candidate)
    if (!is.na(current) && identical(current, expected[[index]])) {
      removed <- tryCatch(unlink(candidate, force = TRUE), error = function(e) 1L)
      if (!identical(removed, 0L) || file.exists(candidate) ||
            dir.exists(candidate) || .path_is_symlink(candidate)) {
        unlink_failed <- TRUE
      }
    }
  }
  remaining <- .update_journal_entries(path)
  if (!unlink_failed && !.journal_entries_unreadable(remaining) &&
        length(remaining) == 0L) {
    removed <- tryCatch(unlink(path, recursive = TRUE, force = TRUE),
                        error = function(e) 1L)
    gone <- !file.exists(path) && !dir.exists(path) && !.path_is_symlink(path)
    if (identical(removed, 0L) && gone) return(invisible(NULL))
    unlink_failed <- TRUE
  }
  if (!unlink_failed && !.journal_entries_unreadable(remaining) &&
        length(remaining) == 0L) {
    return(invisible(NULL))
  }
  destination <- .update_lock_apart_path(path, name)
  if (!file.rename(path, destination)) {
    stop(.bb_trf("Could not set aside the update-lock entry: %s", path),
         call. = FALSE)
  }
  message(.bb_trf(
    "Set aside old update-lock content at %s; no bytes from the set-aside entry were deleted.",
    destination
  ))
  invisible(destination)
}

.update_lock_state <- function(project_dir, recover = FALSE) {
  lock_path <- .update_lock_path(project_dir)
  entry_exists <- file.exists(lock_path) || dir.exists(lock_path) ||
    .path_is_symlink(lock_path)
  discarded <- .update_lock_discarded_paths(project_dir)
  if (entry_exists && length(discarded) > 0L) {
    discarded_path <- discarded[[1L]]
    discarded_owner <- .read_update_lock_owner(discarded_path)
    discarded_status <- if (is.null(discarded_owner)) {
      "uncertain"
    } else {
      .update_owner_status(discarded_owner)
    }
    if (discarded_status %in% c("alive", "live_token_conflict") ||
          (identical(discarded_status, "uncertain") && !isTRUE(recover))) {
      action <- if (identical(discarded_status, "uncertain")) {
        "blocked_uncertain_discard"
      } else {
        "blocked_live_discard"
      }
      return(list(
        status = discarded_status, path = lock_path, owner = discarded_owner,
        old_owner = discarded_owner, claim_path = discarded_path,
        owner_digest = .update_lock_record_digest(discarded_path, "owner"),
        claim_digest = .update_lock_record_digest(discarded_path, "claim"),
        action = action
      ))
    }
  }
  if (file.exists(lock_path) && !dir.exists(lock_path)) {
    return(list(status = "user_entry", path = lock_path, owner = NULL,
                action = "apart_user_lock"))
  }
  if (.path_is_symlink(lock_path)) {
    return(list(status = "uncertain", path = lock_path, owner = NULL,
                action = "blocked_lock"))
  }
  if (!entry_exists) {
    if (length(discarded) > 0L) {
      claim <- .read_update_lock_claim(discarded[[1L]])
      old_owner <- .read_update_lock_owner(discarded[[1L]])
      old_owner_status <- if (is.null(old_owner)) {
        "uncertain"
      } else {
        .update_owner_status(old_owner)
      }
      claim_status <- if (is.null(claim)) {
        "uncertain"
      } else {
        .update_owner_status(claim)
      }
      action <- if (old_owner_status %in% c("alive", "live_token_conflict")) {
        "restore_live_discard"
      } else if (identical(old_owner_status, "uncertain") &&
                   !isTRUE(recover)) {
        "blocked_uncertain_discard"
      } else if (identical(old_owner_status, "uncertain")) {
        if (is.null(old_owner) && is.null(claim)) {
          "set_aside_unreadable_discard"
        } else {
          "claim_uncertain_discard"
        }
      } else if (claim_status %in% c("alive", "live_token_conflict")) {
        "blocked_live_claim"
      } else if (identical(claim_status, "uncertain") &&
                   !isTRUE(recover)) {
        "blocked_claim"
      } else {
        "claim_orphan_discard"
      }
      status <- if (identical(action, "restore_live_discard")) {
        old_owner_status
      } else if (action %in% c("blocked_uncertain_discard",
                               "set_aside_unreadable_discard")) {
        old_owner_status
      } else {
        claim_status
      }
      return(list(status = status, path = lock_path, owner = claim,
                  old_owner = old_owner, claim_path = discarded[[1L]],
                  claim_status = claim_status,
                  old_owner_status = old_owner_status,
                  owner_digest = .update_lock_record_digest(
                    discarded[[1L]], "owner"
                  ),
                  claim_digest = .update_lock_record_digest(
                    discarded[[1L]], "claim"
                  ),
                  action = action))
    }
    return(list(status = "free", path = lock_path, owner = NULL,
                action = "free"))
  }
  owner <- .read_update_lock_owner(lock_path)
  if (is.null(owner)) {
    return(list(status = "uncertain", path = lock_path, owner = NULL,
                owner_digest = .update_lock_record_digest(lock_path, "owner"),
                action = if (isTRUE(recover)) "set_aside_unreadable_lock" else
                  "blocked_uncertain_lock"))
  }
  status <- .update_owner_status(owner)
  action <- if (status %in% c("alive", "live_token_conflict")) {
    "blocked_live_lock"
  } else if (identical(status, "uncertain")) {
    if (isTRUE(recover)) "claim_uncertain_lock" else "blocked_uncertain_lock"
  } else {
    "claim_orphan_lock"
  }
  list(status = status, path = lock_path, owner = owner,
       owner_digest = .update_lock_record_digest(lock_path, "owner"),
       action = action)
}

.report_update_lock_dry_run <- function(state) {
  action <- switch(state$action,
    free = "free",
    blocked_live_lock = "live",
    blocked_uncertain_lock = "uncertain",
    claim_orphan_lock = "orphan reclaimable",
    claim_uncertain_lock = "uncertain and reclaimable with recover = TRUE",
    blocked_uncertain_temporary = "blocked: an uncertain lock preparation would make the update abort",
    blocked_live_temporary = "blocked: a live lock preparation is present",
    set_aside_symbolic_link = "symbolic link to set aside with recover = TRUE",
    apart_user_lock = "user entry to set aside",
    blocked_lock = "symbolic link or unreadable entry",
    blocked_live_claim = "live orphan-recovery claim",
    blocked_live_discard = "live owner in discarded lock",
    blocked_claim = "uncertain orphan-recovery claim",
    restore_live_discard = "live owner in discarded lock (would be restored)",
    blocked_uncertain_discard = "uncertain discarded owner",
    claim_uncertain_discard = "uncertain discarded owner reclaimable with recover = TRUE",
    set_aside_unreadable_lock = "unreadable lock to set aside with recover = TRUE",
    set_aside_unreadable_discard = "unreadable discarded lock to set aside",
    lock_creation = if (is.null(state$creation_reason)) {
      "the lock parent is not writable"
    } else {
      state$creation_reason
    },
    claim_orphan_discard = "orphan-recovery claim to discard",
    state$action
  )
  message(.bb_trf(
    "Dry run: update lock at %s is %s; no lock was acquired or changed.",
    state$path, action
  ))
  invisible(state)
}

.reconcile_lock_temps <- function(project_dir, recover = FALSE,
                                  dry_run = FALSE) {
  paths <- .update_lock_temporary_paths(project_dir)
  if (length(paths) == 0L) return(list())
  plans <- lapply(paths, function(path) {
    owner <- .read_update_lock_owner(path)
    status <- if (is.null(owner)) "uncertain" else .update_owner_status(owner)
    action <- if (is.null(owner)) {
      "apart_lock_preparation"
    } else if (status %in% c("alive", "live_token_conflict")) {
      "live_lock_preparation"
    } else if (identical(status, "uncertain") && !isTRUE(recover)) {
      "uncertain_lock_preparation"
    } else {
      "apart_lock_preparation"
    }
    list(path = path, owner = owner, status = status, action = action)
  })
  for (item in plans) {
    if (isTRUE(dry_run)) {
      message(.bb_trf(
        "Dry run: update-lock preparation at %s would be %s; no entry was changed.",
        item$path, item$action
      ))
      next
    }
    if (!identical(item$action, "apart_lock_preparation")) next
    destination <- .update_lock_apart_path(item$path, basename(project_dir))
    if (file.rename(item$path, destination)) {
      message(.bb_trf(
        "Set aside incomplete update-lock preparation at %s; no bytes from the set-aside entry were deleted.",
        destination
      ))
    }
  }
  invisible(plans)
}

.journal_directory_chain <- function(path) {
  out <- character()
  current <- dirname(path)
  while (!identical(current, ".") && nzchar(current)) {
    out <- c(out, current)
    current <- dirname(current)
  }
  out
}

.update_lock_creation_problem <- function(project_dir, lock_path, reason) {
  reason <- if (length(reason) != 1L || is.na(reason) || !nzchar(reason)) {
    .bb_tr("the lock parent rejected creation or has no available space")
  } else {
    reason
  }
  .update_lock_problem(
    project_dir, lock_path, status = "lock_creation",
    next_step = .bb_trf(
      paste0(
        "Could not prepare the update lock at %s: %s. Check that its parent is ",
        "writable and has free space; no mutation was performed."
      ),
      lock_path, reason
    )
  )
}

.update_lock_parent_writable <- function(path) {
  isTRUE(file.access(path, 2L) == 0L)
}

.update_lock_make_temporary <- function(project_dir, lock_path) {
  temporary <- .update_lock_temporary_path(project_dir)
  warning_text <- character()
  created <- withCallingHandlers(
    dir.create(temporary, showWarnings = TRUE),
    warning = function(condition) {
      warning_text <<- c(warning_text, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  if (isTRUE(created)) return(temporary)
  if (file.exists(lock_path) || dir.exists(lock_path) ||
        .path_is_symlink(lock_path)) return(NULL)
  reason <- if (length(warning_text) > 0L) warning_text[[1L]] else if (
    !.update_lock_parent_writable(dirname(lock_path))
  ) {
    .bb_tr("the lock parent is not writable")
  } else {
    .bb_tr("the lock parent rejected creation or has no available space")
  }
  .update_lock_creation_problem(project_dir, lock_path, reason)
}

.acquire_update_lock <- function(project_dir, recover = FALSE,
                                 dry_run = FALSE) {
  lock_path <- .update_lock_path(project_dir)
  owner <- .update_owner_record()
  temporary_plan <- .reconcile_lock_temps(
    project_dir, recover = recover, dry_run = dry_run
  )
  if (isTRUE(dry_run)) {
    state <- .update_lock_state(project_dir, recover = recover)
    uncertain_temporary <- vapply(
      temporary_plan,
      function(item) identical(item$action, "uncertain_lock_preparation"),
      logical(1L)
    )
    live_temporary <- Filter(function(item) {
      item$action %in% c("live_lock_preparation")
    }, temporary_plan)
    if (length(live_temporary) > 0L) {
      state$status <- live_temporary[[1L]]$status
      state$owner <- live_temporary[[1L]]$owner
      state$action <- "blocked_live_temporary"
    } else if (any(uncertain_temporary) && !isTRUE(recover)) {
      state$status <- "blocked"
      state$action <- "blocked_uncertain_temporary"
    } else if (identical(state$action, "blocked_lock") &&
                 isTRUE(recover)) {
      state$action <- "set_aside_symbolic_link"
    }
    if (identical(state$action, "free") &&
          !.update_lock_parent_writable(dirname(lock_path))) {
      state$action <- "lock_creation"
      state$creation_reason <- .bb_tr("the lock parent is not writable")
    }
    .report_update_lock_dry_run(state)
    return(c(state, list(owner = owner, temporary = temporary_plan,
                         acquired = FALSE)))
  }
  uncertain_temporary <- vapply(
    temporary_plan,
    function(item) identical(item$action, "uncertain_lock_preparation"),
    logical(1L)
  )
  if (any(uncertain_temporary)) {
    .update_lock_problem(project_dir, lock_path, status = "uncertain")
  }
  live_temporary <- Filter(function(item) {
    item$action %in% c("live_lock_preparation")
  }, temporary_plan)
  if (length(live_temporary) > 0L) {
    .update_lock_problem(
      project_dir, lock_path, live_temporary[[1L]]$owner,
      live_temporary[[1L]]$status
    )
  }
  no_progress <- 0L
  repeat {
    state <- .update_lock_state(project_dir, recover = recover)
    progress_actions <- c(
      "user_entry", "set_aside_symbolic_link", "set_aside_unreadable_lock",
      "set_aside_unreadable_discard", "claim_orphan_discard",
      "claim_uncertain_discard"
    )
    if (state$action %in% progress_actions) {
      no_progress <- 0L
    } else {
      no_progress <- no_progress + 1L
    }
    if (no_progress > 100L) {
      reason <- if (identical(state$action, "free")) {
        .bb_tr("the free lock could not publish its temporary preparation")
      } else {
        .bb_trf("the lock entry remained in state %s", state$action)
      }
      .update_lock_problem(
        project_dir, lock_path, status = "lock_retry",
        next_step = .bb_trf(
          paste0(
            "Could not acquire the update lock at %s because %s made no ",
            "progress after %s consecutive retries; inspect entry %s before retrying."
          ),
          lock_path, reason, no_progress, state$path
        )
      )
    }
    if (identical(state$status, "user_entry")) {
      .apart_update_lock(lock_path, basename(project_dir))
      next
    }
    if (identical(state$status, "uncertain") &&
          identical(state$action, "blocked_lock")) {
      if (isTRUE(recover)) {
        .apart_update_lock(lock_path, basename(project_dir))
        next
      }
      .update_lock_problem(project_dir, lock_path, status = "symbolic_link")
    }
    if (identical(state$action, "restore_live_discard")) {
      restored <- if (!file.exists(lock_path) && !dir.exists(lock_path) &&
                        !.path_is_symlink(lock_path)) {
        file.rename(state$claim_path, lock_path)
      } else {
        FALSE
      }
      if (isTRUE(restored)) {
        .update_lock_problem(
          project_dir, lock_path, state$old_owner,
          state$old_owner_status
        )
      }
      current <- .read_update_lock_owner(lock_path)
      current_status <- if (is.null(current)) {
        "uncertain"
      } else {
        .update_owner_status(current)
      }
      if (current_status %in% c("alive", "live_token_conflict")) {
        .update_lock_problem(project_dir, lock_path, current, current_status)
      }
      next
    }
    if (identical(state$action, "set_aside_unreadable_lock")) {
      .apart_update_lock(lock_path, basename(project_dir))
      next
    }
    if (identical(state$action, "set_aside_unreadable_discard")) {
      .apart_update_lock(state$claim_path, basename(project_dir))
      next
    }
    if (state$action %in% c("claim_orphan_discard",
                            "claim_uncertain_discard")) {
      .update_lock_inventory_discard(
        state$claim_path, state$old_owner, basename(project_dir),
        owner_digest = state$owner_digest,
        claim = state$owner, claim_digest = state$claim_digest
      )
      next
    }
    if (state$status %in% c("alive", "live_token_conflict") ||
          identical(state$action, "blocked_uncertain_lock") ||
          identical(state$action, "blocked_claim") ||
          identical(state$action, "blocked_live_discard") ||
          identical(state$action, "blocked_uncertain_discard")) {
      owner_for_error <- if (identical(state$action,
                                       "blocked_uncertain_discard")) {
        state$old_owner
      } else {
        state$owner
      }
      status_for_error <- if (identical(state$action,
                                        "blocked_uncertain_discard")) {
        state$old_owner_status
      } else {
        state$status
      }
      .update_lock_problem(
        project_dir, lock_path, owner_for_error, status_for_error
      )
    }
    if (identical(state$status, "free")) {
      temporary <- .update_lock_make_temporary(project_dir, lock_path)
      if (is.null(temporary)) next
      saved <- tryCatch({
        .atomic_save_rds(owner, .update_lock_owner_path(temporary))
        TRUE
      }, error = function(e) FALSE)
      if (!isTRUE(saved)) {
        unlink(temporary, recursive = TRUE, force = TRUE)
        stop(.bb_trf("Could not create temporary directory for %s", lock_path),
             call. = FALSE)
      }
      if (file.rename(temporary, lock_path)) {
        return(list(path = lock_path, owner = owner, acquired = TRUE))
      }
      unlink(temporary, recursive = TRUE, force = TRUE)
      next
    }
    previous <- state$owner
    previous_digest <- state$owner_digest
    discard <- .update_lock_discard_path(lock_path, basename(project_dir))
    current <- .read_update_lock_owner(lock_path)
    current_digest <- .update_lock_record_digest(lock_path, "owner")
    if (!identical(previous, current) ||
          !identical(previous_digest, current_digest)) next
    if (!file.rename(lock_path, discard)) next
    observed <- .read_update_lock_owner(discard)
    observed_digest <- .update_lock_record_digest(discard, "owner")
    if (!isTRUE(identical(previous, observed)) ||
          !isTRUE(identical(previous_digest, observed_digest))) {
      restored <- if (!file.exists(lock_path) && !dir.exists(lock_path)) {
        file.rename(discard, lock_path)
      } else {
        FALSE
      }
      if (!isTRUE(restored)) {
        current <- .read_update_lock_owner(lock_path)
        current_status <- if (is.null(current)) {
          "uncertain"
        } else {
          .update_owner_status(current)
        }
        if (current_status %in% c("alive", "live_token_conflict")) {
          .update_lock_problem(project_dir, lock_path, current, current_status)
        }
      }
      next
    }
    claimed <- tryCatch({
      .atomic_save_rds(owner, .update_lock_claim_path(discard))
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(claimed)) {
      .update_lock_inventory_discard(
        discard, observed, basename(project_dir),
        owner_digest = observed_digest, claim = owner
      )
      next
    }
    claim_digest <- .update_lock_record_digest(discard, "claim")
    temporary <- .update_lock_temporary_path(project_dir)
    if (!dir.create(temporary, showWarnings = FALSE)) {
      .update_lock_inventory_discard(
        discard, observed, basename(project_dir),
        owner_digest = observed_digest, claim = owner,
        claim_digest = claim_digest
      )
      next
    }
    saved <- tryCatch({
      .atomic_save_rds(owner, .update_lock_owner_path(temporary))
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(saved) || !file.rename(temporary, lock_path)) {
      unlink(temporary, recursive = TRUE, force = TRUE)
      .update_lock_inventory_discard(
        discard, observed, basename(project_dir),
        owner_digest = observed_digest, claim = owner,
        claim_digest = claim_digest
      )
      next
    }
    discard_error <- tryCatch({
      .update_lock_inventory_discard(
        discard, observed, basename(project_dir),
        owner_digest = observed_digest, claim = owner,
        claim_digest = claim_digest
      )
      NULL
    }, error = identity)
    if (!is.null(discard_error) &&
          !identical(.read_update_lock_owner(lock_path), owner)) {
      stop(discard_error)
    }
    return(list(path = lock_path, owner = owner, acquired = TRUE))
  }
}

.release_update_lock <- function(lock) {
  if (is.null(lock) || !dir.exists(lock$path)) return(invisible(NULL))
  owner <- .read_update_lock_owner(lock$path)
  if (isTRUE(identical(owner, lock$owner))) {
    unlink(lock$path, recursive = TRUE, force = TRUE)
  }
  invisible(NULL)
}

.unarmed_journal_is_expected <- function(journal_path, marker) {
  entries <- .update_journal_entries(journal_path)
  if (any(vapply(file.path(journal_path, entries), .path_is_symlink,
                 logical(1L)))) return(FALSE)
  entry_is_dir <- dir.exists(file.path(journal_path, entries))
  backup_files <- file.path("backup", names(marker$backup_hashes))
  allowed_dirs <- unique(c("staging", unlist(lapply(
    backup_files, .journal_directory_chain
  ), use.names = FALSE)))
  allowed <- c("marker.rds", .update_journal_digest_name,
               backup_files, allowed_dirs)
  expected_temporary <- function(entry) {
    if (grepl("^\\.state\\.rds-[[:alnum:]]+$", entry)) return(TRUE)
    if (grepl("^\\.(?:tombstone|tombstone\\.md5)-[[:alnum:]]+$",
              entry, perl = TRUE)) return(TRUE)
    if (!startsWith(entry, "backup/")) return(FALSE)
    relative <- substring(entry, nchar("backup/") + 1L)
    directory <- dirname(relative)
    filename <- basename(relative)
    candidates <- names(marker$backup_hashes)
    same_directory <- dirname(candidates) == directory
    any(same_directory & vapply(basename(candidates), function(expected) {
      grepl(paste0("^\\.", .escape_regex_literal(expected),
                   "-[[:alnum:]]+$"), filename, perl = TRUE)
    }, logical(1L)))
  }
  unexpected <- setdiff(entries, allowed)
  unexpected_dirs <- entry_is_dir[match(unexpected, entries)]
  unexpected <- unexpected[unexpected_dirs | !vapply(
    unexpected, expected_temporary, logical(1L)
  )]
  if (length(unexpected) > 0L) return(FALSE)
  backed <- intersect(entries[!entry_is_dir], backup_files)
  if (length(backed) == 0L) return(TRUE)
  relative <- substring(backed, nchar("backup/") + 1L)
  actual <- vapply(file.path(journal_path, backed), .file_digest, character(1L))
  identical(unname(actual), unname(marker$backup_hashes[relative]))
}

.armed_journal_is_expected <- function(journal_path, marker) {
  entries <- .update_journal_entries(journal_path)
  full <- file.path(journal_path, entries)
  if (any(vapply(full, .path_is_symlink, logical(1L)))) return(FALSE)
  is_dir <- dir.exists(full)
  backup_files <- file.path("backup", names(marker$backup_hashes))
  allowed_dirs <- unique(c("staging", unlist(lapply(
    backup_files, .journal_directory_chain
  ), use.names = FALSE)))
  allowed_files <- c(
    "marker.rds", "state.rds", "intent.log",
    .update_journal_digest_name, backup_files
  )
  unexpected_dirs <- entries[is_dir & !entries %in% allowed_dirs]
  files <- entries[!is_dir]
  unexpected_files <- setdiff(files, allowed_files)
  temporary_names <- c(basename(marker$planned_files), "intent.log")
  temporary_pattern <- paste0(
    "^staging/\\.(?:",
    paste(vapply(temporary_names, .escape_regex_literal, character(1L)),
          collapse = "|"),
    ")-[[:alnum:]]+$"
  )
  unexpected_files <- unexpected_files[!grepl(
    temporary_pattern, unexpected_files, perl = TRUE
  )]
  unexpected_files <- unexpected_files[!grepl(
    "^\\.(?:tombstone|tombstone\\.md5)-[[:alnum:]]+$",
    unexpected_files, perl = TRUE
  )]
  if (length(unexpected_dirs) > 0L || length(unexpected_files) > 0L) {
    return(FALSE)
  }
  backups <- file.path(journal_path, backup_files)
  if (!all(file.exists(backups) & !dir.exists(backups))) return(FALSE)
  actual <- vapply(backups, .file_digest, character(1L))
  identical(unname(actual), unname(marker$backup_hashes))
}

.unknown_update_paths <- function(project_dir, state, intents) {
  original <- state$original_hashes
  scope <- unique(c(names(original), intents$path))
  unknown <- vapply(scope, function(relative) {
    path <- file.path(project_dir, relative)
    if (dir.exists(path)) return(TRUE)
    current <- .file_digest(path)
    intended <- intents$hash[intents$path == relative &
                               intents$operation == "write"]
    deleting <- any(intents$path == relative & intents$operation == "delete")
    if (relative %in% names(original)) {
      if (identical(current, unname(original[[relative]]))) return(FALSE)
      if (!is.na(current) && current %in% intended) return(FALSE)
      # On Windows rename() cannot replace an existing file atomically. If the
      # process dies after removing the destination but before the rename, the
      # intended temporary is still in the journal staging area. An absence is
      # known only when that temporary has the intended digest; otherwise it is
      # an unknown user or external state.
      if (is.na(current) &&
            .staged_update_write_matches(project_dir, relative, intended)) {
        return(FALSE)
      }
      if (is.na(current) && deleting) return(FALSE)
      return(TRUE)
    }
    if (is.na(current) || (!is.na(current) && current %in% intended)) FALSE else TRUE
  }, logical(1L))
  scope[unknown]
}

.preserve_unknown_update_paths <- function(project_dir, name, paths) {
  if (length(paths) == 0L) return(NULL)
  stamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
  base <- file.path(dirname(project_dir), paste0(
    ".", name, ".bigbang-preserved-", stamp, "-", Sys.getpid()
  ))
  preserved <- base
  suffix <- 0L
  while (file.exists(preserved) || dir.exists(preserved)) {
    suffix <- suffix + 1L
    preserved <- paste0(base, "-", suffix)
  }
  if (!dir.create(preserved)) {
    stop(.bb_trf("Could not create temporary directory for %s", preserved),
         call. = FALSE)
  }
  for (relative in paths) {
    source <- file.path(project_dir, relative)
    if (!file.exists(source) && !dir.exists(source)) next
    destination <- file.path(preserved, relative)
    if (!dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE) &&
          !dir.exists(dirname(destination))) {
      stop(.bb_trf("Could not create temporary directory for %s", destination),
           call. = FALSE)
    }
    copied <- if (dir.exists(source)) {
      file.rename(source, destination)
    } else {
      file.copy(source, destination, overwrite = FALSE)
    }
    if (!copied) {
      stop(.bb_trf("Could not back up generated file: %s", source), call. = FALSE)
    }
  }
  preserved
}

.restore_from_update_journal <- function(project_dir, name, journal, state,
                                         intents, unknown = character(),
                                         preserve_unknown = FALSE) {
  preserved <- if (preserve_unknown) {
    .preserve_unknown_update_paths(project_dir, name, unknown)
  } else {
    NULL
  }
  .activate_update_journal(journal, project_dir, name, record = FALSE)
  on.exit(.deactivate_update_journal(), add = TRUE)
  original <- state$original_hashes
  absent_original <- names(original)[vapply(
    file.path(project_dir, names(original)),
    function(path) !file.exists(path) && !dir.exists(path), logical(1L)
  )]
  for (relative in names(original)) {
    destination <- file.path(project_dir, relative)
    if (!dir.exists(dirname(destination)) &&
          !dir.create(dirname(destination), recursive = TRUE)) {
      stop(.bb_trf("Could not create temporary directory for %s", destination),
           call. = FALSE)
    }
    .atomic_copy(
      file.path(journal$path, "backup", relative),
      destination
    )
  }
  created <- setdiff(unique(intents$path), names(original))
  for (relative in created) {
    path <- file.path(project_dir, relative)
    if (!file.exists(path) && !dir.exists(path)) next
    digest <- .file_digest(path)
    intended <- intents$hash[intents$path == relative &
                               intents$operation == "write"]
    if (!preserve_unknown && (is.na(digest) || !digest %in% intended)) next
    if (dir.exists(path) || .stale_unlink(path) != 0L) {
      stop(.bb_trf("Could not remove completely: %s", path), call. = FALSE)
    }
  }
  restored <- vapply(file.path(project_dir, names(original)),
                     .file_digest, character(1L))
  remaining <- created[file.exists(file.path(project_dir, created)) |
                         dir.exists(file.path(project_dir, created))]
  if (!identical(unname(restored), unname(original)) || length(remaining) > 0L) {
    stop(.bb_trf("Could not restore generated files after a failed update: %s",
                 paste(c(names(original)[restored != original], remaining),
                       collapse = ", ")), call. = FALSE)
  }
  .discard_update_journal(journal, project_dir, name)
  list(recovered = TRUE, preserved = preserved,
       restored_absent = absent_original)
}

.update_journal_owner_problem <- function(project_dir, journal_path, kind,
                                          owner, status) {
  detail <- if (status %in% c("alive", "live_token_conflict") &&
                  is.list(owner) && length(owner$pid) == 1L) {
    .bb_trf("An update is still running under pid %s; no mutation was performed.",
            owner$pid)
  } else {
    .bb_tr("An update may still be running; no mutation was performed.")
  }
  .update_journal_problem(
    "bigbang_error_update_in_progress", project_dir, journal_path, kind,
    detail, has_backup = TRUE,
    next_step = .bb_tr(
      "after confirming that no other update is running, call again with recover = TRUE."
    )
  )
}

.recover_pending_update <- function(project_dir, name, recover = FALSE,
                                    dry_run = FALSE, handled = FALSE) {
  journal_path <- .update_journal_path(project_dir)
  if (!file.exists(journal_path) && !dir.exists(journal_path)) {
    if (!dir.exists(project_dir)) {
      .bigbang_abort(
        c("bigbang_error_missing_project", "bigbang_error_missing_manifest"),
        .bb_trf(
          paste0(
            "Cannot update %s because the project directory is missing. It may",
            " have been moved without its update journal. Next step: find the",
            " moved project and its sibling journal, then retry with dest_dir",
            " pointing to that location."
          ),
          project_dir
        ),
        path = project_dir, journal = journal_path
      )
    }
    return(list(pending = FALSE, recovered = FALSE, preserved = NULL,
                restored_absent = character()))
  }
  marker <- .read_update_marker(journal_path, project_dir, name)
  moved_journal <- !identical(marker$name, name)
  if (!dir.exists(project_dir)) {
    .update_journal_problem(
      "bigbang_error_moved_update_journal", project_dir, journal_path,
      "armed",
      .bb_tr("The project directory is missing, so this journal was left behind when the project was moved."),
      has_backup = TRUE,
      next_step = .bb_tr(
        "find the moved project, move this journal beside it without changing its contents, and retry there."
      )
    )
  }
  if (isTRUE(attr(marker, "bigbang_copy_journal"))) {
    message(.bb_trf(
      paste0(
        "Did not adopt update journal %s because project %s still exists beside it; ",
        "it is a copy, so the journal was left untouched."
      ),
      journal_path, marker$name
    ))
    return(list(pending = FALSE, recovered = FALSE, preserved = NULL,
                action = "ignored_copy", ignored_journal = journal_path,
                restored_absent = character()))
  }
  journal <- list(path = journal_path, marker = marker)
  apart <- character()
  tombstone_temporary <- .update_journal_temps(journal_path)
  if (length(tombstone_temporary) > 0L) {
    owner <- .update_marker_owner(marker)
    owner_status <- .update_owner_status(owner)
    if (owner_status %in% c("alive", "live_token_conflict") ||
          (identical(owner_status, "uncertain") && !isTRUE(recover))) {
      .update_journal_owner_problem(
        project_dir, journal_path, "armed", owner, owner_status
      )
    }
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "apart_tombstone_temporary",
                  apart = file.path(journal_path, tombstone_temporary),
                  restored_absent = character()))
    }
    for (temporary in tombstone_temporary) {
      apart <- c(
        apart,
        .apart_update_journal(
          file.path(journal_path, temporary), name, project_dir
        )
      )
    }
  }
  tombstone <- .read_update_tombstone(journal_path)
  if (!is.null(tombstone) &&
        !.update_journal_digest_valid(journal_path)) {
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "apart_unauthenticated_tombstone",
                  apart = journal_path, restored_absent = character()))
    }
    apart <- c(apart, .apart_update_journal(journal_path, name))
    return(list(pending = FALSE, recovered = FALSE, preserved = NULL,
                action = "apart_unauthenticated_tombstone", apart = apart,
                restored_absent = character()))
  }
  if (!is.null(tombstone) &&
        identical(tombstone$manifest_hash,
                  unname(marker$backup_hashes[[.generation_manifest_name]]))) {
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "discard_in_progress", restored_absent = character()))
    }
    .discard_tombstoned_journal(
      journal_path, project_dir, name, tombstone
    )
    message(.bb_trf(
      "Completed the interrupted discard at %s.", journal_path
    ))
    return(list(pending = FALSE, recovered = TRUE, preserved = NULL,
                action = "discarded_in_progress", apart = apart,
                restored_absent = character()))
  }
  state_path <- file.path(journal_path, "state.rds")
  state <- if (file.exists(state_path) && !dir.exists(state_path)) {
    tryCatch(readRDS(state_path), error = function(e) NULL)
  } else {
    NULL
  }
  armed_entries <- if (is.list(state) && isTRUE(state$armed)) {
    .update_journal_entries(journal_path)
  } else {
    character()
  }
  armed_expected <- if (is.list(state) && isTRUE(state$armed)) {
    isTRUE(tryCatch(
      .armed_journal_is_expected(journal_path, marker),
      error = function(e) FALSE
    ))
  } else {
    TRUE
  }
  armed_unverifiable <- is.list(state) && isTRUE(state$armed) &&
    !armed_expected &&
    (.journal_entries_unreadable(armed_entries) ||
       any(!vapply(armed_entries, .valid_update_relative_path, logical(1L))))
  if (armed_unverifiable &&
        isTRUE(recover) && identical(.update_owner_status(state), "dead")) {
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "apart_unrecognized_armed",
                  apart = .update_journal_apart_path(
                    journal_path, name, project_dir
                  ),
                  restored_absent = character()))
    }
    apart <- c(apart, .apart_update_journal(journal_path, name, project_dir))
    return(list(pending = FALSE, recovered = FALSE, preserved = NULL,
                action = "apart_unrecognized_armed", apart = apart,
                restored_absent = character()))
  }
  if (is.null(state) || !isTRUE(state$armed)) {
    if (!.unarmed_journal_is_expected(journal_path, marker)) {
      .update_journal_problem(
        "bigbang_error_unrecognized_update_journal", project_dir, journal_path,
        "unarmed",
        .bb_tr("It contains unexpected files; nothing was deleted."),
        has_backup = length(marker$backup_hashes) > 0L,
        next_step = .bb_tr(
          "leave the folder intact, preserve the backup, and inspect the unexpected entries before retrying."
        )
      )
    }
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "discard_unarmed", restored_absent = character()))
    }
    .discard_update_journal(journal, project_dir, name)
    message(.bb_trf("Discarded an unarmed update journal at %s.", journal_path))
    return(list(pending = FALSE, recovered = FALSE, preserved = NULL,
                action = "discarded_unarmed", restored_absent = character()))
  }
  required <- c("pid", "host", "started_utc", "process_start", "bigbang_version",
                "original_hashes", "old_manifest_hash")
  valid_state <- all(required %in% names(state)) &&
    (is.integer(state$pid) || is.numeric(state$pid)) &&
    length(state$pid) == 1L && !is.na(state$pid) && state$pid > 0 &&
    is.character(state$host) && length(state$host) == 1L &&
    is.character(state$started_utc) && length(state$started_utc) == 1L &&
    is.character(state$process_start) && length(state$process_start) == 1L &&
    (!"process_start_source" %in% names(state) ||
       (is.character(state$process_start_source) &&
          length(state$process_start_source) == 1L)) &&
    is.character(state$bigbang_version) &&
    length(state$bigbang_version) == 1L &&
    is.character(state$original_hashes) &&
    !is.null(names(state$original_hashes)) &&
    is.character(state$old_manifest_hash) &&
    length(state$old_manifest_hash) == 1L &&
    grepl("^[0-9a-f]{32}$", state$old_manifest_hash)
  marker_owner <- .update_marker_owner(marker)
  state_owner <- .update_marker_owner(state)
  if (isTRUE(valid_state) && !identical(state_owner, marker_owner)) {
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "apart_owner_mismatch", apart = journal_path,
                  restored_absent = character()))
    }
    apart <- c(apart, .apart_update_journal(journal_path, name, project_dir))
    return(list(pending = FALSE, recovered = FALSE, preserved = NULL,
                action = "apart_owner_mismatch", apart = apart,
                restored_absent = character()))
  }
  if (!isTRUE(valid_state) ||
        !identical(state$original_hashes, marker$backup_hashes) ||
        !identical(state$old_manifest_hash,
                   unname(marker$backup_hashes[[.generation_manifest_name]]))) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", project_dir, journal_path,
      "armed",
      .bb_tr("Its armed state does not match the backup; nothing was deleted."),
      has_backup = TRUE,
      next_step = .bb_tr("leave the folder intact, preserve the backup, and inspect the state before retrying.")
    )
  }
  if (!.armed_journal_is_expected(journal_path, marker)) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", project_dir, journal_path,
      "armed",
      .bb_tr("It contains unexpected files; nothing was deleted."),
      has_backup = TRUE,
      next_step = .bb_tr(
        "leave the folder intact, preserve the backup, and inspect the unexpected entries before retrying."
      )
    )
  }
  intents <- .read_update_intents(journal_path, project_dir)
  if (any(!intents$path %in% marker$planned_files)) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", project_dir, journal_path,
      "armed",
      .bb_tr("Its intention log is invalid; nothing was deleted."),
      has_backup = TRUE,
      next_step = .bb_tr("leave the folder intact, preserve the backup, and inspect the intention log before retrying.")
    )
  }
  current_manifest <- .file_digest(file.path(project_dir, .generation_manifest_name))
  intended_manifests <- intents$hash[
    intents$operation == "write" &
      intents$path == .generation_manifest_name
  ]
  completed <- !is.na(current_manifest) &&
    !identical(current_manifest, state$old_manifest_hash) &&
    current_manifest %in% intended_manifests &&
    .manifest_matches_project(project_dir)
  if (completed) {
    if (dry_run) {
      return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                  action = "discard_completed", restored_absent = character()))
    }
    .discard_update_journal(journal, project_dir, name)
    message(.bb_trf(
      "The previous update had completed; discarded its journal at %s.",
      journal_path
    ))
    return(list(pending = FALSE, recovered = TRUE, preserved = NULL,
                action = "recognized_completed", restored_absent = character()))
  }
  owner_status <- .update_owner_status(state)
  if (!handled && !moved_journal &&
        (owner_status %in% c("alive", "live_token_conflict") ||
           (identical(owner_status, "uncertain") && !isTRUE(recover)))) {
    .update_journal_owner_problem(
      project_dir, journal_path, "armed", state, owner_status
    )
  }
  .validate_project_write_paths(
    project_dir, unique(c(names(state$original_hashes), intents$path))
  )
  unknown <- .unknown_update_paths(project_dir, state, intents)
  if (dry_run) {
    action <- if (length(unknown) > 0L && !recover) {
      "blocked_unknown"
    } else if (length(unknown) > 0L) {
      "preserve_and_recover"
    } else {
      "recover"
    }
    return(list(pending = TRUE, recovered = FALSE, preserved = NULL,
                action = action, unknown = unknown,
                restored_absent = character()))
  }
  if (length(unknown) > 0L && !recover && !handled) {
    .bigbang_abort(
      "bigbang_error_interrupted_update",
      .bb_trf(
        paste0(
          "The interrupted update left files in an unknown state: %s. ",
          "Journal: %s. The journal contains a backup at %s. Use recover = TRUE ",
          "to preserve them and recover."
        ),
        paste(unknown, collapse = ", "), journal_path,
        .update_journal_backup_path(journal_path)
      ),
      path = project_dir, journal = journal_path, files = unknown
    )
  }
  result <- .restore_from_update_journal(
    project_dir, name, journal, state, intents, unknown,
    preserve_unknown = length(unknown) > 0L
  )
  if (!is.null(result$preserved)) {
    message(.bb_trf("Preserved unknown files from the interrupted update at %s.",
                    result$preserved))
  }
  if (length(result$restored_absent) > 0L) {
    message(.bb_trf(
      "Restored files that were absent when recovery started: %s.",
      paste(result$restored_absent, collapse = ", ")
    ))
  }
  message(.bb_trf("Recovered an interrupted update using %s.", journal_path))
  c(list(pending = FALSE, action = "recovered", apart = apart), result)
}
