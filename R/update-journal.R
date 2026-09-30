.update_journal_format <- "bigbang-update-journal"
.update_journal_version <- 1L
.update_journal_tombstone_name <- "tombstone.rds"
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
  sort(siblings)
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

.update_journal_inventory <- function(path) {
  entries <- list.files(path, all.files = TRUE, recursive = TRUE,
                        no.. = TRUE, include.dirs = TRUE)
  entries <- gsub("\\\\", "/", entries)
  full <- file.path(path, entries)
  is_dir <- dir.exists(full)
  files <- setdiff(entries[!is_dir], .update_journal_tombstone_name)
  if (length(files) > 0L && any(vapply(
    file.path(path, files), .path_is_symlink, logical(1L)
  ))) {
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
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(tombstone, temporary)
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
  tombstone[fields]
}

.discard_update_entry <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
}

.update_journal_after_rename <- function(path) {
  invisible(path)
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
      .update_journal_problem(
        "bigbang_error_unrecognized_update_journal", project_dir, path,
        "discarded",
        .bb_trf(
          paste0(
            "Its tombstone belongs to project '%s' with manifest hash %s, ",
            "not project '%s' with manifest hash %s; nothing was deleted."
          ),
          tombstone$name, tombstone$manifest_hash, name,
          if (is.na(current_manifest)) "missing" else current_manifest
        ),
        has_backup = dir.exists(.update_journal_backup_path(path)),
        next_step = .bb_tr(
          "leave the folder intact, preserve the backup, and move it back beside its project before retrying."
        )
      )
    }
  }
  entries <- list.files(path, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
  entries <- gsub("\\\\", "/", entries)
  full <- file.path(path, entries)
  is_dir <- dir.exists(full)
  files <- setdiff(entries[!is_dir], .update_journal_tombstone_name)
  expected_files <- tombstone$inventory
  expected_dirs <- tombstone$inventory_directories
  unexpected_files <- files[!files %in% names(expected_files)]
  matching_files <- intersect(files, names(expected_files))
  if (length(matching_files) > 0L) {
    actual <- vapply(file.path(path, matching_files), .file_digest, character(1L))
    unexpected_files <- c(
      unexpected_files,
      matching_files[is.na(actual) | actual != unname(expected_files[matching_files])]
    )
  }
  unexpected_dirs <- setdiff(entries[is_dir], expected_dirs)
  symlinks <- entries[vapply(full, .path_is_symlink, logical(1L))]
  unexpected <- unique(c(unexpected_files, unexpected_dirs, symlinks))
  if (length(unexpected) > 0L) {
    .update_journal_problem(
      "bigbang_error_unrecognized_update_journal", path, path, "discarded",
      .bb_trf(
        "It contains entries outside its discard inventory; nothing was deleted. Preserved: %s.",
        paste(sort(unexpected), collapse = ", ")
      ),
      has_backup = dir.exists(.update_journal_backup_path(path)),
      next_step = .bb_tr(
        "leave the folder intact, preserve the backup, remove only the reported user entries, and retry."
      )
    )
  }
  for (entry in matching_files) {
    .discard_update_entry(file.path(path, entry))
  }
  directories <- sort(setdiff(expected_dirs, .update_journal_tombstone_name),
                      decreasing = TRUE)
  directories <- directories[order(nchar(directories), decreasing = TRUE)]
  for (entry in directories) {
    target <- file.path(path, entry)
    if (dir.exists(target)) {
      .discard_update_entry(target)
      if (dir.exists(target)) {
        stop(.bb_trf("Could not remove completely: %s", path), call. = FALSE)
      }
    }
  }
  remaining <- setdiff(list.files(path, all.files = TRUE, no.. = TRUE,
                                  include.dirs = TRUE),
                       .update_journal_tombstone_name)
  if (length(remaining) > 0L) {
    stop(.bb_trf("Could not remove completely: %s", path), call. = FALSE)
  }
  unlink(.update_journal_tombstone_path(path), force = TRUE)
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

.reconcile_update_siblings <- function(project_dir, name, dry_run = FALSE) {
  plan_environment <- new.env(parent = emptyenv())
  plan_environment$items <- list()
  add_plan <- function(path, action, marker = NULL, tombstone = NULL) {
    plan_environment$items <- c(plan_environment$items, list(list(
      path = path, action = action, marker = marker, tombstone = tombstone
    )))
  }
  block_live_owner <- function(path) {
    .bigbang_abort(
      "bigbang_error_update_in_progress",
      .bb_tr("An update may still be running; no mutation was performed."),
      path = project_dir, journal = path, journal_kind = "armed",
      recover = TRUE
    )
  }

  armando <- .update_journal_sibling_paths(project_dir, name, "armando")
  for (path in armando) {
    if (!dir.exists(path) || .path_is_symlink(path)) {
      .update_journal_problem(
        "bigbang_error_unrecognized_update_journal", project_dir, path,
        "unknown",
        .bb_tr("It is not a directory with a valid marker."), FALSE,
        .bb_tr("leave it intact, inspect it, and remove or move it only after preserving its contents.")
      )
    }
    marker_path <- file.path(path, "marker.rds")
    entries <- list.files(path, all.files = TRUE, recursive = TRUE,
                          no.. = TRUE, include.dirs = TRUE)
    if (!file.exists(marker_path)) {
      if (length(entries) == 0L) {
        add_plan(path, "discard_empty")
        next
      }
      if (all(
        !dir.exists(file.path(path, entries)) &&
          grepl("^\\.marker\\.rds-[[:alnum:]]+$", entries)
      )) {
        add_plan(path, "discard_marker_temporary")
        next
      }
      .update_journal_problem(
        "bigbang_error_unrecognized_update_journal", project_dir, path,
        "unarmed",
        .bb_tr("It has no marker but is not empty; nothing was deleted."), FALSE,
        .bb_tr("leave it intact, preserve its contents, and inspect the failed preparation before retrying.")
      )
    }
    marker <- tryCatch(.read_update_marker(path, project_dir, name),
                       error = identity)
    if (inherits(marker, "error")) stop(marker)
    if (.update_owner_may_be_alive(.update_marker_owner(marker))) {
      add_plan(path, "blocked_live_owner", marker = marker)
    } else {
      add_plan(path, "discard_armed", marker = marker)
    }
  }

  discarded <- .update_journal_sibling_paths(project_dir, name, "descartado")
  for (path in discarded) {
    if (!dir.exists(path) || .path_is_symlink(path)) {
      .update_journal_problem(
        "bigbang_error_unrecognized_update_journal", project_dir, path,
        "unknown",
        .bb_tr("It is not a directory with a valid tombstone."), FALSE,
        .bb_tr("leave it intact, preserve its contents, and inspect it before retrying.")
      )
    }
    tombstone <- .read_update_tombstone(path)
    if (is.null(tombstone)) {
      if (length(list.files(path, all.files = TRUE, no.. = TRUE,
                            include.dirs = TRUE)) == 0L) {
        add_plan(path, "discard_empty")
        next
      }
      .update_journal_problem(
        "bigbang_error_unrecognized_update_journal", project_dir, path,
        "unknown",
        .bb_tr("It has no valid tombstone; nothing was deleted."), FALSE,
        .bb_tr("leave the folder intact, preserve its contents, and inspect it before retrying.")
      )
    }
    marker <- tryCatch(.read_update_marker(path, project_dir, name),
                       error = identity)
    if (inherits(marker, "error")) {
      owner <- .update_tombstone_owner(tombstone)
      if (is.null(owner)) stop(marker)
      marker <- owner
    }
    if (.update_owner_may_be_alive(.update_marker_owner(marker))) {
      add_plan(path, "blocked_live_owner", marker = marker,
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
  for (item in plan) {
    if (identical(item$action, "blocked_live_owner")) {
      block_live_owner(item$path)
    } else if (identical(item$action, "discard_empty") ||
                 identical(item$action, "discard_marker_temporary")) {
      unlink(item$path, recursive = TRUE, force = TRUE)
    } else if (identical(item$action, "discard_armed")) {
      if (.update_owner_may_be_alive(.update_marker_owner(item$marker))) {
        block_live_owner(item$path)
      }
      .discard_update_journal(item$path, project_dir, name)
      message(.bb_trf("Discarded an orphaned armed-update folder at %s.",
                      item$path))
    } else if (identical(item$action, "finish_discarded")) {
      if (.update_owner_may_be_alive(.update_marker_owner(item$marker))) {
        block_live_owner(item$path)
      }
      .finish_discarded_journal(
        item$path, name, item$tombstone, project_dir = project_dir,
        verify_project = TRUE
      )
    }
  }
  invisible(plan)
}

.update_process_start <- function(pid = Sys.getpid()) {
  if (.Platform$OS.type == "windows" || !dir.exists("/proc")) return(NA_character_)
  stat <- tryCatch(
    readLines(sprintf("/proc/%d/stat", as.integer(pid)), warn = FALSE, n = 1L),
    error = function(e) character()
  )
  if (length(stat) != 1L) return(NA_character_)
  closing <- regexpr("\\)[^)]*$", stat, perl = TRUE)
  if (closing[[1L]] < 1L) return(NA_character_)
  fields <- strsplit(trimws(substring(stat, closing[[1L]] + 1L)),
                     "[[:space:]]+", perl = TRUE)[[1L]]
  if (length(fields) < 20L) NA_character_ else fields[[20L]]
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
    process_start = .update_process_start()
  )
}

.update_marker_owner <- function(marker) {
  fields <- c("pid", "host", "started_utc", "process_start")
  if (!is.list(marker) || !all(fields %in% names(marker))) return(NULL)
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
          "project '%s'. Rename the project and journal back, or rename the ",
          "journal to match the project."
        ),
        project_dir, journal_path, marker_name, name
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
      is.character(marker$process_start) && length(marker$process_start) == 1L
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
                                   extra_files = character()) {
  journal_path <- .update_journal_path(project_dir)
  if (file.exists(journal_path) || dir.exists(journal_path)) {
    .read_update_marker(journal_path, project_dir, name)
    stop("Internal error: an update journal is already present", call. = FALSE)
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
  .atomic_save_rds(state, file.path(staging_path, "state.rds"))
  if (file.exists(journal_path) || dir.exists(journal_path)) {
    stop(.bb_trf("Could not arm the update journal because its final path already exists: %s",
                 journal_path), call. = FALSE)
  }
  if (!file.rename(staging_path, journal_path)) {
    stop(.bb_trf("Could not arm the update journal at %s", journal_path),
         call. = FALSE)
  }
  list(path = journal_path, marker = marker, state = state)
}

.activate_update_journal <- function(journal, project_dir, name,
                                     record = TRUE) {
  .update_journal_runtime$current <- list(
    path = journal$path,
    project = normalizePath(project_dir, winslash = "/", mustWork = TRUE),
    name = name,
    record = isTRUE(record)
  )
  invisible(NULL)
}

.deactivate_update_journal <- function() {
  .update_journal_runtime$current <- NULL
  invisible(NULL)
}

.project_relative_destination <- function(destination, project_dir) {
  destination <- normalizePath(destination, winslash = "/", mustWork = FALSE)
  prefix <- paste0(project_dir, "/")
  if (!startsWith(destination, prefix)) return(NULL)
  relative <- substring(destination, nchar(prefix) + 1L)
  if (!.valid_update_relative_path(relative)) return(NULL)
  relative
}

.append_update_intent <- function(operation, relative, hash = "-") {
  context <- .update_journal_runtime$current
  if (is.null(context) || !isTRUE(context$record)) return(invisible(NULL))
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

.update_owner_may_be_alive <- function(state) {
  if (!is.list(state) || length(state$pid) != 1L ||
        is.na(state$pid) || length(state$host) != 1L ||
        !is.character(state$host) || is.na(state$host) ||
        length(.update_host()) != 1L || is.na(.update_host())) return(TRUE)
  if (!identical(state$host, .update_host())) return(TRUE)
  if (.Platform$OS.type == "windows") return(TRUE)
  alive <- isTRUE(tryCatch(tools::pskill(as.integer(state$pid), 0L),
                           error = function(e) FALSE))
  if (!alive) return(FALSE)
  recorded <- state$process_start
  current <- .update_process_start(state$pid)
  if (!is.null(recorded) && length(recorded) == 1L && !is.na(recorded) &&
        !is.na(current) && !identical(recorded, current)) return(FALSE)
  TRUE
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
        length(owner$process_start) != 1L) return(NULL)
  owner
}

.update_lock_problem <- function(project_dir, lock_path) {
  .bigbang_abort(
    "bigbang_error_update_in_progress",
    paste(
      .bb_tr("An update may still be running; no mutation was performed."),
      .bb_tr("after confirming that no other update is running, call again with recover = TRUE."),
      sep = " "
    ),
    path = project_dir, lock = lock_path, recover = TRUE
  )
}

.acquire_update_lock <- function(project_dir) {
  lock_path <- .update_lock_path(project_dir)
  owner <- .update_owner_record()
  for (attempt in seq_len(4L)) {
    if (file.exists(lock_path) && !dir.exists(lock_path)) {
      .update_lock_problem(project_dir, lock_path)
    }
    if (dir.create(lock_path, showWarnings = FALSE)) {
      saved <- tryCatch({
        saveRDS(owner, .update_lock_owner_path(lock_path))
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(saved)) return(list(path = lock_path, owner = owner))
      unlink(lock_path, recursive = TRUE, force = TRUE)
      stop(.bb_trf("Could not create temporary directory for %s", lock_path),
           call. = FALSE)
    }
    if (.path_is_symlink(lock_path)) .update_lock_problem(project_dir, lock_path)
    previous <- .read_update_lock_owner(lock_path)
    if (is.null(previous) || .update_owner_may_be_alive(previous)) {
      .update_lock_problem(project_dir, lock_path)
    }
    unlink(lock_path, recursive = TRUE, force = TRUE)
    if (file.exists(lock_path) || dir.exists(lock_path)) {
      .update_lock_problem(project_dir, lock_path)
    }
  }
  .update_lock_problem(project_dir, lock_path)
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
  entries <- list.files(journal_path, all.files = TRUE, recursive = TRUE,
                        no.. = TRUE, include.dirs = TRUE)
  if (any(vapply(file.path(journal_path, entries), .path_is_symlink,
                 logical(1L)))) return(FALSE)
  entry_is_dir <- dir.exists(file.path(journal_path, entries))
  backup_files <- file.path("backup", names(marker$backup_hashes))
  directory_chain <- function(path) {
    out <- character()
    current <- dirname(path)
    while (!identical(current, ".") && nzchar(current)) {
      out <- c(out, current)
      current <- dirname(current)
    }
    out
  }
  allowed_dirs <- unique(c("staging", unlist(lapply(
    backup_files, directory_chain
  ), use.names = FALSE)))
  allowed <- c("marker.rds", backup_files, allowed_dirs)
  expected_temporary <- function(entry) {
    if (grepl("^\\.state\\.rds-[[:alnum:]]+$", entry)) return(TRUE)
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
  entries <- list.files(journal_path, all.files = TRUE, recursive = TRUE,
                        no.. = TRUE, include.dirs = TRUE)
  full <- file.path(journal_path, entries)
  if (any(vapply(full, .path_is_symlink, logical(1L)))) return(FALSE)
  is_dir <- dir.exists(full)
  backup_files <- file.path("backup", names(marker$backup_hashes))
  directory_chain <- function(path) {
    out <- character()
    current <- dirname(path)
    while (!identical(current, ".") && nzchar(current)) {
      out <- c(out, current)
      current <- dirname(current)
    }
    out
  }
  allowed_dirs <- unique(c("staging", unlist(lapply(
    backup_files, directory_chain
  ), use.names = FALSE)))
  allowed_files <- c("marker.rds", "state.rds", "intent.log", backup_files)
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
  journal <- list(path = journal_path, marker = marker)
  tombstone <- .read_update_tombstone(journal_path, name)
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
                action = "discarded_in_progress", restored_absent = character()))
  }
  state_path <- file.path(journal_path, "state.rds")
  state <- if (file.exists(state_path) && !dir.exists(state_path)) {
    tryCatch(readRDS(state_path), error = function(e) NULL)
  } else {
    NULL
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
    is.character(state$bigbang_version) &&
    length(state$bigbang_version) == 1L &&
    is.character(state$original_hashes) &&
    !is.null(names(state$original_hashes)) &&
    is.character(state$old_manifest_hash) &&
    length(state$old_manifest_hash) == 1L &&
    grepl("^[0-9a-f]{32}$", state$old_manifest_hash)
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
  if (!handled && !recover && .update_owner_may_be_alive(state)) {
    .update_journal_problem(
      "bigbang_error_update_in_progress", project_dir, journal_path, "armed",
      .bb_tr("An update may still be running; no mutation was performed."),
      has_backup = TRUE,
      next_step = .bb_tr("after confirming that no other update is running, call again with recover = TRUE.")
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
  c(list(pending = FALSE, action = "recovered"), result)
}
