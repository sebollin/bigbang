# Archive inspection and extraction share the same stateful walk. Tar reads its
# streaming headers and ZIP reads its central directory plus local payloads;
# both can run with a null destination or with a fresh empty destination.

.archive_is_zip <- function(ext) identical(tolower(ext), ".zip")

.archive_tar_extensions <- c(
  ".tar", ".tar.gz", ".tgz", ".tar.bz2", ".tbz", ".tbz2", ".tar.xz", ".txz"
)

.archive_is_tar <- function(ext) tolower(ext) %in% .archive_tar_extensions

.archive_abort <- function(class, message, ...) {
  if (exists(".bigbang_abort", mode = "function", inherits = TRUE)) {
    return(.bigbang_abort(class, message, ...))
  }
  condition <- structure(
    c(list(message = message, call = NULL), list(...)),
    class = c(class, "bigbang_error", "error", "condition")
  )
  stop(condition)
}

.archive_parse_error <- function(message, ...) {
  structure(
    c(list(message = message, call = NULL), list(...)),
    class = c("bigbang_archive_parse_error", "error", "condition")
  )
}

.archive_portable_names <- function() {
  name <- "bigbang.archive.portable_names"
  value <- getOption(name, TRUE)
  if (length(value) != 1L || !is.logical(value) || is.na(value)) {
    .archive_abort(
      "bigbang_error_archive_portable_names",
      .bb_trf("Option %s must be TRUE or FALSE.", name),
      option = name, value = value
    )
  }
  value
}

.archive_warning_state <- new.env(parent = emptyenv())
.archive_warning_state$active <- FALSE
.archive_warning_state$seen <- character()

.archive_name_error <- function(name, reason, other = NULL) {
  if (identical(reason, "case")) {
    if (isFALSE(getOption("bigbang.archive.portable_names", TRUE))) {
      message <- .bb_trf(
        "The extraction destination does not distinguish case, so it cannot create both `%s` and `%s`.",
        other, name
      )
    } else {
      hint <- .bb_trf(
        "If all recipients use Linux or macOS, you can use options(bigbang.archive.portable_names = FALSE)."
      )
      message <- .bb_trf(
        "The extraction destination does not distinguish case, so it cannot create both `%s` and `%s`. %s",
        other, name, hint
      )
    }
  } else {
    hint <- .bb_trf(
      "If all recipients use Linux or macOS, you can use options(bigbang.archive.portable_names = FALSE)."
    )
    reason_text <- switch(
      reason,
      trailing = .bb_trf("it ends in a dot or space"),
      invalid_character = .bb_trf("it contains a character invalid on Windows"),
      device = .bb_trf("it is a reserved Windows device name"),
      .bb_trf("it is not a portable Windows name")
    )
    message <- .bb_trf("Windows cannot create `%s`: %s. %s", name, reason_text, hint)
  }
  .archive_parse_error(message, entry = name, reason = reason)
}

.archive_fs_case_sensitive <- function(path, is_windows = NULL,
                                       file_create = NULL, file_exists = NULL) {
  if (is.null(is_windows)) is_windows <- identical(.Platform$OS.type, "windows")
  if (isTRUE(is_windows) || length(path) != 1L || is.na(path) ||
        !dir.exists(path)) {
    return(FALSE)
  }
  token <- paste0(
    basename(tempfile(".bigbang-case-probe-", tmpdir = path)), "-a"
  )
  lower <- file.path(path, token)
  upper <- file.path(path, toupper(token))
  on.exit(unlink(unique(c(lower, upper)), force = TRUE), add = TRUE)
  if (is.null(file_create)) {
    file_create <- function(path) suppressWarnings(file.create(path))
  }
  if (is.null(file_exists)) file_exists <- base::file.exists
  created <- tryCatch(
    file_create(lower), error = function(e) FALSE
  )
  if (!isTRUE(created)) {
    return(FALSE)
  }
  lower_exists <- tryCatch(file_exists(lower), error = function(e) FALSE)
  upper_exists <- tryCatch(file_exists(upper), error = function(e) TRUE)
  isTRUE(lower_exists) && !isTRUE(upper_exists)
}

.archive_limit_options <- function() {
  option <- function(name, default) {
    value <- getOption(name, NULL)
    if (is.null(value)) value <- default
    if (length(value) != 1L || is.na(value) || !is.numeric(value) ||
          !is.finite(value) || value <= 0) {
      .archive_abort(
        "bigbang_error_archive_limit",
        .bb_trf(
          "Archive option %s must be one finite positive number.", name
        ),
        option = name
      )
    }
    as.numeric(value)
  }
  list(
    max_entries = option("bigbang.archive.max_entries", 100000),
    max_total_bytes = option("bigbang.archive.max_total_bytes", 2 * 1024^3),
    max_metadata_bytes = option("bigbang.archive.max_metadata_bytes", 64 * 1024^2),
    max_path_bytes = option("bigbang.archive.max_path_bytes", 4096)
  )
}

.archive_state_new <- function(archive, extract_dir = NULL, is_windows = NULL,
                               case_sensitive = NULL) {
  state <- new.env(parent = emptyenv())
  state$archive <- archive
  state$extract_dir <- extract_dir
  state$portable_names <- .archive_portable_names()
  if (is.null(is_windows)) is_windows <- identical(.Platform$OS.type, "windows")
  state$is_windows <- isTRUE(is_windows)
  state$case_sensitive <- FALSE
  if (!isTRUE(state$portable_names) && !isTRUE(state$is_windows) &&
        !is.null(extract_dir)) {
    state$case_sensitive <- if (is.null(case_sensitive)) {
      .archive_fs_case_sensitive(extract_dir, is_windows = state$is_windows)
    } else {
      isTRUE(case_sensitive)
    }
  }
  state$relax_case <- !isTRUE(state$portable_names) &&
    !isTRUE(state$is_windows) && !is.null(extract_dir) &&
    isTRUE(state$case_sensitive)
  state$defer_case <- !isTRUE(state$portable_names) &&
    !isTRUE(state$is_windows) && is.null(extract_dir)
  state$relaxed_windows <- character()
  state$relaxed_case <- character()
  state$deferred_case <- character()
  state$limits <- .archive_limit_options()
  state$total_bytes <- 0
  state$metadata_bytes <- 0
  state$entry_count <- 0L
  state$member_count <- 0L
  state$members <- new.env(hash = TRUE, parent = emptyenv())
  state$member_lengths <- new.env(hash = TRUE, parent = emptyenv())
  state$member_display <- new.env(hash = TRUE, parent = emptyenv())
  state$counted_paths <- new.env(hash = TRUE, parent = emptyenv())
  state$types <- new.env(hash = TRUE, parent = emptyenv())
  state$entries <- new.env(hash = TRUE, parent = emptyenv())
  state$case_paths <- new.env(hash = TRUE, parent = emptyenv())
  state$directories <- new.env(hash = TRUE, parent = emptyenv())
  state$directory_count <- 0L
  state
}

.archive_limit_abort <- function(state, what, observed, limit, option) {
  .archive_abort(
    "bigbang_error_archive_limit",
    .bb_trf(
      paste0(
        "Archive %s exceeds its configured %s limit (%s > %s); ",
        "raise options('%s') only for trusted input."
      ),
      state$archive, what, format(observed, trim = TRUE),
      format(limit, trim = TRUE), option
    ),
    archive = state$archive, option = option, observed = observed,
    limit = limit
  )
}

.archive_state_reserve <- function(state, size) {
  if (length(size) != 1L || is.na(size) || !is.finite(size) || size < 0) {
    stop(.archive_parse_error("invalid archive member size"))
  }
  total <- state$total_bytes + size
  if (!is.finite(total) || total > state$limits$max_total_bytes) {
    .archive_limit_abort(
      state, "total archive payload bytes", total,
      state$limits$max_total_bytes, "bigbang.archive.max_total_bytes"
    )
  }
  state$total_bytes <- total
  invisible(NULL)
}

.archive_state_reserve_meta <- function(state, size) {
  if (length(size) != 1L || is.na(size) || !is.finite(size) || size < 0) {
    stop(.archive_parse_error("invalid archive metadata size"))
  }
  metadata <- state$metadata_bytes + size
  if (!is.finite(metadata) || metadata > state$limits$max_metadata_bytes) {
    .archive_limit_abort(
      state, "metadata payload bytes", metadata,
      state$limits$max_metadata_bytes, "bigbang.archive.max_metadata_bytes"
    )
  }
  state$metadata_bytes <- metadata
  .archive_state_reserve(state, size)
}

.archive_case_fold_table <- local({
  latin1_upper <- c(seq.int(0x00c0, 0x00d6), seq.int(0x00d8, 0x00de))
  latin1_lower <- latin1_upper + 0x20
  extended_upper <- c(
    seq.int(0x0100, 0x012e, by = 2L),
    seq.int(0x0132, 0x0136, by = 2L),
    seq.int(0x0139, 0x0147, by = 2L),
    0x014aL, 0x014cL, 0x014eL,
    seq.int(0x0150, 0x0176, by = 2L),
    0x0178L,
    seq.int(0x0179, 0x017d, by = 2L)
  )
  extended_lower <- extended_upper + 1L
  extended_lower[extended_upper == 0x0178L] <- 0x00ffL
  greek_upper <- c(
    0x0386L, 0x0388L, 0x0389L, 0x038aL, 0x038cL, 0x038eL, 0x038fL,
    seq.int(0x0391, 0x03a1), seq.int(0x03a3, 0x03ab)
  )
  greek_lower <- c(
    0x03acL, 0x03adL, 0x03aeL, 0x03afL, 0x03ccL, 0x03cdL, 0x03ceL,
    seq.int(0x03b1, 0x03c1), seq.int(0x03c3, 0x03cb)
  )
  upper <- c(
    latin1_upper, extended_upper, 0x0130L,
    greek_upper, 0x03f4L,
    seq.int(0x0400, 0x040f), seq.int(0x0410, 0x042f),
    0x017fL, 0x212aL, 0x03c2L
  )
  lower <- c(
    latin1_lower, extended_lower, 0x0069L,
    greek_lower, 0x03b8L,
    seq.int(0x0450, 0x045f), seq.int(0x0430, 0x044f),
    0x0073L, 0x006bL, 0x03c3L
  )
  # NTFS and APFS fold capital sharp S (U+1E9E) with sharp S (U+00DF).
  # They also fold dotless i (U+0131) with ASCII i/I in case-insensitive names.
  upper <- c(upper, 0x1e9eL, 0x0131L)
  lower <- c(lower, 0x00dfL, 0x0069L)
  stats::setNames(lower, as.character(upper))
})

.archive_case_key <- function(name) {
  value <- chartr(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz",
    enc2utf8(name)
  )
  points <- utf8ToInt(value)
  matches <- match(as.character(points), names(.archive_case_fold_table))
  replace <- !is.na(matches)
  points[replace] <- unname(.archive_case_fold_table[matches[replace]])
  intToUtf8(points)
}

.archive_validate_name_text <- function(name, limits, allow_empty = FALSE,
                                        allow_dots = FALSE,
                                        portable_names = NULL,
                                        is_windows = NULL) {
  if (is.null(portable_names)) portable_names <- .archive_portable_names()
  if (is.null(is_windows)) is_windows <- identical(.Platform$OS.type, "windows")
  if (length(name) != 1L || is.na(name) || !nzchar(name) && !allow_empty ||
        !validUTF8(name)) {
    stop(.archive_parse_error("archive entry path is not valid UTF-8"))
  }
  codepoints <- utf8ToInt(name)
  if (any(codepoints <= 0x1fL | codepoints >= 0x7fL & codepoints <= 0x9fL)) {
    stop(.archive_parse_error("archive entry path contains control characters"))
  }
  if (grepl("\\", name, fixed = TRUE)) {
    stop(.archive_parse_error("archive entry path contains a backslash"))
  }
  if (nchar(enc2utf8(name), type = "bytes") > limits$max_path_bytes) {
    stop(.archive_parse_error("archive entry path is too long"))
  }
  if (startsWith(name, "/") || grepl("^[A-Za-z]:", name, perl = TRUE) ||
        startsWith(name, "//")) {
    stop(.archive_parse_error("archive entry has an absolute path"))
  }
  parts <- strsplit(name, "/", fixed = TRUE)[[1L]]
  if (length(parts) > 0L && identical(utils::tail(parts, 1L)[[1L]], "")) {
    parts <- utils::head(parts, -1L)
  }
  if (length(parts) == 0L || any(!nzchar(parts)) ||
        (!allow_dots && any(parts %in% c(".", "..")))) {
    stop(.archive_parse_error("archive entry path is not portable"))
  }
  if (any(grepl("^[. ]+$", parts, perl = TRUE))) {
    stop(.archive_parse_error("archive entry path is not portable"))
  }
  relaxed <- character()
  if (any(grepl("[. ]$", parts))) {
    if (isTRUE(portable_names) || isTRUE(is_windows)) {
      stop(.archive_name_error(name, "trailing"))
    }
    relaxed <- c(relaxed, "windows")
  }
  if (any(grepl("[<>:\"|?*]", parts, perl = TRUE))) {
    if (isTRUE(portable_names) || isTRUE(is_windows)) {
      stop(.archive_name_error(name, "invalid_character"))
    }
    relaxed <- c(relaxed, "windows")
  }
  device_pattern <- paste0(
    "^(CON|PRN|AUX|NUL|COM(?:[0-9]|\\x{00B9}|\\x{00B2}|\\x{00B3})|",
    "LPT(?:[0-9]|\\x{00B9}|\\x{00B2}|\\x{00B3})|CONIN\\$|CONOUT\\$)",
    "(?:\\..*)?$"
  )
  if (any(grepl(device_pattern, parts, ignore.case = TRUE, perl = TRUE))) {
    if (isTRUE(portable_names) || isTRUE(is_windows)) {
      stop(.archive_name_error(name, "device"))
    }
    relaxed <- c(relaxed, "windows")
  }
  invisible(unique(relaxed))
}

.archive_state_note_relaxed <- function(state, name, rules) {
  if ("windows" %in% rules) {
    state$relaxed_windows <- unique(c(state$relaxed_windows, name))
  }
  invisible(NULL)
}

.archive_state_register_path <- function(state, name) {
  parts <- strsplit(name, "/", fixed = TRUE)[[1L]]
  for (index in seq_along(parts)) {
    prefix <- paste(parts[seq_len(index)], collapse = "/")
    key <- .archive_case_key(prefix)
    if (exists(key, envir = state$case_paths, inherits = FALSE)) {
      previous <- get(key, envir = state$case_paths, inherits = FALSE)
      if (!identical(previous, prefix)) {
        if (isTRUE(state$relax_case)) {
          state$relaxed_case <- unique(c(
            state$relaxed_case, previous, prefix
          ))
        } else if (isTRUE(state$defer_case)) {
          state$deferred_case <- unique(c(
            state$deferred_case, previous, prefix
          ))
        } else {
          stop(.archive_name_error(prefix, "case", other = previous))
        }
      }
    } else {
      assign(key, prefix, envir = state$case_paths)
    }
  }
  invisible(NULL)
}

.archive_warning <- function(message, rule, entries, archive) {
  key <- paste(archive, rule, paste(entries, collapse = "\n"), sep = "\n")
  if (isTRUE(.archive_warning_state$active) &&
        key %in% .archive_warning_state$seen) {
    return(invisible(NULL))
  }
  if (isTRUE(.archive_warning_state$active)) {
    .archive_warning_state$seen <- c(.archive_warning_state$seen, key)
  }
  condition <- structure(
    list(message = message, call = NULL, archive = archive,
         rule = rule, entries = entries),
    class = c("bigbang_warning_archive_nonportable_name", "warning", "condition")
  )
  warning(condition)
  invisible(NULL)
}

.archive_warning_entries <- function(entries) {
  shown <- utils::head(entries, 3L)
  if (length(shown) == 0L) return(character())
  text <- paste(paste0("`", shown, "`"), collapse = ", ")
  extra <- length(entries) - length(shown)
  if (extra > 0L) {
    text <- paste0(text, " ", .bb_trf("and %d more", extra))
  }
  text
}

.archive_state_warn <- function(state) {
  if (isTRUE(state$portable_names)) return(invisible(NULL))
  entries <- utils::head(state$relaxed_windows, 3L)
  if (length(entries) > 0L) {
    .archive_warning(
      .bb_trf(
        paste0(
          "Archive %s contains entries accepted only because ",
          "options(bigbang.archive.portable_names = FALSE): %s. ",
          "This package will not be installable on Windows."
        ),
        state$archive, .archive_warning_entries(state$relaxed_windows)
      ),
      rule = "windows", entries = entries, archive = state$archive
    )
  }
  entries <- utils::head(state$relaxed_case, 3L)
  if (length(entries) > 0L) {
    .archive_warning(
      .bb_trf(
        paste0(
          "Archive %s contains entries accepted because the destination ",
          "filesystem distinguishes case: %s. This package will not be ",
          "installable on filesystems that do not distinguish case."
        ),
        state$archive, .archive_warning_entries(state$relaxed_case)
      ),
      rule = "case", entries = entries, archive = state$archive
    )
  }
  invisible(NULL)
}

.archive_state_relaxed_names <- function(state) {
  list(
    windows = state$relaxed_windows,
    case = state$relaxed_case
  )
}

.archive_state_count_path <- function(state, name) {
  if (exists(name, envir = state$counted_paths, inherits = FALSE)) {
    return(invisible(NULL))
  }
  if (state$entry_count >= state$limits$max_entries) {
    .archive_limit_abort(
      state, "entry count", state$entry_count + 1,
      state$limits$max_entries, "bigbang.archive.max_entries"
    )
  }
  state$entry_count <- state$entry_count + 1L
  assign(name, TRUE, envir = state$counted_paths)
  invisible(NULL)
}

.archive_state_count_entry <- function(state) {
  if (state$entry_count >= state$limits$max_entries) {
    .archive_limit_abort(
      state, "entry count", state$entry_count + 1,
      state$limits$max_entries, "bigbang.archive.max_entries"
    )
  }
  state$entry_count <- state$entry_count + 1L
  invisible(NULL)
}

.archive_state_add_member <- function(state, name, size = 0, display = name) {
  .archive_state_count_path(state, name)
  state$member_count <- state$member_count + 1L
  assign(as.character(state$member_count), name, envir = state$members)
  assign(as.character(state$member_count), as.numeric(size), envir = state$member_lengths)
  assign(as.character(state$member_count), display, envir = state$member_display)
  invisible(NULL)
}

.archive_state_members <- function(state) {
  if (state$member_count == 0L) return(character())
  unlist(
    mget(as.character(seq_len(state$member_count)), envir = state$members),
    use.names = FALSE
  )
}

.archive_state_lengths <- function(state) {
  if (state$member_count == 0L) return(numeric())
  unname(unlist(
    mget(as.character(seq_len(state$member_count)), envir = state$member_lengths),
    use.names = FALSE
  ))
}

.archive_state_display <- function(state) {
  if (state$member_count == 0L) return(character())
  unlist(
    mget(as.character(seq_len(state$member_count)), envir = state$member_display),
    use.names = FALSE
  )
}

.archive_state_add_directory <- function(state, path, mode, mtime) {
  state$directory_count <- state$directory_count + 1L
  assign(
    as.character(state$directory_count),
    list(path = path, mode = mode, mtime = mtime),
    envir = state$directories
  )
  invisible(NULL)
}

.archive_link_abort <- function(archive, entry, kind) {
  entry <- if (length(entry) == 0L || is.na(entry) || !nzchar(entry)) {
    "<unnamed entry>"
  } else {
    entry
  }
  .archive_abort(
    "bigbang_error_archive_link",
    .bb_trf(
      "Archive %s contains symbolic links or another unsupported special entry: %s",
      archive, entry
    ),
    archive = archive, entry = entry, entry_type = kind
  )
}

.archive_read_exact <- function(connection, size) {
  if (length(size) != 1L || is.na(size) || size < 0 ||
        size > .Machine$integer.max) {
    stop(.archive_parse_error("requested archive block is too large"))
  }
  if (size == 0) return(raw())
  result <- if (inherits(connection, "bigbang_tar_stream")) {
    .archive_tar_read(connection, size)
  } else {
    readBin(connection, "raw", n = as.integer(size))
  }
  if (length(result) != size) stop(.archive_parse_error("unexpected end of archive"))
  result
}

.archive_discard <- function(connection, size) {
  if (length(size) != 1L || is.na(size) || size < 0) {
    stop(.archive_parse_error("invalid archive member size"))
  }
  remaining <- size
  while (remaining > 0) {
    chunk <- min(remaining, 1024^2)
    .archive_read_exact(connection, chunk)
    remaining <- remaining - chunk
  }
  invisible(NULL)
}

.archive_raw_text <- function(bytes) {
  if (length(bytes) == 0L) return("")
  rawToChar(bytes, multiple = FALSE)
}

.archive_cstring <- function(bytes) {
  values <- as.integer(bytes)
  zero <- match(0L, values)
  if (!is.na(zero)) bytes <- bytes[seq_len(zero - 1L)]
  .archive_raw_text(bytes)
}

.archive_number <- function(bytes, base = 8L) {
  values <- as.integer(bytes)
  if (length(values) == 0L) stop(.archive_parse_error("empty numeric archive field"))
  if (bitwAnd(values[[1L]], 128L) != 0L) {
    stop(.archive_parse_error("base-256 tar numbers are not supported"))
  }
  text <- trimws(.archive_cstring(bytes))
  if (!nzchar(text)) return(0)
  if (!grepl("^[0-7]+$", text)) stop(.archive_parse_error("invalid octal archive number"))
  result <- 0
  number_base <- base
  for (value in as.integer(strsplit(text, "", fixed = TRUE)[[1L]])) {
    result <- result * number_base + (value - as.integer("0"))
    if (!is.finite(result) || result > 2^53) {
      stop(.archive_parse_error("archive number cannot be represented exactly"))
    }
  }
  result
}

.archive_gzip_member_trailer <- function(archive) {
  size <- file.info(archive)$size
  if (is.na(size) || size < 18 || size > .Machine$integer.max) {
    stop(.archive_parse_error("invalid gzip archive size"))
  }
  connection <- file(archive, open = "rb")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  seek(connection, where = size - 8, origin = "start")
  trailer <- readBin(connection, "raw", n = 8L)
  if (length(trailer) != 8L) {
    stop(.archive_parse_error("truncated gzip archive trailer"))
  }
  list(
    crc = .archive_u32(trailer[1L:4L]),
    size = .archive_u32(trailer[5L:8L])
  )
}

.archive_tar_connection <- function(archive, ext) {
  ext <- tolower(ext)
  stream <- if (ext %in% c(".tar.bz2", ".tbz", ".tbz2")) {
    bzfile(archive, open = "rb")
  } else if (ext %in% c(".tar.xz", ".txz")) {
    xzfile(archive, open = "rb")
  } else if (ext %in% c(".tar.gz", ".tgz")) {
    archive_size <- file.info(archive)$size
    if (is.na(archive_size) || archive_size < 18) {
      stop(.archive_parse_error("invalid gzip archive size"))
    }
    raw_input <- file(archive, open = "rb")
    gzip <- new.env(parent = emptyenv())
    # The trailer is checked after the streaming walk. The CRC sink receives
    # each decompressed block, so validation never sizes a buffer from ISIZE.
    gzip$archive <- normalizePath(archive, winslash = "/", mustWork = TRUE)
    gzip$bytes <- 0
    gzip$checksum <- .archive_crc32_sink()
    gzip$checksum_finished <- FALSE
    gzip$closed <- FALSE
    result <- list(stream = gzcon(raw_input), raw_input = raw_input, gzip = gzip)
    class(result) <- c("bigbang_tar_stream", "list")
    return(result)
  } else {
    file(archive, open = "rb")
  }
  result <- list(stream = stream, raw_input = NULL, gzip = NULL)
  class(result) <- c("bigbang_tar_stream", "list")
  result
}

.archive_tar_read <- function(connection, size) {
  if (!inherits(connection, "bigbang_tar_stream")) {
    return(readBin(connection, "raw", n = as.integer(size)))
  }
  value <- NULL
  messages <- character()
  result <- tryCatch(
    withCallingHandlers(
      {
        messages <- utils::capture.output(
          value <- readBin(connection$stream, "raw", n = as.integer(size)),
          type = "message"
        )
        value
      },
      warning = function(w) {
        stop(.archive_parse_error(
          paste0("could not decompress TAR archive: ", conditionMessage(w))
        ))
      }
    ),
    error = identity
  )
  if (inherits(result, "error")) {
    if (inherits(result, "bigbang_archive_parse_error")) stop(result)
    stop(.archive_parse_error(
      .archive_gzip_trailer_message(conditionMessage(result))
    ))
  }
  if (length(messages) > 0L) {
    stop(.archive_parse_error(
      .archive_gzip_trailer_message(paste(messages, collapse = "; "))
    ))
  }
  result <- value
  if (!is.null(connection$gzip) && length(result) > 0L) {
    connection$gzip$bytes <- connection$gzip$bytes + length(result)
    connection$gzip$checksum$write(result)
  }
  result
}

.archive_tar_close <- function(connection) {
  if (!inherits(connection, "bigbang_tar_stream")) {
    try(close(connection), silent = TRUE)
    return(invisible(NULL))
  }
  if (!is.null(connection$gzip)) {
    if (!isTRUE(connection$gzip$closed)) {
      try(close(connection$stream), silent = TRUE)
      connection$gzip$closed <- TRUE
    }
    if (!isTRUE(connection$gzip$checksum_finished)) {
      try(connection$gzip$checksum$finish(), silent = TRUE)
      connection$gzip$checksum_finished <- TRUE
    }
    try(unlink(connection$gzip$checksum$path, force = TRUE), silent = TRUE)
  } else {
    try(close(connection$stream), silent = TRUE)
  }
  invisible(NULL)
}

.archive_tar_finish <- function(connection) {
  if (!inherits(connection, "bigbang_tar_stream") || is.null(connection$gzip)) {
    return(invisible(NULL))
  }
  close_result <- tryCatch(
    withCallingHandlers(
      utils::capture.output(close(connection$stream), type = "message"),
      warning = function(w) {
        stop(.archive_parse_error(
          paste0("could not decompress TAR archive: ", conditionMessage(w))
        ))
      }
    ),
    error = identity
  )
  connection$gzip$closed <- TRUE
  if (inherits(close_result, "error")) {
    if (inherits(close_result, "bigbang_archive_parse_error")) stop(close_result)
    stop(.archive_parse_error(
      .archive_gzip_trailer_message(conditionMessage(close_result))
    ))
  }
  if (length(close_result) > 0L) {
    stop(.archive_parse_error(
      .archive_gzip_trailer_message(paste(close_result, collapse = "; "))
    ))
  }
  generated <- connection$gzip$checksum$finish()
  connection$gzip$checksum_finished <- TRUE
  trailer <- .archive_gzip_member_trailer(connection$gzip$archive)
  if (trailer$crc != generated$crc || trailer$size != generated$size) {
    stop(.archive_parse_error(
      "gzip trailer CRC-32 or ISIZE does not match the decompressed TAR stream"
    ))
  }
  invisible(NULL)
}

.archive_gzip_trailer_message <- function(detail) {
  detail <- as.character(detail)
  if (grepl(
    "crc error|internal error|memdecompress", detail,
    ignore.case = TRUE, perl = TRUE
  )) {
    return(.bb_trf("could not validate gzip member trailer"))
  }
  .bb_trf("could not decompress TAR archive: %s", detail)
}

.archive_tar_checksum_ok <- function(header) {
  expected <- .archive_number(header[149L:156L], base = 8L)
  checked <- as.integer(header)
  checked[149L:156L] <- 32L
  sum(checked) == expected
}

.archive_pax_records <- function(payload) {
  if (length(payload) == 0L) return(list())
  result <- list()
  position <- 1L
  payload_size <- length(payload)
  while (position <= payload_size) {
    newline <- match(10L, as.integer(payload[position:payload_size]))
    if (is.na(newline)) stop(.archive_parse_error("unterminated pax record"))
    newline <- position + newline - 1L
    space <- match(32L, as.integer(payload[position:newline]))
    if (is.na(space)) stop(.archive_parse_error("invalid pax record"))
    space <- position + space - 1L
    length_text <- .archive_raw_text(payload[position:(space - 1L)])
    if (!grepl("^[1-9][0-9]*$", length_text)) {
      stop(.archive_parse_error("invalid pax record length"))
    }
    record_length <- as.numeric(length_text)
    if (!is.finite(record_length) || record_length != newline - position + 1L) {
      stop(.archive_parse_error("inconsistent pax record length"))
    }
    body_start <- space + 1L
    body_end <- newline - 1L
    body <- if (body_start <= body_end) {
      .archive_raw_text(payload[body_start:body_end])
    } else {
      ""
    }
    if (grepl("\\x00|\\r", body, perl = TRUE)) {
      stop(.archive_parse_error("invalid pax record value"))
    }
    equals <- regexpr("=", body, fixed = TRUE)[[1L]]
    if (equals <= 1L) stop(.archive_parse_error("invalid pax key/value record"))
    key <- substr(body, 1L, equals - 1L)
    value <- substr(body, equals + 1L, nchar(body))
    if (key %in% names(result) && !identical(result[[key]], value)) {
      stop(.archive_parse_error("contradictory pax records"))
    }
    result[[key]] <- value
    position <- newline + 1L
  }
  result
}

.archive_pax_validate_keys <- function(records) {
  keys <- names(records)
  dangerous <- grepl(
    "sparse|offset|numblocks|map|realsize|hole|data", keys,
    ignore.case = TRUE, perl = TRUE
  ) & keys != "size"
  if (any(dangerous)) {
    stop(.archive_parse_error("unsupported pax record changes tar geometry"))
  }
  invisible(records)
}

.archive_pax_number <- function(value, key) {
  if (length(value) != 1L || is.na(value) ||
        !grepl("^[0-9]+$", value, perl = TRUE)) {
    stop(.archive_parse_error(paste0("invalid pax ", key, " value")))
  }
  result <- 0
  for (digit in as.integer(strsplit(value, "", fixed = TRUE)[[1L]])) {
    result <- result * 10 + digit
    if (!is.finite(result) || result > 2^53) {
      stop(.archive_parse_error(paste0("pax ", key, " is too large")))
    }
  }
  result
}

.archive_pax_time <- function(value) {
  if (length(value) != 1L || is.na(value) ||
        !grepl("^[0-9]+(\\.[0-9]+)?$", value, perl = TRUE)) {
    stop(.archive_parse_error("invalid pax mtime value"))
  }
  result <- as.numeric(value)
  if (!is.finite(result) || result > 2^53) {
    stop(.archive_parse_error("pax mtime is too large"))
  }
  result
}

.archive_tar_normalize_path <- function(name, limits = NULL,
                                        portable_names = NULL,
                                        is_windows = NULL) {
  if (is.null(limits)) limits <- .archive_limit_options()
  first_rules <- .archive_validate_name_text(
    name, limits, allow_dots = TRUE,
    portable_names = portable_names, is_windows = is_windows
  )
  trailing_slash <- endsWith(name, "/")
  parts <- strsplit(name, "/", fixed = TRUE)[[1L]]
  if (any(parts == "..")) stop(.archive_parse_error("tar entry has parent traversal"))
  parts <- parts[nzchar(parts) & parts != "."]
  if (length(parts) == 0L) stop(.archive_parse_error("tar entry has an empty path"))
  normalized <- paste(parts, collapse = "/")
  second_rules <- .archive_validate_name_text(
    normalized, limits, portable_names = portable_names,
    is_windows = is_windows
  )
  attr(normalized, "archive_trailing_slash") <- trailing_slash
  attr(normalized, "archive_relaxed_rules") <- unique(c(first_rules, second_rules))
  normalized
}

.archive_tar_existing_kind <- function(path, is_windows = NULL,
                                       normalize = NULL, exists = NULL) {
  if (isTRUE(.path_is_symlink(
    path, is_windows = is_windows, normalize = normalize, exists = exists
  ))) {
    return("symbolic link")
  }
  if (dir.exists(path)) return("directory")
  if (file.exists(path)) return("file")
  ""
}

.archive_tar_prepare_parent <- function(extract_dir, name, state, archive) {
  parts <- strsplit(name, "/", fixed = TRUE)[[1L]]
  if (length(parts) <= 1L) return(invisible(NULL))
  current <- if (is.null(extract_dir)) "" else extract_dir
  for (index in seq_len(length(parts) - 1L)) {
    relative <- paste(parts[seq_len(index)], collapse = "/")
    if (nzchar(current)) current <- file.path(current, parts[[index]])
    kind <- if (exists(relative, envir = state$types, inherits = FALSE)) {
      get(relative, envir = state$types, inherits = FALSE)
    } else {
      ""
    }
    if (identical(kind, "symbolic link")) {
      .archive_link_abort(archive, relative, "existing symbolic link")
    }
    if (nzchar(kind) && !identical(kind, "directory")) {
      stop(.archive_parse_error("tar path crosses a non-directory entry"))
    }
    if (!nzchar(kind)) {
      .archive_state_count_path(state, relative)
      if (nzchar(current) && !dir.create(
        current, showWarnings = FALSE, mode = "0700"
      )) {
        stop(.archive_parse_error("could not create tar parent directory"))
      }
      assign(relative, "directory", envir = state$types)
    }
  }
  invisible(NULL)
}

.archive_tar_payload <- function(connection, size, keep = FALSE, state = NULL) {
  if (!is.null(state)) .archive_state_reserve_meta(state, size)
  if (!keep) {
    .archive_discard(connection, size)
    return(raw())
  }
  .archive_read_exact(connection, size)
}

.archive_tar_copy_payload <- function(connection, path, size) {
  output <- file(path, open = "wb")
  on.exit(try(close(output), silent = TRUE), add = TRUE)
  remaining <- size
  while (remaining > 0) {
    chunk <- min(remaining, 1024^2)
    bytes <- .archive_read_exact(connection, chunk)
    writeBin(bytes, output)
    remaining <- remaining - chunk
  }
  close(output)
  invisible(NULL)
}

.archive_tar_apply_mode_time <- function(path, mode, mtime, directory = FALSE) {
  permission <- bitwAnd(mode, 511L)
  if (isTRUE(directory)) permission <- bitwOr(permission, 448L)
  if (!isTRUE(tryCatch(
    Sys.chmod(path, mode = sprintf("0%o", permission), use_umask = FALSE),
    error = function(error) FALSE
  ))) {
    stop(.archive_parse_error("could not apply tar entry permissions"))
  }
  if (!is.null(mtime)) {
    time <- as.POSIXct(mtime, origin = "1970-01-01", tz = "UTC")
    if (!isTRUE(tryCatch(Sys.setFileTime(path, time), error = function(error) FALSE))) {
      stop(.archive_parse_error("could not apply tar entry time"))
    }
  }
  invisible(NULL)
}

.archive_tar_long_name <- function(payload) {
  value <- .archive_raw_text(payload)
  value <- sub("\\x00.*$", "", value, perl = TRUE)
  sub("\\n$", "", value, perl = TRUE)
}

.archive_tar_walk <- function(archive, ext, extract_dir = NULL,
                              is_windows = NULL, case_sensitive = NULL) {
  connection <- .archive_tar_connection(archive, ext)
  on.exit(.archive_tar_close(connection), add = TRUE)
  state <- .archive_state_new(
    archive, extract_dir, is_windows = is_windows,
    case_sensitive = case_sensitive
  )
  global_pax <- list()
  pending_pax <- list()
  long_name <- NULL

  repeat {
    header <- .archive_tar_read(connection, 512L)
    if (length(header) == 0L) {
      stop(.archive_parse_error("tar archive has no end-of-archive blocks"))
    }
    if (length(header) != 512L) {
      stop(.archive_parse_error("truncated tar header"))
    }
    if (all(as.integer(header) == 0L)) {
      .archive_state_reserve_meta(state, 512L)
      second <- .archive_read_exact(connection, 512L)
      .archive_state_reserve_meta(state, 512L)
      if (!all(as.integer(second) == 0L)) {
        stop(.archive_parse_error("truncated tar end-of-archive marker"))
      }
      repeat {
        trailing <- .archive_tar_read(connection, 512L)
        if (length(trailing) == 0L) break
        .archive_state_reserve_meta(state, length(trailing))
        if (length(trailing) != 512L || any(as.integer(trailing) != 0L)) {
          stop(.archive_parse_error("non-zero data follows the tar end-of-archive marker"))
        }
      }
      if (length(pending_pax) > 0L || !is.null(long_name)) {
        stop(.archive_parse_error("tar metadata has no following entry"))
      }
      break
    }
    if (!.archive_tar_checksum_ok(header)) {
      stop(.archive_parse_error("invalid tar header checksum"))
    }

    .archive_state_reserve_meta(state, 512L)

    header_name <- .archive_cstring(header[1L:100L])
    prefix <- .archive_cstring(header[346L:500L])
    combined_name <- if (nzchar(prefix)) paste0(prefix, "/", header_name) else header_name
    typeflag <- as.integer(header[[157L]])
    type_code <- if (typeflag == 0L) 0L else if (typeflag %in% 48L:55L) {
      typeflag - 48L
    } else {
      typeflag
    }
    header_size <- .archive_number(header[125L:136L], base = 8L)
    header_mode <- .archive_number(header[101L:108L], base = 8L)
    header_mtime <- .archive_number(header[137L:148L], base = 8L)
    linkname <- .archive_cstring(header[158L:257L])

    if (typeflag %in% c(120L, 103L, 76L, 75L)) {
      .archive_state_count_entry(state)
      payload <- .archive_tar_payload(
        connection, header_size, keep = TRUE, state = state
      )
      padding <- (512 - (header_size %% 512)) %% 512
      if (padding > 0) .archive_discard(connection, padding)
      if (typeflag %in% c(120L, 103L)) {
        records <- .archive_pax_records(payload)
        .archive_pax_validate_keys(records)
        if ("linkpath" %in% names(records)) {
          .archive_link_abort(archive, records[["linkpath"]], "pax linkpath")
        }
        if (typeflag == 103L) {
          global_pax <- utils::modifyList(global_pax, records)
        } else {
          pending_pax <- utils::modifyList(pending_pax, records)
        }
      } else if (typeflag == 76L) {
        if (!is.null(long_name)) stop(.archive_parse_error("contradictory GNU long names"))
        long_name <- .archive_tar_long_name(payload)
      } else {
        .archive_link_abort(archive, .archive_tar_long_name(payload), "GNU long link")
      }
      next
    }

    attributes <- utils::modifyList(global_pax, pending_pax)
    pax_name <- attributes[["path"]]
    raw_name <- if (!is.null(pax_name)) pax_name else if (!is.null(long_name)) {
      long_name
    } else {
      combined_name
    }
    name <- .archive_tar_normalize_path(
      raw_name, state$limits, portable_names = state$portable_names,
      is_windows = state$is_windows
    )
    .archive_state_note_relaxed(
      state, name, attr(name, "archive_relaxed_rules")
    )
    if (!is.null(pax_name) && !is.null(long_name) && !identical(pax_name, long_name)) {
      stop(.archive_parse_error("contradictory tar entry names"))
    }
    if (!is.null(attributes[["linkpath"]])) {
      .archive_link_abort(archive, attributes[["linkpath"]], "pax linkpath")
    }

    type_name <- switch(
      as.character(type_code),
      `0` = "regular file", `1` = "hard link", `2` = "symbolic link",
      `3` = "character device", `4` = "block device", `5` = "directory",
      `6` = "FIFO", `7` = "contiguous file", "unknown tar entry"
    )
    if (!(type_code %in% c(0L, 5L)) || nzchar(linkname)) {
      .archive_link_abort(archive, name, type_name)
    }

    size <- header_size
    if (!is.null(attributes[["size"]])) {
      size <- .archive_pax_number(attributes[["size"]], "size")
    }
    if (type_code == 5L && size != 0) {
      stop(.archive_parse_error("tar directory has non-zero size"))
    }
    if (isTRUE(attr(name, "archive_trailing_slash")) && type_code != 5L) {
      stop(.archive_parse_error(
        "tar entry ending in '/' is not a directory"
      ))
    }
    padding <- (512 - (size %% 512)) %% 512
    .archive_state_register_path(state, name)
    .archive_tar_prepare_parent(extract_dir, name, state, archive)

    already_entry <- exists(name, envir = state$entries, inherits = FALSE)
    if (already_entry) stop(.archive_parse_error("duplicate tar entry path"))

    if (type_code == 0L && exists(name, envir = state$types, inherits = FALSE) &&
          identical(get(name, envir = state$types, inherits = FALSE), "directory")) {
      stop(.archive_parse_error("tar file replaces an existing directory entry"))
    }

    .archive_state_add_member(state, name, size)

    target <- if (is.null(extract_dir)) NULL else file.path(extract_dir, name)
    if (type_code == 5L) {
      existing <- if (exists(name, envir = state$types, inherits = FALSE)) {
        get(name, envir = state$types, inherits = FALSE)
      } else {
        ""
      }
      if (identical(existing, "symbolic link")) .archive_link_abort(archive, name, existing)
      if (identical(existing, "file")) {
        stop(.archive_parse_error("tar directory replaces a file"))
      }
      if (is.null(target)) {
        assign(name, "directory", envir = state$types)
      } else if (!identical(existing, "directory") &&
                   !dir.create(target, showWarnings = FALSE)) {
        stop(.archive_parse_error("could not create tar directory"))
      }
      assign(name, "directory", envir = state$types)
      assign(name, "directory", envir = state$entries)
      .archive_state_add_directory(
        state, target, header_mode,
        if (!is.null(attributes[["mtime"]]) &&
              nzchar(attributes[["mtime"]])) .archive_pax_time(attributes[["mtime"]]) else header_mtime
      )
      if (padding > 0) .archive_discard(connection, padding)
    } else {
      .archive_state_reserve(state, size)
      if (!is.null(target)) {
        .archive_tar_copy_payload(connection, target, size)
        mtime <- if (!is.null(attributes[["mtime"]]) && nzchar(attributes[["mtime"]])) {
          .archive_pax_time(attributes[["mtime"]])
        } else {
          header_mtime
        }
        .archive_tar_apply_mode_time(target, header_mode, mtime, directory = FALSE)
      }
      if (is.null(extract_dir)) .archive_discard(connection, size)
      if (padding > 0) .archive_discard(connection, padding)
      assign(name, "file", envir = state$types)
      assign(name, "file", envir = state$entries)
    }
    pending_pax <- list()
    long_name <- NULL
  }

  .archive_tar_finish(connection)

  if (!is.null(extract_dir) && state$directory_count > 0L) {
    for (index in seq_len(state$directory_count)) {
      directory <- get(as.character(index), envir = state$directories)
      .archive_tar_apply_mode_time(
        directory$path, directory$mode, directory$mtime, directory = TRUE
      )
    }
  }
  .archive_state_warn(state)
  result <- .archive_state_members(state)
  attr(result, "archive_relaxed_names") <- .archive_state_relaxed_names(state)
  result
}

.archive_tar_inspect <- function(archive, ext, is_windows = NULL,
                                 case_sensitive = NULL) {
  .archive_tar_walk(
    archive, ext, is_windows = is_windows, case_sensitive = case_sensitive
  )
}

.archive_tar_extract <- function(archive, ext, extract_dir, is_windows = NULL,
                                 case_sensitive = NULL) {
  .archive_tar_walk(
    archive, ext, extract_dir = extract_dir, is_windows = is_windows,
    case_sensitive = case_sensitive
  )
}


.archive_u16 <- function(bytes) {
  values <- as.integer(bytes)
  sum(values * c(1, 256))
}

.archive_u32 <- function(bytes) {
  values <- as.integer(bytes)
  result <- 0
  for (index in seq_along(values)) result <- result + values[[index]] * 256^(index - 1L)
  if (!is.finite(result) || result > 2^53) stop(.archive_parse_error("ZIP number is too large"))
  result
}

.archive_le32 <- function(value) {
  as.raw(c(
    value %% 256, value %/% 256 %% 256, value %/% 65536 %% 256,
    value %/% 16777216 %% 256
  ))
}

.archive_crc32_sink <- function() {
  path <- tempfile("bigbang-archive-crc-")
  output <- file(path, open = "wb")
  stream <- gzcon(output, level = 0)
  state <- new.env(parent = emptyenv())
  state$closed <- FALSE
  write <- function(bytes) {
    if (length(bytes) == 0L) return(invisible(NULL))
    result <- tryCatch(writeBin(bytes, stream), error = identity)
    if (inherits(result, "error")) {
      stop(.archive_parse_error(
        paste0("could not calculate archive CRC-32: ", conditionMessage(result))
      ))
    }
    invisible(NULL)
  }
  finish <- function() {
    if (!isTRUE(state$closed)) {
      result <- tryCatch(close(stream), error = identity)
      state$closed <- TRUE
      if (inherits(result, "error")) {
        stop(.archive_parse_error(
          paste0("could not calculate archive CRC-32: ", conditionMessage(result))
        ))
      }
    }
    size <- file.info(path)$size
    if (is.na(size) || size < 18) {
      stop(.archive_parse_error("incomplete gzip CRC-32 output"))
    }
    connection <- file(path, open = "rb")
    on.exit(try(close(connection), silent = TRUE), add = TRUE)
    seek(connection, where = size - 8, origin = "start")
    trailer <- readBin(connection, "raw", n = 8L)
    if (length(trailer) != 8L) {
      stop(.archive_parse_error("incomplete gzip CRC-32 trailer"))
    }
    list(
      crc = .archive_u32(trailer[1L:4L]),
      size = .archive_u32(trailer[5L:8L])
    )
  }
  list(path = path, write = write, finish = finish)
}

.archive_crc32_table <- local({
  table <- integer(256L)
  for (index in 0:255) {
    value <- index
    for (bit in seq_len(8L)) {
      value <- if (bitwAnd(value, 1L) != 0L) {
        bitwXor(bitwShiftR(value, 1L), -306674912L)
      } else {
        bitwShiftR(value, 1L)
      }
    }
    table[index + 1L] <- value
  }
  table
})

.archive_crc32 <- function(bytes) {
  crc <- -1L
  if (length(bytes) > 0L) {
    for (byte in as.integer(bytes)) {
      index <- bitwAnd(bitwXor(crc, byte), 255L) + 1L
      crc <- bitwXor(bitwShiftR(crc, 8L), .archive_crc32_table[[index]])
    }
  }
  bitwXor(crc, -1L)
}

.archive_crc32_unsigned <- function(value) {
  value <- as.numeric(value)
  if (value < 0) value + 4294967296 else value
}

.archive_zip_extra <- function(bytes) {
  if (length(bytes) == 0L) return(list())
  result <- list()
  position <- 1L
  while (position <= length(bytes)) {
    if (position + 3L > length(bytes)) stop(.archive_parse_error("truncated ZIP extra field"))
    identifier <- .archive_u16(bytes[position:(position + 1L)])
    size <- .archive_u16(bytes[(position + 2L):(position + 3L)])
    end <- position + 3L + size
    if (end > length(bytes)) stop(.archive_parse_error("inconsistent ZIP extra field"))
    result[[length(result) + 1L]] <- list(id = identifier, data = if (size > 0) {
      bytes[(position + 4L):end]
    } else {
      raw()
    })
    position <- end + 1L
  }
  result
}

.archive_zip_name <- function(bytes) {
  if (any(as.integer(bytes) == 0L)) stop(.archive_parse_error("ZIP entry contains a NUL byte"))
  .archive_raw_text(bytes)
}

.archive_zip_normalize_name <- function(name, limits = NULL,
                                        portable_names = NULL,
                                        is_windows = NULL) {
  if (is.null(limits)) limits <- .archive_limit_options()
  rules <- .archive_validate_name_text(
    name, limits, portable_names = portable_names, is_windows = is_windows
  )
  attr(name, "archive_relaxed_rules") <- rules
  name
}

.archive_zip_read_at <- function(connection, offset, size) {
  if (offset < 0 || size < 0 || size > .Machine$integer.max) {
    stop(.archive_parse_error("invalid ZIP file offset"))
  }
  seek(connection, where = offset, origin = "start")
  result <- readBin(connection, "raw", n = as.integer(size))
  if (length(result) != size) stop(.archive_parse_error("truncated ZIP file"))
  result
}

.archive_gzip_container <- function(deflate, crc, size) {
  c(
    as.raw(c(31L, 139L, 8L, 0L, 0L, 0L, 0L, 0L, 0L, 3L)),
    deflate, .archive_le32(crc), .archive_le32(size)
  )
}

.archive_zip_error_message <- function(error) {
  detail <- if (inherits(error, "condition")) {
    conditionMessage(error)
  } else {
    as.character(error)
  }
  if (grepl("crc error", tolower(detail), fixed = TRUE)) {
    paste0(detail, " (CRC-32 verification failed)")
  } else {
    detail
  }
}

.archive_zip_descriptor <- function(connection, start, central_offset, entry) {
  if (start >= central_offset || central_offset - start < 12L) {
    stop(.archive_parse_error("ZIP data descriptor is missing"))
  }
  available <- min(16L, central_offset - start)
  bytes <- .archive_zip_read_at(connection, start, available)
  matches <- list()
  if (length(bytes) >= 12L) {
    values <- c(
      .archive_u32(bytes[1L:4L]), .archive_u32(bytes[5L:8L]),
      .archive_u32(bytes[9L:12L])
    )
    if (identical(values, c(entry$crc, entry$compressed_size,
                            entry$uncompressed_size))) {
      matches[[length(matches) + 1L]] <- 12L
    }
  }
  if (length(bytes) >= 16L && .archive_u32(bytes[1L:4L]) == 0x08074b50) {
    values <- c(
      .archive_u32(bytes[5L:8L]), .archive_u32(bytes[9L:12L]),
      .archive_u32(bytes[13L:16L])
    )
    if (identical(values, c(entry$crc, entry$compressed_size,
                            entry$uncompressed_size))) {
      matches[[length(matches) + 1L]] <- 16L
    }
  }
  if (length(matches) == 0L) {
    stop(.archive_parse_error(
      "ZIP data descriptor does not match the central directory"
    ))
  }
  if (length(matches) > 1L) 16L else matches[[1L]]
}

.archive_zip_intervals <- function(intervals, central_offset) {
  if (length(intervals) == 0L) {
    if (central_offset != 0L) {
      stop(.archive_parse_error(
        "ZIP local-file records do not cover the pre-central region"
      ))
    }
    return(invisible(NULL))
  }
  starts <- vapply(intervals, `[[`, numeric(1L), "start")
  order_index <- order(starts, method = "radix")
  cursor <- 0
  for (index in order_index) {
    interval <- intervals[[index]]
    if (interval$start != cursor) {
      message <- if (interval$start < cursor) {
        "ZIP local-file records overlap"
      } else {
        "ZIP local-file records leave a gap before the central directory"
      }
      stop(.archive_parse_error(message))
    }
    if (interval$end <= interval$start) {
      stop(.archive_parse_error("invalid ZIP local-file record bounds"))
    }
    cursor <- interval$end
  }
  if (cursor != central_offset) {
    stop(.archive_parse_error(
      "ZIP local-file records do not cover the pre-central region"
    ))
  }
  invisible(NULL)
}

.archive_zip_payload <- function(connection, entry, state = NULL) {
  if (!is.null(state)) .archive_state_reserve(state, entry$compressed_size)
  compressed <- .archive_zip_read_at(
    connection, entry$local_data_start, entry$compressed_size
  )
  if (entry$method == 0L) {
    if (entry$compressed_size != entry$uncompressed_size) {
      stop(.archive_parse_error("stored ZIP entry has inconsistent sizes"))
    }
  } else if (entry$method == 8L) {
    gzip <- .archive_gzip_container(
      compressed, entry$crc, entry$uncompressed_size
    )
  } else {
    stop(.archive_parse_error("ZIP compression method is not supported"))
  }
  output_limit <- entry$uncompressed_size
  if (!is.null(state)) {
    previous_total <- state$total_bytes - entry$uncompressed_size
    remaining_total <- state$limits$max_total_bytes - previous_total
    output_limit <- min(output_limit, remaining_total)
  }
  checksum <- .archive_crc32_sink()
  on.exit(unlink(checksum$path, force = TRUE), add = TRUE)
  chunks <- list()
  if (entry$method == 0L) {
    checksum$write(compressed)
    payload <- compressed
  } else {
    raw_input <- rawConnection(gzip, open = "rb")
    stream <- gzcon(raw_input)
    stream_state <- new.env(parent = emptyenv())
    stream_state$closed <- FALSE
    on.exit({
      if (!isTRUE(stream_state$closed)) {
        try(suppressWarnings(close(stream)), silent = TRUE)
        try(suppressWarnings(close(raw_input)), silent = TRUE)
      }
    }, add = TRUE)
    read_chunk <- function(size) {
      tryCatch(
        withCallingHandlers(
          readBin(stream, "raw", n = as.integer(size)),
          warning = function(w) {
            stop(.archive_parse_error(
              paste0(
                "could not decompress ZIP entry: ",
                .archive_zip_error_message(w)
              )
            ))
          }
        ),
        error = function(error) {
          if (inherits(error, "bigbang_archive_parse_error")) stop(error)
          stop(.archive_parse_error(
            paste0(
              "could not decompress ZIP entry: ",
              .archive_zip_error_message(error)
            )
          ))
        }
      )
    }
    close_stream <- function() {
      result <- tryCatch(
        withCallingHandlers(
          close(stream),
          warning = function(w) {
            stop(.archive_parse_error(
              paste0(
                "could not decompress ZIP entry: ",
                .archive_zip_error_message(w)
              )
            ))
          }
        ),
        error = identity
      )
      if (inherits(result, "error")) stop(result)
      stream_state$closed <- TRUE
      invisible(NULL)
    }
    produced <- 0
    messages <- utils::capture.output({
      repeat {
        remaining <- output_limit + 1 - produced
        if (remaining <= 0) break
        chunk <- read_chunk(min(4 * 1024^2, remaining))
        if (length(chunk) == 0L) break
        checksum$write(chunk)
        chunks[[length(chunks) + 1L]] <- chunk
        produced <- produced + length(chunk)
        if (produced > output_limit) {
          stop(.archive_parse_error(
            "ZIP entry exceeds its declared or configured decompressed size"
          ))
        }
      }
      close_stream()
    }, type = "message")
    if (length(messages) > 0L) {
      stop(.archive_parse_error(
        paste0(
          "could not decompress ZIP entry: ",
          .archive_zip_error_message(paste(messages, collapse = " "))
        )
      ))
    }
    payload <- if (length(chunks) == 0L) raw() else do.call(c, chunks)
  }
  if (length(payload) != entry$uncompressed_size) {
    stop(.archive_parse_error("ZIP entry has an inconsistent uncompressed size"))
  }
  generated <- checksum$finish()
  if (generated$crc != entry$crc || generated$size != entry$uncompressed_size) {
    stop(.archive_parse_error(
      "ZIP entry CRC-32 or uncompressed size does not match the central directory"
    ))
  }
  payload
}

.archive_zip_apply_mode <- function(path, mode, directory) {
  permission <- bitwAnd(mode, 511L)
  if (directory) {
    permission <- bitwOr(permission, 448L)
  } else if (permission == 0L) {
    permission <- 420L
  }
  if (!isTRUE(tryCatch(
    Sys.chmod(path, mode = sprintf("0%o", permission), use_umask = FALSE),
    error = function(error) FALSE
  ))) {
    stop(.archive_parse_error("could not apply ZIP entry permissions"))
  }
  invisible(NULL)
}

.archive_zip_walk <- function(archive, extract_dir = NULL, is_windows = NULL,
                              case_sensitive = NULL) {
  size <- file.info(archive)$size
  if (is.na(size) || size < 22) {
    stop(.archive_parse_error("invalid ZIP file size"))
  }
  connection <- file(archive, open = "rb")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  state <- .archive_state_new(
    archive, extract_dir, is_windows = is_windows,
    case_sensitive = case_sensitive
  )
  tail_start <- max(0, size - 65557)
  tail <- .archive_zip_read_at(connection, tail_start, size - tail_start)
  values <- as.integer(tail)
  tail_length <- length(values)
  candidates <- which(
    values[seq_len(tail_length - 3L)] == 80L &
      values[2L:(tail_length - 2L)] == 75L &
      values[3L:(tail_length - 1L)] == 5L &
      values[4L:tail_length] == 6L
  )
  if (length(candidates) == 0L) stop(.archive_parse_error("ZIP end-of-central-directory record not found"))
  candidate_record <- function(candidate) {
    candidate_offset <- tail_start + candidate - 1L
    if (candidate_offset + 22L > size) return(NULL)
    field <- function(offset, width) {
      tail[(candidate + offset):(candidate + offset + width - 1L)]
    }
    list(
      offset = candidate_offset,
      disk = .archive_u16(field(4L, 2L)),
      central_disk = .archive_u16(field(6L, 2L)),
      disk_entries = .archive_u16(field(8L, 2L)),
      total_entries = .archive_u16(field(10L, 2L)),
      central_size = .archive_u32(field(12L, 4L)),
      central_offset = .archive_u32(field(16L, 4L)),
      comment_size = .archive_u16(field(20L, 2L))
    )
  }
  records <- lapply(candidates, candidate_record)
  structural <- vapply(records, function(record) {
    !is.null(record) &&
      record$disk_entries == record$total_entries &&
      record$central_offset + record$central_size == record$offset &&
      record$central_offset <= record$offset
  }, logical(1L))
  consistent <- vapply(seq_along(records), function(index) {
    record <- records[[index]]
    if (!isTRUE(structural[[index]]) ||
          record$offset + 22L + record$comment_size != size) {
      return(FALSE)
    }
    central_signature <- tryCatch(
      .archive_zip_read_at(connection, record$central_offset, 4L),
      error = function(error) raw()
    )
    (record$central_size == 0L && record$total_entries == 0L) ||
      (length(central_signature) == 4L &&
         .archive_u32(central_signature) == 0x02014b50)
  }, logical(1L))
  if (sum(consistent) > 1L) {
    stop(.archive_parse_error(
      "ZIP contains multiple end-of-central-directory records"
    ))
  }
  selected <- which(consistent)
  if (length(selected) != 1L) {
    stop(.archive_parse_error("inconsistent ZIP end-of-central-directory record"))
  }
  eocd <- records[[selected[[1L]]]]$offset
  eocd_relative <- eocd - tail_start + 1L
  eocd_field <- function(offset, width) {
    tail[(eocd_relative + offset):(eocd_relative + offset + width - 1L)]
  }
  if (.archive_u16(eocd_field(4L, 2L)) != 0L ||
        .archive_u16(eocd_field(6L, 2L)) != 0L) {
    stop(.archive_parse_error("multi-disk ZIP archives are not supported"))
  }
  disk_entries <- .archive_u16(eocd_field(8L, 2L))
  total_entries <- .archive_u16(eocd_field(10L, 2L))
  central_size <- .archive_u32(eocd_field(12L, 4L))
  central_offset <- .archive_u32(eocd_field(16L, 4L))
  comment_size <- .archive_u16(eocd_field(20L, 2L))
  if (disk_entries != total_entries || eocd + 22L + comment_size != size) {
    stop(.archive_parse_error("inconsistent ZIP end-of-central-directory record"))
  }
  .archive_state_reserve_meta(state, comment_size)
  near_start <- max(1L, eocd_relative - 100L)
  near_end <- eocd_relative - 1L
  zip64_signature <- if (near_start <= near_end) {
    window <- values[near_start:near_end]
    length(window) >= 4L && any(
      window[seq_len(length(window) - 3L)] == 80L &
        window[2L:(length(window) - 2L)] == 75L &
        window[3L:(length(window) - 1L)] == 6L &
        window[4L:length(window)] %in% c(6L, 7L)
    )
  } else {
    FALSE
  }
  if (disk_entries == 65535L || total_entries == 65535L ||
        central_size == 0xffffffff || central_offset == 0xffffffff ||
        zip64_signature) {
    stop(.archive_parse_error("ZIP64 archives are not supported"))
  }
  if (central_offset + central_size != eocd || central_size > .Machine$integer.max) {
    stop(.archive_parse_error("inconsistent ZIP central-directory bounds"))
  }

  if (total_entries > state$limits$max_entries) {
    .archive_limit_abort(
      state, "entry count", total_entries,
      state$limits$max_entries, "bigbang.archive.max_entries"
    )
  }
  .archive_state_reserve_meta(state, central_size)
  central <- .archive_zip_read_at(connection, central_offset, central_size)
  entries <- new.env(hash = TRUE, parent = emptyenv())
  seen_names <- new.env(hash = TRUE, parent = emptyenv())
  intervals <- list()
  position <- 1L
  for (entry_index in seq_len(total_entries)) {
    if (position + 45L > length(central) ||
          .archive_u32(central[position:(position + 3L)]) != 0x02014b50) {
      stop(.archive_parse_error("invalid ZIP central-directory entry"))
    }
    made_by <- .archive_u16(central[(position + 4L):(position + 5L)])
    origin <- made_by %/% 256L
    needed <- .archive_u16(central[(position + 6L):(position + 7L)])
    flags <- .archive_u16(central[(position + 8L):(position + 9L)])
    method <- .archive_u16(central[(position + 10L):(position + 11L)])
    crc <- .archive_u32(central[(position + 16L):(position + 19L)])
    compressed_size <- .archive_u32(central[(position + 20L):(position + 23L)])
    uncompressed_size <- .archive_u32(central[(position + 24L):(position + 27L)])
    name_size <- .archive_u16(central[(position + 28L):(position + 29L)])
    extra_size <- .archive_u16(central[(position + 30L):(position + 31L)])
    comment_entry_size <- .archive_u16(central[(position + 32L):(position + 33L)])
    disk_start <- .archive_u16(central[(position + 34L):(position + 35L)])
    external <- central[(position + 38L):(position + 41L)]
    local_offset <- .archive_u32(central[(position + 42L):(position + 45L)])
    end <- position + 45L + name_size + extra_size + comment_entry_size
    if (end > length(central) || disk_start != 0L || bitwAnd(flags, 65L) != 0L ||
          needed >= 45L || compressed_size == 0xffffffff || uncompressed_size == 0xffffffff ||
          local_offset == 0xffffffff) {
      stop(.archive_parse_error("encrypted, ZIP64, or multi-disk ZIP entry"))
    }
    name_start <- position + 46L
    name_end <- name_start + name_size - 1L
    extra_start <- name_end + 1L
    extra_end <- extra_start + extra_size - 1L
    listed_name <- .archive_zip_normalize_name(.archive_zip_name(
      if (name_size > 0L) central[name_start:name_end] else raw()
    ), state$limits, portable_names = state$portable_names,
    is_windows = state$is_windows)
    relaxed_rules <- attr(listed_name, "archive_relaxed_rules")
    name <- if (endsWith(listed_name, "/")) {
      substr(listed_name, 1L, nchar(listed_name) - 1L)
    } else {
      listed_name
    }
    extras <- .archive_zip_extra(if (extra_size > 0L) central[extra_start:extra_end] else raw())
    if (length(extras) > 0L && any(vapply(extras, function(x) identical(x$id, 1), logical(1L)))) {
      stop(.archive_parse_error("ZIP64 extra field is not supported"))
    }

    if (origin != 0L && origin != 3L) stop(.archive_parse_error("ZIP entry origin cannot be interpreted safely"))
    mode <- as.integer(external[[3L]]) + 256L * as.integer(external[[4L]])
    mode_type <- if (origin == 3L) (mode %/% 4096L) * 4096L else 0L
    has_trailing_slash <- endsWith(listed_name, "/")
    dos_directory <- bitwAnd(
      as.integer(external[[1L]]) + 256L * as.integer(external[[2L]]), 16L
    ) != 0L
    unix_directory <- origin == 3L && mode_type == 16384L
    directory_mode <- dos_directory || unix_directory
    if (has_trailing_slash && (!directory_mode || uncompressed_size != 0)) {
      stop(.archive_parse_error(
        "ZIP directory names must have directory mode and zero size"
      ))
    }
    if (!has_trailing_slash && directory_mode) {
      stop(.archive_parse_error(
        "ZIP directory mode requires a trailing '/' in the entry name"
      ))
    }
    directory <- has_trailing_slash
    if (origin == 3L && !directory && !mode_type %in% c(0L, 32768L)) {
      .archive_link_abort(archive, name, "ZIP special entry")
    }
    if (origin == 3L && mode_type %in% c(40960L, 4096L, 8192L, 24576L, 49152L)) {
      .archive_link_abort(archive, name, "ZIP link or special entry")
    }
    if (!(method %in% c(0L, 8L))) {
      stop(.archive_parse_error("ZIP compression method is not supported"))
    }

    if (exists(name, envir = seen_names, inherits = FALSE)) {
      stop(.archive_parse_error("duplicate ZIP entry path"))
    }
    assign(name, TRUE, envir = seen_names)

    if (local_offset >= central_offset || local_offset + 30L > central_offset) {
      stop(.archive_parse_error("inconsistent ZIP local-file header"))
    }
    .archive_state_reserve_meta(state, 30L)
    local_header <- .archive_zip_read_at(connection, local_offset, 30L)
    if (.archive_u32(local_header[1L:4L]) != 0x04034b50) {
      stop(.archive_parse_error("inconsistent ZIP local-file header"))
    }
    local_name_size <- .archive_u16(local_header[27L:28L])
    local_extra_size <- .archive_u16(local_header[29L:30L])
    local_flags <- .archive_u16(local_header[7L:8L])
    local_method <- .archive_u16(local_header[9L:10L])
    .archive_state_reserve_meta(state, local_name_size + local_extra_size)
    local_data <- .archive_zip_read_at(
      connection, local_offset + 30L, local_name_size + local_extra_size
    )
    data_start <- local_offset + 30L + local_name_size + local_extra_size
    if (data_start + compressed_size > central_offset) {
      stop(.archive_parse_error("inconsistent ZIP compressed-data bounds"))
    }
    descriptor_size <- if (bitwAnd(flags, 8L) != 0L) {
      .archive_zip_descriptor(
        connection, data_start + compressed_size, central_offset,
        list(
          crc = crc, compressed_size = compressed_size,
          uncompressed_size = uncompressed_size
        )
      )
    } else {
      0L
    }
    if (data_start + compressed_size + descriptor_size > central_offset) {
      stop(.archive_parse_error("inconsistent ZIP data descriptor bounds"))
    }
    if (descriptor_size > 0L) .archive_state_reserve_meta(state, descriptor_size)
    intervals[[length(intervals) + 1L]] <- list(
      start = local_offset,
      end = data_start + compressed_size + descriptor_size
    )
    local_extras <- .archive_zip_extra(if (local_extra_size > 0L) {
      local_data[(local_name_size + 1L):(local_name_size + local_extra_size)]
    } else {
      raw()
    })
    if (bitwAnd(local_flags, 65L) != 0L || local_method != method ||
      (length(local_extras) > 0L && any(vapply(
        local_extras, function(x) identical(x$id, 1), logical(1L)
      )))) {
      stop(.archive_parse_error("inconsistent or encrypted ZIP local-file entry"))
    }
    local_name <- if (local_name_size > 0L) {
      .archive_zip_normalize_name(
        .archive_zip_name(local_data[seq_len(local_name_size)]), state$limits,
        portable_names = state$portable_names, is_windows = state$is_windows
      )
    } else {
      ""
    }
    if (!identical(local_name, listed_name)) {
      stop(.archive_parse_error("inconsistent ZIP local-file entry"))
    }
    local_crc <- .archive_u32(local_header[15L:18L])
    local_compressed_size <- .archive_u32(local_header[19L:22L])
    local_uncompressed_size <- .archive_u32(local_header[23L:26L])
    if (local_flags != flags || bitwAnd(flags, 8L) == 0L &&
          (local_crc != crc || local_compressed_size != compressed_size ||
             local_uncompressed_size != uncompressed_size)) {
      stop(.archive_parse_error("inconsistent ZIP local-file sizes or CRC-32"))
    }
    assign(as.character(entry_index), list(
      name = name, method = method, crc = crc,
      compressed_size = compressed_size, uncompressed_size = uncompressed_size,
      local_data_start = data_start, directory = directory, mode = mode,
      display = listed_name, relaxed_rules = relaxed_rules,
      descriptor_size = descriptor_size
    ), envir = entries)
    position <- end + 1L
  }
  if (position != length(central) + 1L) stop(.archive_parse_error("inconsistent ZIP central-directory size"))
  .archive_zip_intervals(intervals, central_offset)
  if (total_entries > 0L) {
    for (entry_index in seq_len(total_entries)) {
      entry <- get(as.character(entry_index), envir = entries)
      name <- entry$name
      .archive_state_note_relaxed(
        state, entry$display, entry$relaxed_rules
      )
      .archive_state_register_path(state, name)
      if (exists(name, envir = state$entries, inherits = FALSE)) {
        stop(.archive_parse_error("duplicate ZIP entry path"))
      }
      if (entry$directory && exists(name, envir = state$types, inherits = FALSE) &&
            identical(get(name, envir = state$types, inherits = FALSE), "file")) {
        stop(.archive_parse_error("ZIP directory replaces a file"))
      }
      .archive_tar_prepare_parent(extract_dir, name, state, archive)
      if (!entry$directory && exists(name, envir = state$types, inherits = FALSE) &&
            identical(get(name, envir = state$types, inherits = FALSE), "directory")) {
        stop(.archive_parse_error("ZIP file replaces an existing directory entry"))
      }
      if (entry$directory && entry$compressed_size != 0) {
        stop(.archive_parse_error("ZIP directory has compressed data"))
      }
      if (entry$directory && entry$crc != 0) {
        stop(.archive_parse_error("ZIP directory CRC-32 is not zero"))
      }
      .archive_state_add_member(
        state, name, entry$uncompressed_size, entry$display
      )
      .archive_state_reserve(state, entry$uncompressed_size)
      target <- if (is.null(extract_dir)) NULL else file.path(extract_dir, name)
      payload <- if (entry$directory) {
        raw()
      } else {
        .archive_zip_payload(connection, entry, state = state)
      }
      if (!is.null(target)) {
        if (entry$directory) {
          if (!exists(name, envir = state$types, inherits = FALSE) &&
                !dir.create(target, showWarnings = FALSE)) {
            stop(.archive_parse_error("could not create ZIP directory"))
          }
        } else {
          output <- tryCatch(file(target, open = "wb"), error = identity)
          if (inherits(output, "error")) {
            stop(.archive_parse_error(paste0(
              "could not open ZIP entry for writing: ", conditionMessage(output)
            )))
          }
          write_error <- tryCatch({
            if (length(payload) > 0L) writeBin(payload, output)
            close(output)
            NULL
          }, error = identity)
          if (inherits(write_error, "error")) {
            try(close(output), silent = TRUE)
            stop(.archive_parse_error(paste0(
              "could not write ZIP entry: ", conditionMessage(write_error)
            )))
          }
          .archive_zip_apply_mode(target, entry$mode, FALSE)
        }
      }
      assign(name, if (entry$directory) "directory" else "file", envir = state$types)
      assign(name, if (entry$directory) "directory" else "file", envir = state$entries)
      if (entry$directory) {
        .archive_state_add_directory(state, target, entry$mode, NULL)
      }
    }
  }
  if (!is.null(extract_dir) && state$directory_count > 0L) {
    for (index in seq_len(state$directory_count)) {
      directory <- get(as.character(index), envir = state$directories)
      .archive_zip_apply_mode(directory$path, directory$mode, TRUE)
    }
  }
  .archive_state_warn(state)
  result <- data.frame(
    Name = .archive_state_display(state),
    Length = .archive_state_lengths(state),
    stringsAsFactors = FALSE
  )
  attr(result, "archive_relaxed_names") <- .archive_state_relaxed_names(state)
  result
}

.archive_zip_inspect <- function(archive, is_windows = NULL,
                                 case_sensitive = NULL) {
  .archive_zip_walk(
    archive, is_windows = is_windows, case_sensitive = case_sensitive
  )
}

.archive_destination_is_empty <- function(path, is_windows = NULL,
                                          normalize = NULL, exists = NULL) {
  if (isTRUE(.path_is_symlink(
    path, is_windows = is_windows, normalize = normalize, exists = exists
  ))) {
    return(FALSE)
  }
  length(list.files(
    path, all.files = TRUE, no.. = TRUE, include.dirs = TRUE
  )) == 0L
}

.archive_cleanup_tree <- function(path, is_windows = NULL,
                                  normalize = NULL, exists = NULL) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(invisible(NULL))
  }
  if (isTRUE(.path_is_symlink(
    path, is_windows = is_windows, normalize = normalize, exists = exists
  ))) {
    unlink(path, recursive = FALSE, force = TRUE)
    return(invisible(NULL))
  }
  if (dir.exists(path)) {
    try(Sys.chmod(path, mode = "0700", use_umask = FALSE), silent = TRUE)
    children <- list.files(
      path, all.files = TRUE, no.. = TRUE, include.dirs = TRUE,
      full.names = TRUE
    )
    for (child in children) {
      .archive_cleanup_tree(
        child, is_windows = is_windows, normalize = normalize, exists = exists
      )
    }
    try(Sys.chmod(path, mode = "0700", use_umask = FALSE), silent = TRUE)
  } else if (file.exists(path)) {
    try(Sys.chmod(path, mode = "0600", use_umask = FALSE), silent = TRUE)
  }
  unlink(path, recursive = TRUE, force = TRUE)
  invisible(NULL)
}

.archive_format_wrap <- function(archive, error) {
  if (inherits(error, c(
    "bigbang_error_archive_link", "bigbang_error_archive_limit",
    "bigbang_error_archive_format", "bigbang_error_archive_portable_names"
  ))) {
    stop(error)
  }
  .archive_abort(
    "bigbang_error_archive_format",
    .bb_trf(
      "Could not extract archive %s safely: %s", archive,
      conditionMessage(error)
    ),
    archive = archive,
    entry = if (is.null(error$entry)) NULL else error$entry,
    reason = if (is.null(error$reason)) NULL else error$reason
  )
}

.archive_extract <- function(archive, ext, extract_dir, is_windows = NULL,
                             case_sensitive = NULL) {
  staging_dir <- NULL
  committed <- FALSE
  committed_paths <- character()
  on.exit({
    if (!isTRUE(committed)) {
      .archive_cleanup_tree(staging_dir)
      for (path in rev(committed_paths)) .archive_cleanup_tree(path)
    }
  }, add = TRUE)
  result <- tryCatch({
    if (!identical(.archive_tar_existing_kind(extract_dir), "directory") ||
          !.archive_destination_is_empty(extract_dir)) {
      stop(.archive_parse_error(
        "archive extraction directory must be a new empty directory"
      ))
    }
    staging_dir <- tempfile(".bigbang-archive-", tmpdir = extract_dir)
    if (!dir.create(staging_dir, mode = "0700")) {
      stop(.archive_parse_error("could not create private archive directory"))
    }
    inspection <- if (.archive_is_zip(ext)) {
      .archive_zip_walk(
        archive, extract_dir = staging_dir, is_windows = is_windows,
        case_sensitive = case_sensitive
      )
    } else if (.archive_is_tar(ext)) {
      .archive_tar_extract(
        archive, ext, staging_dir, is_windows = is_windows,
        case_sensitive = case_sensitive
      )
    } else {
      stop(.archive_parse_error("unsupported archive format"))
    }
    children <- list.files(
      staging_dir, all.files = TRUE, no.. = TRUE, include.dirs = TRUE,
      full.names = TRUE
    )
    targets <- file.path(extract_dir, basename(children))
    if (any(file.exists(targets) | dir.exists(targets))) {
      stop(.archive_parse_error("archive extraction destination changed during extraction"))
    }
    for (index in seq_along(children)) {
      if (!isTRUE(file.rename(children[[index]], targets[[index]]))) {
        stop(.archive_parse_error("could not commit archive extraction"))
      }
      committed_paths <- c(committed_paths, targets[[index]])
    }
    .archive_cleanup_tree(staging_dir)
    committed <- TRUE
    list(inspection = inspection)
  }, error = identity)
  if (inherits(result, "error")) .archive_format_wrap(archive, result)
  invisible(result)
}

.archive_inspect <- function(archive, ext, is_windows = NULL,
                             case_sensitive = NULL) {
  inspected <- tryCatch({
    if (.archive_is_zip(ext)) {
      .archive_zip_walk(
        archive, is_windows = is_windows, case_sensitive = case_sensitive
      )
    } else if (.archive_is_tar(ext)) {
      .archive_tar_inspect(
        archive, ext, is_windows = is_windows,
        case_sensitive = case_sensitive
      )
    } else {
      stop(.archive_parse_error("unsupported archive format"))
    }
  }, error = identity)
  if (inherits(inspected, "error")) {
    .archive_format_wrap(archive, inspected)
  }
  inspected
}

.archive_inspection_members <- function(inspection, ext) {
  if (.archive_is_zip(ext)) {
    inspection$Name
  } else {
    inspection
  }
}

.archive_relaxed_names <- function(inspection) {
  value <- attr(inspection, "archive_relaxed_names", exact = TRUE)
  if (is.null(value)) list(windows = character(), case = character()) else value
}
