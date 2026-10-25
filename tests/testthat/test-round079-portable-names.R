r079_raw <- function(text) charToRaw(enc2utf8(text))
r079_or_null <- function(x, y) if (is.null(x)) y else x

r079_tar_header <- function(name, size, type = "0", mode = 420L,
                            prefix = NULL) {
  field <- function(value, width) {
    value <- if (is.raw(value)) value else r079_raw(value)
    c(value, raw(width - length(value)))
  }
  octal <- function(value, width) field(sprintf(paste0("%0", width - 1L, "o"), value), width)
  header <- raw(512L)
  header[1:100] <- field(name, 100L)
  header[101:108] <- octal(mode, 8L)
  header[109:116] <- octal(0L, 8L)
  header[117:124] <- octal(0L, 8L)
  header[125:136] <- octal(size, 12L)
  header[137:148] <- octal(0L, 12L)
  header[149:156] <- field("        ", 8L)
  header[157] <- field(type, 1L)
  header[258:263] <- field("ustar", 6L)
  header[264:265] <- field("00", 2L)
  if (!is.null(prefix)) header[346:500] <- field(prefix, 155L)
  checksum <- sum(as.integer(header))
  header[149:156] <- c(r079_raw(sprintf("%06o", checksum)), as.raw(c(32L, 0L)))
  header
}

r079_pax_record <- function(key, value) {
  for (digits in 1:8) {
    size <- digits + 1L + nchar(key) + 1L + nchar(value) + 1L
    if (nchar(as.character(size)) == digits) {
      return(r079_raw(paste0(size, " ", key, "=", value, "\n")))
    }
  }
  stop("PAX record too long")
}

r079_crc_table <- local({
  table <- integer(256)
  for (i in 0:255) {
    value <- as.integer(i)
    for (bit in 1:8) {
      value <- if (bitwAnd(value, 1L) == 1L) {
        bitwXor(bitwShiftR(value, 1L), as.integer(-306674912L))
      } else {
        bitwShiftR(value, 1L)
      }
    }
    table[i + 1L] <- value
  }
  table
})

r079_crc32 <- function(bytes) {
  crc <- as.integer(-1L)
  for (byte in as.integer(bytes)) {
    index <- bitwAnd(bitwXor(crc, byte), 255L) + 1L
    crc <- bitwXor(bitwShiftR(crc, 8L), r079_crc_table[[index]])
  }
  crc <- bitwXor(crc, as.integer(-1L))
  if (crc < 0) crc + 2^32 else crc
}

r079_le16 <- function(value) as.raw(c(value %% 256, value %/% 256))
r079_le32 <- function(value) {
  as.raw(c(
    value %% 256, value %/% 256 %% 256, value %/% 65536 %% 256,
    value %/% 16777216 %% 256
  ))
}

r079_write_zip <- function(path, entries) {
  locals <- raw()
  centrals <- list()
  offsets <- numeric(length(entries))
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    name <- r079_raw(entry$name)
    data <- if (is.null(entry$data)) raw() else entry$data
    crc <- r079_crc32(data)
    offsets[[index]] <- length(locals)
    locals <- c(
      locals, r079_raw("PK\003\004"), r079_le16(20L), r079_le16(0L),
      r079_le16(0L), raw(4L), r079_le32(crc), r079_le32(length(data)),
      r079_le32(length(data)), r079_le16(length(name)), raw(2L), name, data
    )
    centrals[[index]] <- c(
      r079_raw("PK\001\002"), r079_le16(0x0314), r079_le16(20L),
      r079_le16(0L), r079_le16(0L), raw(4L), r079_le32(crc),
      r079_le32(length(data)), r079_le32(length(data)), r079_le16(length(name)),
      raw(8L), raw(4L), r079_le32(offsets[[index]]), name
    )
  }
  central <- if (length(centrals) == 0L) raw() else do.call(c, centrals)
  eocd <- c(
    r079_raw("PK\005\006"), raw(4L), r079_le16(length(entries)),
    r079_le16(length(entries)), r079_le32(length(central)), r079_le32(length(locals)),
    raw(2L)
  )
  writeBin(c(locals, central, eocd), path)
  path
}

r079_write_tar <- function(path, entries) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  for (entry in entries) {
    data <- if (is.null(entry$data)) raw() else entry$data
    if (!is.null(entry$pax)) {
      payload <- do.call(c, lapply(names(entry$pax), function(key) {
        r079_pax_record(key, entry$pax[[key]])
      }))
      writeBin(r079_tar_header("PaxHeaders.0", length(payload), "x"), connection)
      writeBin(payload, connection)
      padding <- (512L - length(payload) %% 512L) %% 512L
      if (padding > 0L) writeBin(raw(padding), connection)
    }
    if (!is.null(entry$gnu_name)) {
      payload <- c(r079_raw(entry$gnu_name), as.raw(0L))
      writeBin(r079_tar_header("gnu-long-name", length(payload), "L"), connection)
      writeBin(payload, connection)
      padding <- (512L - length(payload) %% 512L) %% 512L
      if (padding > 0L) writeBin(raw(padding), connection)
    }
    writeBin(r079_tar_header(
      entry$name, length(data), r079_or_null(entry$type, "0"),
      prefix = entry$prefix
    ), connection)
    if (length(data) > 0L) writeBin(data, connection)
    padding <- (512L - length(data) %% 512L) %% 512L
    if (padding > 0L) writeBin(raw(padding), connection)
  }
  writeBin(raw(1024L), connection)
  invisible(path)
}

# The archive is written byte by byte: on older Windows, writing R/aux.R to
# disk reaches the AUX device, so a tar built from files would lack the entry.
r079_archive_with_names <- function(root, names, package = "r079pkg") {
  archive <- file.path(root, paste0(package, ".tar"))
  member <- function(path, text) {
    list(name = paste0(package, "/", path), data = r079_raw(text))
  }
  r079_write_tar(archive, c(
    list(
      member("DESCRIPTION", paste0("Package: ", package, "\nVersion: 1.0.0\n")),
      member("NAMESPACE", "export(aux_value)\n")
    ),
    lapply(names, function(name) {
      member(paste0("R/", name), "aux_value <- function() 079\n")
    })
  ))
  archive
}

r079_capture_warnings <- function(expr) {
  captured <- list()
  value <- withCallingHandlers(
    force(expr),
    warning = function(condition) {
      captured <<- c(captured, list(condition))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = captured)
}

test_that("the default keeps every Windows-name rule strict and helpful", {
  skip_on_os("windows")
  root <- tempfile("round079-strict-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(
    root, c("aux.R", "colon:name.R", "trailing.")
  )
  withr::local_options(bigbang.archive.portable_names = TRUE)
  error <- expect_error(bigbang:::.archive_inspect(archive, ".tar"), class = "bigbang_error_archive_format")
  expect_match(conditionMessage(error), "pkg/R/aux.R", fixed = TRUE)
  expect_match(conditionMessage(error), "bigbang.archive.portable_names = FALSE", fixed = TRUE)
  for (name in c("pkg/R/aux.R", "pkg/R/colon:name.R", "pkg/R/trailing.")) {
    condition <- expect_error(
      bigbang:::.archive_validate_name_text(name, bigbang:::.archive_limit_options()),
      class = "bigbang_archive_parse_error"
    )
    expect_match(conditionMessage(condition), name, fixed = TRUE)
  }
})

test_that("FALSE relaxes Windows-only names on Linux, with a typed warning", {
  skip_on_os("windows")
  root <- tempfile("round079-relaxed-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(
    root, c("aux.R", "con.R", "colon:name.R", "question?.R", "trailing.")
  )
  withr::local_options(bigbang.archive.portable_names = FALSE)
  inspected <- r079_capture_warnings(bigbang:::.archive_inspect(archive, ".tar"))
  expect_length(inspected$warnings, 1L)
  expect_s3_class(inspected$warnings[[1L]], "bigbang_warning_archive_nonportable_name")
  expect_match(conditionMessage(inspected$warnings[[1L]]), "will not be installable on Windows", fixed = TRUE)
  destination <- file.path(root, "destination")
  dir.create(destination)
  extracted <- r079_capture_warnings(bigbang:::.archive_extract(archive, ".tar", destination))
  expect_length(extracted$warnings, 1L)
  expect_true(file.exists(file.path(destination, "r079pkg", "R", "aux.R")))
  expect_true(file.exists(file.path(destination, "r079pkg", "R", "con.R")))
  expect_true(file.exists(file.path(destination, "r079pkg", "R", "colon:name.R")))
  expect_true(file.exists(file.path(destination, "r079pkg", "R", "question?.R")))
  expect_true(file.exists(file.path(destination, "r079pkg", "R", "trailing.")))
})

test_that("portable-name warnings summarize entries beyond the first three", {
  skip_on_os("windows")
  root <- tempfile("round079-warning-summary-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(
    root, c("aux.R", "con.R", "colon:name.R", "question?.R", "trailing.")
  )
  withr::local_options(bigbang.archive.portable_names = FALSE)
  inspected <- r079_capture_warnings(bigbang:::.archive_inspect(archive, ".tar"))
  expect_length(inspected$warnings, 1L)
  expect_match(conditionMessage(inspected$warnings[[1L]]),
               "`r079pkg/R/aux.R`, `r079pkg/R/con.R`, `r079pkg/R/colon:name.R`",
               fixed = TRUE)
  expect_match(conditionMessage(inspected$warnings[[1L]]), "and 2 more", fixed = TRUE)

  generated <- new.env(parent = baseenv())
  generated$.meta_tr <- identity
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), generated)
  expect_identical(
    generated$.archive_warning_entries(c("a", "b", "c", "d", "e")),
    bigbang:::.archive_warning_entries(c("a", "b", "c", "d", "e"))
  )
})

test_that("case-only names follow injected filesystem detection", {
  skip_on_os("windows")
  root <- tempfile("round079-case-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "case.tar")
  r079_write_tar(archive, list(
    list(name = "pkg/Name.R", data = r079_raw("x")),
    list(name = "pkg/name.R", data = r079_raw("y"))
  ))
  withr::local_options(bigbang.archive.portable_names = FALSE)
  inspection <- r079_capture_warnings(bigbang:::.archive_inspect(
    archive, ".tar", is_windows = FALSE, case_sensitive = TRUE
  ))
  expect_length(inspection$warnings, 0L)
  expect_length(attr(inspection$value, "archive_relaxed_names")$case, 0L)
  accepted_destination <- file.path(root, "accepted")
  dir.create(accepted_destination)
  accepted <- r079_capture_warnings(bigbang:::.archive_extract(
    archive, ".tar", accepted_destination,
    is_windows = FALSE, case_sensitive = TRUE
  ))
  expect_length(accepted$warnings, 1L)
  expect_s3_class(accepted$warnings[[1L]], "bigbang_warning_archive_nonportable_name")
  rejected_destination <- file.path(root, "rejected")
  dir.create(rejected_destination)
  rejected <- expect_error(bigbang:::.archive_extract(
    archive, ".tar", rejected_destination,
    is_windows = FALSE, case_sensitive = FALSE
  ), class = "bigbang_error_archive_format")
  expect_match(conditionMessage(rejected), "does not distinguish case", fixed = TRUE)
  expect_no_match(conditionMessage(rejected), "portable_names", fixed = TRUE)
})

test_that("components made only of dots and spaces never relax", {
  root <- tempfile("round079-dot-space-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  withr::local_options(bigbang.archive.portable_names = FALSE)
  names <- c(".. ", "...", ".. .", ". ")
  for (name in names) {
    archive <- file.path(root, paste0("ustar-", which(names == name), ".tar"))
    r079_write_tar(archive, list(list(name = paste0("pkg/", name), data = r079_raw("x"))))
    expect_error(
      bigbang:::.archive_inspect(archive, ".tar"),
      class = "bigbang_error_archive_format", info = paste("USTAR", name)
    )
  }
  prefix <- file.path(root, "prefix.tar")
  r079_write_tar(prefix, list(list(name = ".. ", prefix = "pkg", data = r079_raw("x"))))
  expect_error(bigbang:::.archive_inspect(prefix, ".tar"),
               class = "bigbang_error_archive_format")
  pax <- file.path(root, "pax.tar")
  r079_write_tar(pax, list(list(
    name = "pkg/R/ok.R", pax = list(path = "pkg/.. "), data = r079_raw("x")
  )))
  expect_error(bigbang:::.archive_inspect(pax, ".tar"),
               class = "bigbang_error_archive_format")
  gnu <- file.path(root, "gnu.tar")
  r079_write_tar(gnu, list(list(
    name = "pkg/R/ok.R", gnu_name = "pkg/...", data = r079_raw("x")
  )))
  expect_error(bigbang:::.archive_inspect(gnu, ".tar"),
               class = "bigbang_error_archive_format")
  zip <- file.path(root, "dot-space.zip")
  r079_write_zip(zip, lapply(names, function(name) {
    list(name = paste0("pkg/", name), data = r079_raw("x"))
  }))
  expect_error(bigbang:::.archive_inspect(zip, ".zip"),
               class = "bigbang_error_archive_format")
})

test_that("Unicode case folds include sharp S and dotless i", {
  root <- tempfile("round079-unicode-case-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  withr::local_options(bigbang.archive.portable_names = TRUE)
  state <- bigbang:::.archive_state_new(
    "unicode", extract_dir = root, is_windows = FALSE, case_sensitive = FALSE
  )
  bigbang:::.archive_state_register_path(state, "pkg/\u1e9e.R")
  expect_error(
    bigbang:::.archive_state_register_path(state, "pkg/\u00df.R"),
    class = "bigbang_archive_parse_error"
  )
  state <- bigbang:::.archive_state_new(
    "unicode", extract_dir = root, is_windows = FALSE, case_sensitive = FALSE
  )
  bigbang:::.archive_state_register_path(state, "pkg/\u0131.R")
  expect_error(
    bigbang:::.archive_state_register_path(state, "pkg/i.R"),
    class = "bigbang_archive_parse_error"
  )
})

test_that("case probe is silent and conservative", {
  root <- tempfile("round079-case-probe-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  expect_false(bigbang:::.archive_fs_case_sensitive(
    root, file_create = function(path) TRUE,
    file_exists = function(path) FALSE
  ))
  read_only <- file.path(root, "read-only")
  dir.create(read_only)
  Sys.chmod(read_only, "0555")
  withr::defer(Sys.chmod(read_only, "0755"))
  # Root, as in the r-universe containers, can still write here.
  if (file.access(read_only, 2L) == 0L) {
    skip("The current user can write the read-only directory.")
  }
  warnings <- character()
  value <- withCallingHandlers(
    bigbang:::.archive_fs_case_sensitive(read_only),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(value)
  expect_length(warnings, 0L)
})

test_that("simulated Windows stays strict when the option is FALSE", {
  root <- tempfile("round079-windows-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(root, "aux.R")
  withr::local_options(bigbang.archive.portable_names = FALSE)
  expect_error(
    bigbang:::.archive_inspect(archive, ".tar", is_windows = TRUE),
    class = "bigbang_error_archive_format"
  )
})

test_that("security-critical names never relax", {
  withr::local_options(bigbang.archive.portable_names = FALSE)
  limits <- bigbang:::.archive_limit_options()
  invalid <- c(
    "pkg/R/a\\b.R", "pkg/R/..", "pkg//a.R", "pkg/R/../a.R", "/pkg/R/a.R",
    "pkg/R/a\001.R", "pkg/R/a\u0080.R"
  )
  for (name in invalid) {
    expect_error(
      bigbang:::.archive_validate_name_text(name, limits, is_windows = FALSE),
      class = "bigbang_archive_parse_error",
      info = name
    )
  }
  expect_error(
    bigbang:::.archive_validate_name_text(rawToChar(as.raw(255L)), limits),
    class = "bigbang_archive_parse_error"
  )
})

test_that("portable_names validates to its own translated error", {
  root <- tempfile("round079-option-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(root, "ok.R")
  withr::local_options(bigbang.archive.portable_names = "false")
  error <- expect_error(bigbang:::.archive_inspect(archive, ".tar"),
                        class = "bigbang_error_archive_portable_names")
  expect_match(conditionMessage(error), "TRUE or FALSE", fixed = TRUE)
})

test_that("the generated runtime copies the same name policy", {
  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(c(list(message = message, call = NULL), list(...)),
                   class = c(class, "bigbang_error", "error", "condition")))
  }
  generated$.meta_tr <- identity
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  generated$.archive_warning_state <- new.env(parent = emptyenv())
  generated$.archive_warning_state$active <- FALSE
  generated$.archive_warning_state$seen <- character()
  eval(parse(text = round075_archive_template()), generated)
  root <- tempfile("round079-runtime-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(root, "aux.R")
  withr::local_options(bigbang.archive.portable_names = FALSE)
  if (identical(.Platform$OS.type, "windows")) {
    # Windows cannot create aux.R, so the option never relaxes it there.
    expect_error(generated$.archive_inspect(archive, ".tar"),
                 class = "bigbang_error_archive_format")
  } else {
    expect_s3_class(
      r079_capture_warnings(generated$.archive_inspect(archive, ".tar"))$warnings[[1L]],
      "bigbang_warning_archive_nonportable_name"
    )
  }
  withr::local_options(bigbang.archive.portable_names = TRUE)
  expect_error(generated$.archive_inspect(archive, ".tar"),
               class = "bigbang_error_archive_format")
})

test_that("portable_names works end to end for a generated Linux metapackage", {
  skip_on_os("windows")
  # The local inspection/extraction test and the case-only generated-runtime
  # test below exercise the same warning propagation; keep this larger
  # package-installation fixture for NOT_CRAN coverage only.
  skip_on_cran()
  root <- tempfile("round079-e2e-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(root, "aux.R", package = "r079component")
  destination <- file.path(root, "generated")
  dir.create(destination)
  withr::local_options(bigbang.archive.portable_names = FALSE)
  generated <- r079_capture_warnings(create_metapackage(
    name = "r079meta", packages = archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  ))
  expect_true(any(vapply(
    generated$warnings,
    inherits, logical(1L), what = "bigbang_warning_archive_nonportable_name"
  )))
  meta_library <- file.path(root, "meta-library")
  component_library <- file.path(root, "component-library")
  dir.create(meta_library)
  dir.create(component_library)
  expect_no_warning(utils::install.packages(
    generated$value$path, repos = NULL, type = "source", lib = meta_library,
    quiet = TRUE
  ))
  runtime <- loadNamespace("r079meta", lib.loc = meta_library)
  installed <- r079_capture_warnings(runtime$r079meta_install(
    lib = component_library, verbose = FALSE, upgrade = "always"
  ))
  expect_length(installed$warnings, 1L)
  expect_s3_class(installed$warnings[[1L]], "bigbang_warning_archive_nonportable_name")
  expect_true("r079component" %in% names(installed$value$installed))
  component <- loadNamespace("r079component", lib.loc = component_library)
  expect_identical(component$aux_value(), 79)
  withr::local_options(bigbang.archive.portable_names = TRUE)
  strict <- expect_error(create_metapackage(
    name = "r079meta.strict", packages = archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  ), class = "bigbang_error_archive_format")
  expect_match(conditionMessage(strict), "bigbang.archive.portable_names = FALSE", fixed = TRUE)
})

test_that("case-only names install locally and through generated runtime", {
  skip_on_cran()
  skip_on_os("windows")
  root <- tempfile("round079-case-e2e-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  # macOS defaults to a case-insensitive file system, where the collision is
  # rejected even with the option; the injected-detection tests cover that.
  skip_if_not(
    bigbang:::.archive_fs_case_sensitive(root),
    "The temporary file system does not distinguish case."
  )
  archive <- file.path(root, "r079casecomponent_1.0.0.tar")
  r079_write_tar(archive, list(
    list(name = "r079casecomponent/DESCRIPTION",
         data = r079_raw("Package: r079casecomponent\nVersion: 1.0.0\n")),
    list(name = "r079casecomponent/NAMESPACE",
         data = r079_raw("export(lower_value, upper_value)\n")),
    list(name = "r079casecomponent/R/foo.R",
         data = r079_raw("lower_value <- function() 1\n")),
    list(name = "r079casecomponent/R/FOO.R",
         data = r079_raw("upper_value <- function() 2\n"))
  ))
  withr::local_options(bigbang.archive.portable_names = FALSE)
  scanned <- r079_capture_warnings(scan_bigbang_artifact(archive))
  expect_length(scanned$warnings, 1L)
  expect_true(length(scanned$value$portable_name_warnings$case) > 0L)
  local_library <- file.path(root, "local-library")
  dir.create(local_library)
  local <- r079_capture_warnings(install_local_pkg(
    archive, lib = local_library, verbose = FALSE, upgrade = "always"
  ))
  expect_length(local$warnings, 1L)
  expect_s3_class(local$warnings[[1L]], "bigbang_warning_archive_nonportable_name")
  expect_true("r079casecomponent" %in% rownames(installed.packages(lib.loc = local_library)))

  generated_destination <- file.path(root, "generated")
  dir.create(generated_destination)
  generated <- r079_capture_warnings(create_metapackage(
    name = "r079casemeta", packages = archive, dest_dir = generated_destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character()
  ))
  expect_true(any(vapply(
    generated$warnings, inherits, logical(1L),
    what = "bigbang_warning_archive_nonportable_name"
  )))
  meta_library <- file.path(root, "meta-library")
  runtime_library <- file.path(root, "runtime-library")
  dir.create(meta_library)
  dir.create(runtime_library)
  expect_no_warning(utils::install.packages(
    generated$value$path, repos = NULL, type = "source", lib = meta_library,
    quiet = TRUE
  ))
  runtime <- loadNamespace("r079casemeta", lib.loc = meta_library)
  installed <- r079_capture_warnings(runtime$r079casemeta_install(
    lib = runtime_library, verbose = FALSE, upgrade = "always"
  ))
  expect_length(installed$warnings, 1L)
  expect_s3_class(installed$warnings[[1L]], "bigbang_warning_archive_nonportable_name")
  expect_true(
    "r079casecomponent_1.0.0" %in% names(installed$value$installed),
    info = paste(capture.output(str(installed$value)), collapse = "\n")
  )
})

test_that("archive readers exercise low-level format and limit guards", {
  root <- tempfile("round080b-archive-guards-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  for (value in list(0, NA_real_, "one", Inf)) {
    withr::local_options(bigbang.archive.max_entries = value)
    expect_error(
      bigbang:::.archive_limit_options(), class = "bigbang_error_archive_limit"
    )
  }
  expect_false(bigbang:::.archive_fs_case_sensitive(root, is_windows = TRUE))
  withr::local_options(bigbang.archive.max_entries = NULL)
  state <- bigbang:::.archive_state_new("guards")
  expect_identical(bigbang:::.archive_state_members(state), character())
  expect_identical(bigbang:::.archive_state_lengths(state), numeric())
  expect_identical(bigbang:::.archive_state_display(state), character())
  expect_error(
    bigbang:::.archive_state_reserve(state, -1),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_state_reserve_meta(state, NA_real_),
    class = "bigbang_archive_parse_error"
  )

  expect_identical(
    bigbang:::.archive_read_exact(rawConnection(charToRaw("abc")), 0),
    raw()
  )
  connection <- rawConnection(charToRaw("abc"), open = "rb")
  expect_identical(bigbang:::.archive_read_exact(connection, 2),
                   charToRaw("ab"))
  expect_error(
    bigbang:::.archive_read_exact(connection, 2),
    class = "bigbang_archive_parse_error"
  )
  close(connection)
  expect_error(
    bigbang:::.archive_read_exact(rawConnection(raw()), -1),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_discard(rawConnection(raw()), -1),
    class = "bigbang_archive_parse_error"
  )
  expect_identical(
    bigbang:::.archive_cstring(as.raw(c(97L, 0L, 98L))), "a"
  )
  expect_error(
    bigbang:::.archive_number(raw()), class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_number(charToRaw("08")),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_number(charToRaw("777777777777777777777777")),
    class = "bigbang_archive_parse_error"
  )
  short_gzip <- file.path(root, "short.gz")
  writeBin(raw(10L), short_gzip)
  expect_error(
    bigbang:::.archive_gzip_member_trailer(short_gzip),
    class = "bigbang_archive_parse_error"
  )
  expect_match(
    bigbang:::.archive_gzip_trailer_message("crc error"),
    "validate gzip member trailer", fixed = TRUE
  )
  expect_match(
    bigbang:::.archive_gzip_trailer_message("broken stream"),
    "could not decompress TAR archive", fixed = TRUE
  )
  expect_identical(bigbang:::.archive_pax_time("123.5"), 123.5)
  expect_identical(bigbang:::.archive_pax_number("123", "size"), 123)

  regular_connection <- rawConnection(charToRaw("abc"), open = "rb")
  expect_identical(bigbang:::.archive_tar_read(regular_connection, 2),
                   charToRaw("ab"))
  bigbang:::.archive_tar_close(regular_connection)
  payload_connection <- rawConnection(charToRaw("xy"), open = "rb")
  payload_state <- bigbang:::.archive_state_new("payload")
  expect_identical(
    bigbang:::.archive_tar_payload(payload_connection, 2, keep = FALSE,
                                   state = payload_state),
    raw()
  )
  close(payload_connection)

  expect_identical(bigbang:::.archive_pax_records(raw()), list())
  pax_nul <- c(r079_raw("6 a="), as.raw(c(13L, 10L)))
  for (payload in list(
    r079_raw("x"), r079_raw("4abcd\n"), r079_raw("x a=1\n"),
    r079_raw("3 \n"), r079_raw("4 a\n"), pax_nul
  )) {
    expect_error(
      bigbang:::.archive_pax_records(payload),
      class = "bigbang_archive_parse_error"
    )
  }
  contradictory <- c(r079_pax_record("path", "a"), r079_pax_record("path", "b"))
  expect_error(
    bigbang:::.archive_pax_records(contradictory),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_validate_keys(list(data = "1")),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_number("-1", "size"),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_number(strrep("9", 20L), "size"),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_time("nope"), class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_time(strrep("9", 20L)),
    class = "bigbang_archive_parse_error"
  )
  expect_identical(
    as.character(bigbang:::.archive_tar_normalize_path("pkg/R/")), "pkg/R"
  )
  expect_invisible(bigbang:::.archive_validate_name_text(
    "pkg/R/", bigbang:::.archive_limit_options(), allow_dots = TRUE
  ))
  expect_error(
    bigbang:::.archive_pax_records(r079_raw("5 a=1\n")),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_tar_normalize_path("pkg/../foo.R"),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_tar_normalize_path("./"),
    class = "bigbang_archive_parse_error"
  )
  regular <- file.path(root, "regular")
  writeLines("x", regular)
  expect_identical(bigbang:::.archive_tar_existing_kind(regular), "file")
  expect_identical(
    bigbang:::.archive_tar_existing_kind(file.path(root, "missing")), ""
  )
  parent_state <- bigbang:::.archive_state_new("parent")
  assign("pkg", "symbolic link", envir = parent_state$types)
  expect_error(
    bigbang:::.archive_tar_prepare_parent(NULL, "pkg/file", parent_state, "a.tar"),
    class = "bigbang_error_archive_link"
  )
  blocked_parent <- file.path(root, "blocked-parent")
  writeLines("not a directory", blocked_parent)
  blocked_state <- bigbang:::.archive_state_new("parent")
  expect_error(
    bigbang:::.archive_tar_prepare_parent(
      root, "blocked-parent/file", blocked_state, "a.tar"
    ),
    class = "bigbang_archive_parse_error"
  )
  link_error <- expect_error(
    bigbang:::.archive_link_abort("a.tar", "", "link"),
    class = "bigbang_error_archive_link"
  )
  expect_identical(link_error$entry, "<unnamed entry>")

  valid_archive <- r079_archive_with_names(root, "ok.R", package = "r080barchive")
  extracted <- bigbang:::.archive_cache_extract(NULL, valid_archive, ".tar")
  withr::defer(unlink(extracted, recursive = TRUE, force = TRUE))
  expect_true(dir.exists(extracted))
  expect_true(file.exists(file.path(extracted, "r080barchive", "DESCRIPTION")))
  cache <- bigbang:::.archive_cache_new()
  testthat::local_mocked_bindings(
    .archive_extract = function(...) list(), .package = "bigbang"
  )
  cached <- bigbang:::.archive_cache_extract(cache, valid_archive, ".tar")
  expect_true(dir.exists(cached))
  bigbang:::.archive_cache_close(cache)

  expect_no_error(bigbang:::.validate_metapackage_name(
    "r080bmeta", list(list(package = "r080bcomponent", stem = "r080bcomponent_1.0.0"))
  ))
  expect_false(bigbang:::.path_is_windows_reparse_point(
    root, is_windows = TRUE, normalize = function(path) path
  ))
})

test_that("archive streams report decompression, checksum, and filesystem failures", {
  stream <- rawConnection(raw(), open = "rb")
  close(stream)
  tar_stream <- list(stream = stream, gzip = NULL)
  class(tar_stream) <- c("bigbang_tar_stream", "list")
  expect_error(
    bigbang:::.archive_tar_read(tar_stream, 1),
    class = "bigbang_archive_parse_error"
  )

  expect_error(
    bigbang:::.archive_tar_apply_mode_time("somewhere", 420, NULL),
    class = "bigbang_archive_parse_error"
  )
  sink <- bigbang:::.archive_crc32_sink()
  close(environment(sink$write)$stream)
  expect_error(sink$write(charToRaw("x")), class = "bigbang_archive_parse_error")
})

test_that("ZIP central-directory geometry and payload guards are exercised", {
  root <- tempfile("round080b-zip-geometry-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "entry.zip")
  r079_write_zip(archive, list(list(name = "pkg/file", data = r079_raw("x"))))
  mutate_eocd <- function(path, positions, values) {
    bytes <- readBin(path, "raw", file.info(path)$size)
    bytes[positions] <- values
    writeBin(bytes, path)
  }
  eocd <- file.info(archive)$size - 21L
  disk <- file.path(root, "multi-disk.zip")
  file.copy(archive, disk)
  mutate_eocd(disk, eocd + 4:5, r079_le16(1L))
  expect_error(
    bigbang:::.archive_inspect(disk, ".zip"),
    regexp = "multi-disk ZIP archives are not supported",
    class = "bigbang_error_archive_format"
  )
  zip64 <- file.path(root, "zip64.zip")
  file.copy(archive, zip64)
  mutate_eocd(zip64, eocd + 10:11, r079_le16(65535L))
  expect_error(bigbang:::.archive_inspect(zip64, ".zip"),
               class = "bigbang_error_archive_format")
  bounds <- file.path(root, "bounds.zip")
  file.copy(archive, bounds)
  mutate_eocd(bounds, eocd + 16:19, r079_le32(0L))
  expect_error(bigbang:::.archive_inspect(bounds, ".zip"),
               class = "bigbang_error_archive_format")
  central_bad <- file.path(root, "central-bad.zip")
  file.copy(archive, central_bad)
  bytes <- readBin(central_bad, "raw", file.info(central_bad)$size)
  central_start <- bigbang:::.archive_u32(
    bytes[(eocd + 16L):(eocd + 19L)]
  ) + 1L
  bytes[central_start] <- as.raw(0L)
  writeBin(bytes, central_bad)
  expect_error(bigbang:::.archive_inspect(central_bad, ".zip"),
               class = "bigbang_error_archive_format")
  origin_bad <- file.path(root, "origin-bad.zip")
  file.copy(archive, origin_bad)
  bytes <- readBin(origin_bad, "raw", file.info(origin_bad)$size)
  bytes[central_start + 4L:5L] <- r079_le16(0x0214)
  writeBin(bytes, origin_bad)
  expect_error(bigbang:::.archive_inspect(origin_bad, ".zip"),
               class = "bigbang_error_archive_format")
  deflate <- rawConnection(r079_raw("not deflate"), open = "rb")
  expect_error(
    bigbang:::.archive_zip_payload(
      deflate, list(method = 8L, compressed_size = 11L,
                    uncompressed_size = 1L, local_data_start = 0L, crc = 0L)
    ),
    class = "bigbang_archive_parse_error"
  )
  close(deflate)
})

test_that("ZIP helpers reject malformed records and preserve interval invariants", {
  expect_identical(
    bigbang:::.archive_zip_extra(as.raw(c(1L, 0L, 0L, 0L))),
    list(list(id = 1, data = raw()))
  )
  expect_identical(
    as.character(bigbang:::.archive_zip_normalize_name("pkg/file")), "pkg/file"
  )
  expect_match(
    bigbang:::.archive_zip_error_message(structure(
      list(message = "crc error"), class = c("error", "condition")
    )),
    "CRC-32 verification failed", fixed = TRUE
  )
  expect_identical(
    bigbang:::.archive_zip_error_message("ordinary detail"), "ordinary detail"
  )
  expect_error(
    bigbang:::.archive_zip_extra(as.raw(c(1L, 0L, 2L))),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_extra(as.raw(c(1L, 0L, 2L, 0L, 7L))),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_name(as.raw(c(65L, 0L))),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_read_at(rawConnection(charToRaw("x")), -1, 1),
    class = "bigbang_archive_parse_error"
  )
  connection <- rawConnection(charToRaw("x"), open = "rb")
  expect_error(
    bigbang:::.archive_zip_read_at(connection, 0, 2),
    class = "bigbang_archive_parse_error"
  )
  close(connection)
  expect_error(
    bigbang:::.archive_zip_descriptor(
      rawConnection(raw()), 0, 10, list(crc = 1, compressed_size = 1,
                                        uncompressed_size = 1)
    ),
    class = "bigbang_archive_parse_error"
  )
  descriptor_value <- 0x08074b50
  descriptor <- rawConnection(
    as.raw(rep(c(0x50L, 0x4bL, 0x07L, 0x08L), 4L)), open = "rb"
  )
  expect_identical(
    bigbang:::.archive_zip_descriptor(
      descriptor, 0, 16,
      list(crc = descriptor_value, compressed_size = descriptor_value,
           uncompressed_size = descriptor_value)
    ),
    16L
  )
  close(descriptor)
  expect_silent(bigbang:::.archive_zip_intervals(list(), 0))
  expect_error(
    bigbang:::.archive_zip_intervals(list(), 1),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_intervals(list(list(start = 1, end = 2)), 0),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_intervals(list(list(start = 0, end = 0)), 1),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_intervals(list(
      list(start = 0, end = 3), list(start = 2, end = 4)
    ), 4),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_intervals(list(
      list(start = 0, end = 2), list(start = 3, end = 4)
    ), 4),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_zip_intervals(list(list(start = 0, end = 2)), 3),
    class = "bigbang_archive_parse_error"
  )
  stored <- rawConnection(charToRaw("x"), open = "rb")
  expect_error(
    bigbang:::.archive_zip_payload(
      stored, list(method = 0L, compressed_size = 1, uncompressed_size = 2,
                   local_data_start = 0, crc = 0)
    ),
    class = "bigbang_archive_parse_error"
  )
  close(stored)
  unsupported <- rawConnection(charToRaw("x"), open = "rb")
  expect_error(
    bigbang:::.archive_zip_payload(
      unsupported, list(method = 99L, compressed_size = 1, uncompressed_size = 1,
                        local_data_start = 0, crc = 0)
    ),
    class = "bigbang_archive_parse_error"
  )
  close(unsupported)
})

test_that("archive rejection primitives cover malformed metadata and limits", {
  limits <- bigbang:::.archive_limit_options()
  expect_no_error(bigbang:::.archive_validate_name_text(
    "pkg/", limits, allow_dots = TRUE
  ))
  connection <- rawConnection(raw(), "rb")
  withr::defer(close(connection))
  expect_error(
    bigbang:::.archive_read_exact(connection, 1L),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_read_exact(connection, -1),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_discard(connection, -1),
    class = "bigbang_archive_parse_error"
  )
  state <- bigbang:::.archive_state_new("limits.tar", is_windows = TRUE)
  state$limits$max_entries <- 0
  expect_error(
    bigbang:::.archive_state_count_entry(state),
    class = "bigbang_error_archive_limit"
  )
  state <- bigbang:::.archive_state_new("limits.tar", is_windows = TRUE)
  state$limits$max_total_bytes <- 0
  expect_error(
    bigbang:::.archive_state_reserve(state, 1),
    class = "bigbang_error_archive_limit"
  )
  state <- bigbang:::.archive_state_new("limits.tar", is_windows = TRUE)
  state$limits$max_metadata_bytes <- 0
  expect_error(
    bigbang:::.archive_state_reserve_meta(state, 1),
    class = "bigbang_error_archive_limit"
  )
  for (value in list(NA_real_, 0, Inf, "bad", c(1, 2))) {
    withr::local_options(setNames(list(value), "bigbang.archive.max_entries"))
    expect_error(
      bigbang:::.archive_limit_options(), class = "bigbang_error_archive_limit"
    )
  }
  withr::local_options(bigbang.archive.max_entries = NULL)

  expect_error(
    bigbang:::.archive_number(raw(), 8L),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_number(as.raw(128L), 8L),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_number(charToRaw("8"), 8L),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_number(charToRaw(paste(rep("7", 30), collapse = "")), 8L),
    class = "bigbang_archive_parse_error"
  )
  bad_pax <- function(text) {
    expect_error(
      bigbang:::.archive_pax_records(charToRaw(text)),
      class = "bigbang_archive_parse_error"
    )
  }
  bad_pax_raw <- function(bytes) {
    expect_error(
      bigbang:::.archive_pax_records(bytes),
      class = "bigbang_archive_parse_error"
    )
  }
  bad_pax("12 a=1")
  bad_pax("6a=1\n")
  bad_pax("x a=1\n")
  bad_pax("5 a=1\n")
  bad_pax_raw(as.raw(c(charToRaw("6 a="), 13L, charToRaw("\n"))))
  bad_pax("6 =1\n")
  bad_pax("6 a=1\n6 a=2\n")
  expect_error(
    bigbang:::.archive_pax_validate_keys(list(data = "1")),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_number("bad", "size"),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_number(strrep("9", 30L), "size"),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_time("bad"), class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_pax_time(strrep("9", 30L)),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_tar_normalize_path("../file", limits),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_tar_normalize_path("/", limits),
    class = "bigbang_archive_parse_error"
  )

  empty_state <- bigbang:::.archive_state_new("empty.tar", is_windows = TRUE)
  expect_identical(bigbang:::.archive_state_members(empty_state), character())
  expect_identical(bigbang:::.archive_state_lengths(empty_state), numeric())
  expect_identical(bigbang:::.archive_state_display(empty_state), character())
  expect_identical(
    bigbang:::.archive_tar_existing_kind("not-present", is_windows = TRUE), ""
  )
  parent_state <- bigbang:::.archive_state_new("parent.tar", is_windows = TRUE)
  assign("link", "symbolic link", envir = parent_state$types)
  expect_error(
    bigbang:::.archive_tar_prepare_parent(
      NULL, "link/file", parent_state, "parent.tar"
    ),
    class = "bigbang_error_archive_link"
  )
  assign("file", "file", envir = parent_state$types)
  expect_error(
    bigbang:::.archive_tar_prepare_parent(
      NULL, "file/child", parent_state, "parent.tar"
    ),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_tar_apply_mode_time("not-present", 420L, NULL),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_tar_apply_mode_time("not-present", 420L,
                                           as.POSIXct("2020-01-01", tz = "UTC")),
    class = "bigbang_archive_parse_error"
  )
  expect_error(
    bigbang:::.archive_gzip_member_trailer(tempfile("missing-gzip-")),
    class = "bigbang_archive_parse_error"
  )
  expect_identical(
    bigbang:::.archive_gzip_trailer_message("crc error"),
    "could not validate gzip member trailer"
  )
  expect_match(
    bigbang:::.archive_gzip_trailer_message("other decompression detail"),
    "could not decompress TAR archive",
    fixed = TRUE
  )
})

test_that("malformed TAR and ZIP structures are rejected before extraction", {
  root <- tempfile("round080b-archive-structures-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  write_blocks <- function(path, blocks) {
    connection <- file(path, open = "wb")
    on.exit(close(connection), add = TRUE)
    for (block in blocks) {
      payload <- if (is.null(block$data)) raw() else block$data
      writeBin(r079_tar_header(
        block$name, length(payload), r079_or_null(block$type, "0")
      ), connection)
      if (length(payload) > 0L) writeBin(payload, connection)
      padding <- (512L - length(payload) %% 512L) %% 512L
      if (padding > 0L) writeBin(raw(padding), connection)
    }
    writeBin(raw(1024L), connection)
    path
  }

  truncated <- file.path(root, "truncated.tar")
  writeBin(raw(100L), truncated)
  expect_error(
    bigbang:::.archive_inspect(truncated, ".tar"),
    class = "bigbang_error_archive_format"
  )
  no_end <- file.path(root, "no-end.tar")
  writeBin(r079_tar_header("pkg/file", 0), no_end)
  expect_error(
    bigbang:::.archive_inspect(no_end, ".tar"),
    class = "bigbang_error_archive_format"
  )
  trailing <- file.path(root, "trailing.tar")
  writeBin(c(raw(1024L), as.raw(1L), raw(511L)), trailing)
  expect_error(
    bigbang:::.archive_inspect(trailing, ".tar"),
    class = "bigbang_error_archive_format"
  )
  truncated_end <- file.path(root, "truncated-end.tar")
  writeBin(c(raw(512L), as.raw(1L), raw(511L)), truncated_end)
  expect_error(
    bigbang:::.archive_inspect(truncated_end, ".tar"),
    class = "bigbang_error_archive_format"
  )
  metadata_orphan <- file.path(root, "metadata-orphan.tar")
  write_blocks(metadata_orphan, list(
    list(name = "PaxHeaders.0", type = "x", data = r079_pax_record("path", "pkg/file"))
  ))
  expect_error(
    bigbang:::.archive_inspect(metadata_orphan, ".tar"),
    class = "bigbang_error_archive_format"
  )
  global_pax <- file.path(root, "global-pax.tar")
  write_blocks(global_pax, list(
    list(name = "GlobalHeader", type = "g", data = r079_pax_record("path", "pkg/file")),
    list(name = "ignored", data = r079_raw("x"))
  ))
  expect_identical(
    as.character(bigbang:::.archive_inspect(global_pax, ".tar")), "pkg/file"
  )
  two_long_names <- file.path(root, "two-long-names.tar")
  write_blocks(two_long_names, list(
    list(name = "gnu-one", type = "L", data = c(r079_raw("pkg/one"), as.raw(0L))),
    list(name = "gnu-two", type = "L", data = c(r079_raw("pkg/two"), as.raw(0L))),
    list(name = "ignored", data = r079_raw("x"))
  ))
  expect_error(
    bigbang:::.archive_inspect(two_long_names, ".tar"),
    class = "bigbang_error_archive_format"
  )
  contradictory_names <- file.path(root, "contradictory-names.tar")
  write_blocks(contradictory_names, list(
    list(name = "PaxHeaders.0", type = "x", data = r079_pax_record("path", "pkg/pax")),
    list(name = "gnu-name", type = "L", data = c(r079_raw("pkg/gnu"), as.raw(0L))),
    list(name = "ignored", data = r079_raw("x"))
  ))
  expect_error(
    bigbang:::.archive_inspect(contradictory_names, ".tar"),
    class = "bigbang_error_archive_format"
  )
  pax_link <- file.path(root, "pax-link.tar")
  write_blocks(pax_link, list(
    list(name = "PaxHeaders.0", type = "x", data = r079_pax_record("linkpath", "pkg/target")),
    list(name = "ignored", data = r079_raw("x"))
  ))
  expect_error(
    bigbang:::.archive_inspect(pax_link, ".tar"),
    class = "bigbang_error_archive_link"
  )
  trailing_file <- file.path(root, "trailing-file.tar")
  write_blocks(trailing_file, list(
    list(name = "pkg/file/", data = r079_raw("x"))
  ))
  expect_error(
    bigbang:::.archive_inspect(trailing_file, ".tar"),
    class = "bigbang_error_archive_format"
  )
  directory_over_file <- file.path(root, "directory-over-file.tar")
  write_blocks(directory_over_file, list(
    list(name = "pkg", data = r079_raw("x")),
    list(name = "pkg", type = "5")
  ))
  expect_error(
    bigbang:::.archive_inspect(directory_over_file, ".tar"),
    class = "bigbang_error_archive_format"
  )
  directory_mtime <- file.path(root, "directory-mtime.tar")
  write_blocks(directory_mtime, list(
    list(name = "PaxHeaders.0", type = "x", data = r079_pax_record("mtime", "123.5")),
    list(name = "pkg", type = "5")
  ))
  expect_identical(
    as.character(bigbang:::.archive_inspect(directory_mtime, ".tar")), "pkg"
  )
  parent <- file.path(root, "parent.tar")
  r079_write_tar(parent, list(list(name = "pkg/../file", data = r079_raw("x"))))
  expect_error(
    bigbang:::.archive_inspect(parent, ".tar"),
    class = "bigbang_error_archive_format"
  )
  duplicate <- file.path(root, "duplicate.tar")
  r079_write_tar(duplicate, list(
    list(name = "pkg/file", data = r079_raw("a")),
    list(name = "pkg/file", data = r079_raw("b"))
  ))
  expect_error(
    bigbang:::.archive_inspect(duplicate, ".tar"),
    class = "bigbang_error_archive_format"
  )

  zip <- file.path(root, "duplicate.zip")
  r079_write_zip(zip, list(
    list(name = "pkg/file", data = r079_raw("a")),
    list(name = "pkg/file", data = r079_raw("b"))
  ))
  expect_error(
    bigbang:::.archive_inspect(zip, ".zip"),
    class = "bigbang_error_archive_format"
  )
  broken_zip <- file.path(root, "broken.zip")
  writeBin(r079_raw("not a zip"), broken_zip)
  expect_error(
    bigbang:::.archive_inspect(broken_zip, ".zip"),
    class = "bigbang_error_archive_format"
  )
  bad_method <- file.path(root, "method.zip")
  r079_write_zip(bad_method, list(list(name = "pkg/file", data = r079_raw("x"))))
  bytes <- readBin(bad_method, "raw", n = file.info(bad_method)$size)
  bytes[9:10] <- r079_le16(99L)
  # The local-file mutation is enough to exercise the local/central consistency guard.
  writeBin(bytes, bad_method)
  expect_error(
    bigbang:::.archive_inspect(bad_method, ".zip"),
    class = "bigbang_error_archive_format"
  )
})

test_that("runtime qualification leaves unassigned functions and call-free code intact", {
  anonymous <- "function() base::identity(1)"
  call_free <- "value <- function() 1L"
  expect_identical(
    bigbang:::.qualify_generated_runtime_calls(anonymous), anonymous
  )
  expect_identical(
    bigbang:::.qualify_generated_runtime_calls(call_free), call_free
  )
})

test_that("scanner retries extraction after a deferred case collision", {
  root <- tempfile("round080b-scan-case-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(root, "value.R", package = "r080bscan")
  extracted <- file.path(root, "extracted")
  dir.create(file.path(extracted, "r080bscan", "R"), recursive = TRUE)
  writeLines(c(
    "Package: r080bscan", "Version: 1.0.0", "Title: Scan fixture",
    "Description: Scanner retry fixture.", "License: MIT"
  ), file.path(extracted, "r080bscan", "DESCRIPTION"))
  writeLines("export(value)", file.path(extracted, "r080bscan", "NAMESPACE"))
  writeLines("value <- function() 1L", file.path(extracted, "r080bscan", "R", "value.R"))
  listing_calls <- 0L
  inspection <- structure(
    c("r080bscan/DESCRIPTION", "r080bscan/NAMESPACE", "r080bscan/R/value.R"),
    archive_relaxed_names = list(windows = character(), case = "r080bscan/R/Value.R")
  )
  case_error <- structure(
    list(message = "case collision", call = NULL, reason = "case"),
    class = c("bigbang_error_archive_format", "bigbang_error", "error", "condition")
  )
  withr::local_options(bigbang.archive.portable_names = FALSE)
  testthat::local_mocked_bindings(
    .archive_cache_listing = function(...) {
      listing_calls <<- listing_calls + 1L
      if (listing_calls == 1L) stop(case_error)
      inspection
    },
    .archive_cache_extract = function(...) extracted,
    .package = "bigbang"
  )
  scanned <- bigbang:::.scan_source_archive(archive)
  expect_identical(scanned$provenance$Package, "r080bscan")
  expect_equal(listing_calls, 2L)
})

test_that("scanner retains archive-link and extraction failures", {
  root <- tempfile("round080c-scanner-errors-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r079_archive_with_names(root, "value.R", package = "r080cscan")

  link <- structure(
    list(
      message = "archive link",
      call = NULL,
      archive = archive,
      entry = "r080cscan/R/value.R",
      entry_type = "symbolic link"
    ),
    class = c("bigbang_error_archive_link", "bigbang_error", "error", "condition")
  )
  extraction_error <- link
  testthat::local_mocked_bindings(
    .archive_cache_listing = function(...) {
      stop(structure(
        list(message = "case collision", call = NULL, reason = "case"),
        class = c("bigbang_error_archive_format", "bigbang_error",
                  "error", "condition")
      ))
    },
    .archive_cache_extract = function(...) stop(extraction_error),
    .package = "bigbang"
  )
  withr::local_options(bigbang.archive.portable_names = FALSE)
  linked <- bigbang:::.scan_source_archive(archive)
  expect_s3_class(linked$archive_issue, "bigbang_error_archive_link")

  extraction <- structure(
    list(message = "extract failed", call = NULL),
    class = c("simpleError", "error", "condition")
  )
  extraction_error <- extraction
  expect_error(bigbang:::.scan_source_archive(archive), "extract failed")
})

test_that("archive rejection families preserve their specific diagnostics", {
  root <- tempfile("round080c-archive-rejections-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  malformed_gzip <- file.path(root, "malformed.tar.gz")
  writeBin(charToRaw("not-a-gzip-stream-with-enough-bytes"), malformed_gzip)
  gzip_stream <- bigbang:::.archive_tar_connection(malformed_gzip, ".tar.gz")
  expect_error(
    bigbang:::.archive_tar_finish(gzip_stream),
    class = "bigbang_archive_parse_error"
  )
  bigbang:::.archive_tar_close(gzip_stream)

  expect_error(
    bigbang:::.archive_u32(as.raw(rep(255L, 8L))),
    class = "bigbang_archive_parse_error"
  )
  crc <- bigbang:::.archive_crc32_sink()
  close(environment(crc$write)$stream)
  expect_error(crc$write(r079_raw("x")),
               class = "bigbang_archive_parse_error")
  crc_missing <- bigbang:::.archive_crc32_sink()
  try(suppressWarnings(close(environment(crc_missing$write)$stream)),
      silent = TRUE)
  environment(crc_missing$finish)$state$closed <- TRUE
  unlink(crc_missing$path, force = TRUE)
  expect_error(crc_missing$finish(), class = "bigbang_archive_parse_error")

  pax_link <- file.path(root, "pax-link.tar")
  r079_write_tar(pax_link, list(list(
    name = "pkg/R/value.R", pax = list(linkpath = "outside"),
    data = r079_raw("value <- 1L\n")
  )))
  expect_error(
    bigbang:::.archive_inspect(pax_link, ".tar"),
    class = "bigbang_error_archive_link"
  )
  trailing_file <- file.path(root, "trailing-file.tar")
  r079_write_tar(trailing_file, list(list(
    name = "pkg/R/value.R/", data = r079_raw("value <- 1L\n")
  )))
  expect_error(
    bigbang:::.archive_inspect(trailing_file, ".tar"),
    regexp = "ending in '/'.*not a directory",
    class = "bigbang_error_archive_format"
  )

  zip <- file.path(root, "stored.zip")
  r079_write_zip(zip, list(list(name = "pkg/value.R", data = r079_raw("x"))))
  destination <- file.path(root, "destination")
  dir.create(destination)
  expect_no_error(bigbang:::.archive_extract(zip, ".zip", destination))
  expect_true(file.exists(file.path(destination, "pkg", "value.R")))
})
