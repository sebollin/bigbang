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

r079_archive_with_names <- function(root, names, package = "r079pkg") {
  source <- file.path(root, package)
  dir.create(file.path(source, "R"), recursive = TRUE)
  writeLines(
    c(paste0("Package: ", package), "Version: 1.0.0"),
    file.path(source, "DESCRIPTION")
  )
  writeLines("export(aux_value)", file.path(source, "NAMESPACE"))
  for (name in names) writeLines("aux_value <- function() 079", file.path(source, "R", name))
  archive <- file.path(root, paste0(package, ".tar"))
  withr::with_dir(root, utils::tar(archive, files = package, compression = "none", tar = "internal"))
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
  read_only <- file.path(root, "read-only")
  dir.create(read_only)
  Sys.chmod(read_only, "0555")
  warnings <- character()
  value <- withCallingHandlers(
    bigbang:::.archive_fs_case_sensitive(read_only),
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      invokeRestart("muffleWarning")
    }
  )
  Sys.chmod(read_only, "0755")
  expect_false(value)
  expect_length(warnings, 0L)
  expect_false(bigbang:::.archive_fs_case_sensitive(
    root, file_create = function(path) TRUE,
    file_exists = function(path) FALSE
  ))
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
  expect_s3_class(
    r079_capture_warnings(generated$.archive_inspect(archive, ".tar"))$warnings[[1L]],
    "bigbang_warning_archive_nonportable_name"
  )
  withr::local_options(bigbang.archive.portable_names = TRUE)
  expect_error(generated$.archive_inspect(archive, ".tar"),
               class = "bigbang_error_archive_format")
})

test_that("portable_names works end to end for a generated Linux metapackage", {
  skip_on_os("windows")
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
  skip_on_os("windows")
  root <- tempfile("round079-case-e2e-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
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
