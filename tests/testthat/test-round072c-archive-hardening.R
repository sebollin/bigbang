t072c_or_null <- function(x, y) if (is.null(x)) y else x
`%||%` <- t072c_or_null

t072c_raw <- function(text) charToRaw(enc2utf8(text))

t072c_field <- function(value, width) {
  value <- if (is.raw(value)) value else t072c_raw(value)
  if (length(value) > width) stop("fixture field is too long")
  c(value, raw(width - length(value)))
}

t072c_octal <- function(value, width) {
  t072c_field(sprintf(paste0("%0", width - 1L, "o"), value), width)
}

t072c_tar_header <- function(name, size, type = "0", mode = 420L) {
  header <- raw(512L)
  header[1:100] <- t072c_field(name, 100L)
  header[101:108] <- t072c_octal(mode, 8L)
  header[109:116] <- t072c_octal(0L, 8L)
  header[117:124] <- t072c_octal(0L, 8L)
  header[125:136] <- t072c_octal(size, 12L)
  header[137:148] <- t072c_octal(0L, 12L)
  header[149:156] <- t072c_field("        ", 8L)
  header[157] <- t072c_raw(type)
  header[258:263] <- t072c_field("ustar", 6L)
  header[264:265] <- t072c_raw("00")
  header[149:156] <- c(
    t072c_raw(sprintf("%06o", sum(as.integer(header)))), as.raw(c(32L, 0L))
  )
  header
}

t072c_write_tar <- function(path, entries) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  for (entry in entries) {
    data <- if (is.null(entry$data)) raw() else entry$data
    header <- t072c_tar_header(
      if (is.null(entry$name_raw)) entry$name else entry$name_raw,
      length(data), entry$type %||% "0", entry$mode %||% 420L
    )
    writeBin(header, connection)
    if (length(data) > 0L) writeBin(data, connection)
    padding <- (512L - length(data) %% 512L) %% 512L
    if (padding > 0L) writeBin(raw(padding), connection)
  }
  writeBin(raw(1024L), connection)
  invisible(path)
}

t072c_le16 <- function(value) as.raw(c(value %% 256, value %/% 256))
t072c_le32 <- function(value) {
  as.raw(c(
    value %% 256, value %/% 256 %% 256, value %/% 65536 %% 256,
    value %/% 16777216 %% 256
  ))
}

t072c_crc32 <- function(bytes) {
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
  crc <- -1L
  for (byte in as.integer(bytes)) {
    index <- bitwAnd(bitwXor(crc, byte), 255L) + 1L
    crc <- bitwXor(bitwShiftR(crc, 8L), table[[index]])
  }
  crc <- bitwXor(crc, -1L)
  if (crc < 0) crc <- crc + 4294967296
  crc
}

t072c_zip_write <- function(path, entries, comment = raw()) {
  local <- raw()
  central <- raw()
  offset <- 0L
  for (entry in entries) {
    name <- if (is.null(entry$name_raw)) t072c_raw(entry$name) else entry$name_raw
    data <- if (is.null(entry$data)) raw() else entry$data
    method <- entry$method %||% 0L
    compressed <- if (method == 8L) {
      gzip <- memCompress(data, type = "gzip")
      # R returns a zlib-wrapped deflate stream for memCompress("gzip").
      gzip[3L:(length(gzip) - 4L)]
    } else {
      data
    }
    crc <- if (isTRUE(entry$bad_crc)) 0 else t072c_crc32(data)
    mode <- entry$mode %||% 33188L
    dos <- if (isTRUE(entry$dos_directory)) c(16L, 0L) else c(0L, 0L)
    external <- as.raw(c(dos, mode %% 256L, mode %/% 256L %% 256L))
    local <- c(
      local, t072c_raw("PK\003\004"), t072c_le16(20L), t072c_le16(0L),
      t072c_le16(method), raw(4L), t072c_le32(crc),
      t072c_le32(length(compressed)), t072c_le32(length(data)),
      t072c_le16(length(name)), t072c_le16(0L), name, compressed
    )
    central <- c(
      central, t072c_raw("PK\001\002"), t072c_le16(0x0314), t072c_le16(20L),
      t072c_le16(0L), t072c_le16(method), raw(4L), t072c_le32(crc),
      t072c_le32(length(compressed)), t072c_le32(length(data)),
      t072c_le16(length(name)), raw(8L), external, t072c_le32(offset), name
    )
    offset <- length(local)
  }
  eocd <- c(
    t072c_raw("PK\005\006"), raw(4L), t072c_le16(length(entries)),
    t072c_le16(length(entries)), t072c_le32(length(central)),
    t072c_le32(length(local)), t072c_le16(length(comment)), comment
  )
  writeBin(c(local, central, eocd), path)
  invisible(path)
}

t072c_verdict <- function(fun, ...) {
  value <- tryCatch(fun(...), error = identity)
  if (inherits(value, "error")) {
    list(ok = FALSE, class = class(value)[[1L]], message = conditionMessage(value))
  } else {
    list(ok = TRUE, class = NA_character_, value = value)
  }
}

test_that("R29 geometry traps use the same tar and ZIP walk", {
  root <- tempfile("round072c-geometry-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  extract <- get(".archive_extract", asNamespace("bigbang"))

  parent_file <- file.path(root, "parent-file.tar")
  t072c_write_tar(parent_file, list(
    list(name = "pkg/a/b", data = t072c_raw("child")),
    list(name = "pkg/a", data = t072c_raw("parent"))
  ))
  inspected <- t072c_verdict(reader, parent_file, ".tar")
  destination <- file.path(root, "parent-file-extracted")
  dir.create(destination)
  extracted <- t072c_verdict(extract, parent_file, ".tar", destination)
  expect_false(inspected$ok)
  expect_false(extracted$ok)
  expect_identical(inspected$class, extracted$class)
  expect_match(inspected$message, "existing directory entry")

  dir_with_data <- file.path(root, "dir-with-data.zip")
  t072c_zip_write(dir_with_data, list(
    list(name = "pkg/", mode = 16877L, data = t072c_raw("lost"))
  ))
  expect_false(t072c_verdict(reader, dir_with_data, ".zip")$ok)

  dos_without_slash <- file.path(root, "dos-without-slash.zip")
  t072c_zip_write(dos_without_slash, list(
    list(name = "pkg/dir", dos_directory = TRUE)
  ))
  expect_false(t072c_verdict(reader, dos_without_slash, ".zip")$ok)
})

test_that("ZIP extraction is own stored/deflate code with CRC verification", {
  root <- tempfile("round072c-zip-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "payload.zip")
  t072c_zip_write(archive, list(
    list(name = "pkg/", mode = 16877L),
    list(name = "pkg/stored.txt", data = t072c_raw("stored")),
    list(name = "pkg/deflated.txt", data = t072c_raw(strrep("deflate ", 100L)), method = 8L)
  ))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  extract <- get(".archive_extract", asNamespace("bigbang"))
  listing <- reader(archive, ".zip")
  expect_identical(as.character(listing$Name), c("pkg/", "pkg/stored.txt", "pkg/deflated.txt"))
  destination <- file.path(root, "extracted")
  dir.create(destination)
  expect_invisible(extract(archive, ".zip", destination))
  expect_identical(
    rawToChar(readBin(file.path(destination, "pkg", "stored.txt"), "raw", 100L)),
    "stored"
  )
  expect_identical(
    rawToChar(readBin(file.path(destination, "pkg", "deflated.txt"), "raw", 10000L)),
    strrep("deflate ", 100L)
  )

  bad <- file.path(root, "bad-crc.zip")
  t072c_zip_write(bad, list(
    list(name = "pkg/file", data = t072c_raw("payload"), bad_crc = TRUE)
  ))
  bad_result <- t072c_verdict(reader, bad, ".zip")
  expect_false(bad_result$ok)
  expect_identical(bad_result$class, "bigbang_error_archive_format")
  expect_match(bad_result$message, "CRC-32")
})

test_that("archive destinations must be empty and never clobber existing links", {
  root <- tempfile("round072c-destination-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  outside <- file.path(root, "outside.txt")
  writeBin(t072c_raw("keep"), outside)
  archive <- file.path(root, "archive.zip")
  t072c_zip_write(archive, list(list(name = "pkg", data = t072c_raw("new"))))
  destination <- file.path(root, "destination")
  dir.create(destination)
  link <- suppressWarnings(file.symlink(outside, file.path(destination, "pkg")))
  if (!isTRUE(link)) writeBin(t072c_raw("existing"), file.path(destination, "pkg"))
  result <- t072c_verdict(
    get(".archive_extract", asNamespace("bigbang")), archive, ".zip", destination
  )
  expect_false(result$ok)
  expect_identical(rawToChar(readBin(outside, "raw", 100L)), "keep")
  if (isTRUE(link)) expect_true(nzchar(Sys.readlink(file.path(destination, "pkg"))))
})

test_that("archive limits apply in inspection and extraction", {
  root <- tempfile("round072c-limits-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  extract <- get(".archive_extract", asNamespace("bigbang"))
  two <- file.path(root, "two.tar")
  t072c_write_tar(two, list(list(name = "a"), list(name = "b")))
  withr::local_options(bigbang.archive.max_entries = 1)
  expect_identical(t072c_verdict(reader, two, ".tar")$class, "bigbang_error_archive_limit")
  destination <- file.path(root, "two-extracted")
  dir.create(destination)
  expect_identical(
    t072c_verdict(extract, two, ".tar", destination)$class,
    "bigbang_error_archive_limit"
  )

  bytes <- file.path(root, "bytes.tar")
  t072c_write_tar(bytes, list(list(name = "payload", data = t072c_raw("12"))))
  withr::local_options(bigbang.archive.max_entries = 100000)
  withr::local_options(bigbang.archive.max_total_bytes = 1)
  expect_identical(t072c_verdict(reader, bytes, ".tar")$class, "bigbang_error_archive_limit")
  destination <- file.path(root, "bytes-extracted")
  dir.create(destination)
  expect_identical(
    t072c_verdict(extract, bytes, ".tar", destination)$class,
    "bigbang_error_archive_limit"
  )

  metadata <- file.path(root, "metadata.tar")
  pax <- t072c_raw(paste0("21 path=pkg/file.txt\n"))
  t072c_write_tar(metadata, list(
    list(name = "PaxHeaders.0", type = "x", data = pax),
    list(name = "pkg/file.txt", data = t072c_raw("x"))
  ))
  withr::local_options(bigbang.archive.max_total_bytes = 100000)
  withr::local_options(bigbang.archive.max_metadata_bytes = 8)
  expect_identical(t072c_verdict(reader, metadata, ".tar")$class, "bigbang_error_archive_limit")

  path <- file.path(root, "long-path.tar")
  t072c_write_tar(path, list(list(name = "long-name")))
  withr::local_options(bigbang.archive.max_metadata_bytes = 1024^2)
  withr::local_options(bigbang.archive.max_path_bytes = 4)
  expect_identical(t072c_verdict(reader, path, ".tar")$class, "bigbang_error_archive_format")
})

test_that("portable names accept UTF-8 and reject controls, case collisions, and Windows names", {
  root <- tempfile("round072c-names-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  tar_case <- file.path(root, "case.tar")
  t072c_write_tar(tar_case, list(
    list(name = "pkg/Name.txt"), list(name = "pkg/name.txt")
  ))
  expect_false(t072c_verdict(reader, tar_case, ".tar")$ok)
  tar_control <- file.path(root, "control.tar")
  t072c_write_tar(tar_control, list(list(name = "pkg/bad\001.txt")))
  expect_false(t072c_verdict(reader, tar_control, ".tar")$ok)
  tar_reserved <- file.path(root, "reserved.tar")
  t072c_write_tar(tar_reserved, list(list(name = "pkg/CON.txt")))
  expect_false(t072c_verdict(reader, tar_reserved, ".tar")$ok)
  tar_backslash <- file.path(root, "backslash.tar")
  t072c_write_tar(tar_backslash, list(list(name = "pkg\\bad.txt")))
  expect_false(t072c_verdict(reader, tar_backslash, ".tar")$ok)
  tar_trailing <- file.path(root, "trailing.tar")
  t072c_write_tar(tar_trailing, list(list(name = "pkg/bad. ")))
  expect_false(t072c_verdict(reader, tar_trailing, ".tar")$ok)
  utf8 <- file.path(root, "utf8.tar")
  t072c_write_tar(utf8, list(list(name = "pkg/población.csv")))
  expect_true(t072c_verdict(reader, utf8, ".tar")$ok)
  utf8_zip <- file.path(root, "utf8.zip")
  t072c_zip_write(utf8_zip, list(list(
    name = "pkg/naïve.csv", data = t072c_raw("ok")
  )))
  expect_true(t072c_verdict(reader, utf8_zip, ".zip")$ok)
  zip_bad_names <- file.path(root, "zip-bad-names.zip")
  t072c_zip_write(zip_bad_names, list(
    list(name = "pkg\\bad.txt"), list(name = "pkg/bad. ")
  ))
  expect_false(t072c_verdict(reader, zip_bad_names, ".zip")$ok)
  zip_reserved <- file.path(root, "zip-reserved.zip")
  t072c_zip_write(zip_reserved, list(list(name = "pkg/AUX.txt")))
  expect_false(t072c_verdict(reader, zip_reserved, ".zip")$ok)
  zip_nul <- file.path(root, "zip-nul.zip")
  t072c_zip_write(zip_nul, list(list(
    name_raw = c(t072c_raw("pkg/bad"), as.raw(0L), t072c_raw(".txt"))
  )))
  expect_false(t072c_verdict(reader, zip_nul, ".zip")$ok)
  invalid <- file.path(root, "invalid.zip")
  t072c_zip_write(invalid, list(list(
    name_raw = c(t072c_raw("pkg/bad"), as.raw(255L), t072c_raw(".txt"))
  )))
  expect_false(t072c_verdict(reader, invalid, ".zip")$ok)
})

test_that("the scanner reports damaged archives and accepts EOCD comments containing signatures", {
  root <- tempfile("round072c-scanner-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "comment.zip")
  t072c_zip_write(archive, list(
    list(name = "demo/", mode = 16877L),
    list(name = "demo/DESCRIPTION", data = t072c_raw(paste(
      "Package: demo", "Version: 1.0.0", "Title: Demo", "Description: Demo.",
      "License: MIT", sep = "\n"
    ))),
    list(name = "demo/R/", mode = 16877L),
    list(name = "demo/R/code.R", data = t072c_raw("value <- 1\n"))
  ), comment = t072c_raw("comment PK\005\006 inside"))
  result <- scan_bigbang_artifact(archive)
  expect_s3_class(result, "bigbang_artifact_scan")
  expect_null(result$archive_error)

  damaged <- file.path(root, "damaged.zip")
  bytes <- readBin(archive, "raw", file.size(archive) - 3L)
  writeBin(bytes, damaged)
  damaged_result <- scan_bigbang_artifact(damaged)
  expect_s3_class(damaged_result, "bigbang_artifact_scan")
  expect_true(is.list(damaged_result$archive_error))
  expect_match(damaged_result$archive_error$message, "Could not extract archive")
})

test_that("the generated installer reader has the same verdict and tree", {
  root <- tempfile("round072c-parity-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "parity.zip")
  t072c_zip_write(archive, list(
    list(name = "demo/", mode = 16877L),
    list(name = "demo/file.txt", data = t072c_raw("same"), method = 8L)
  ))
  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(
      c(list(message = message, call = NULL), list(...)),
      class = c(class, "bigbang_error", "error", "condition")
    ))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), generated)
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)
  expect_identical(
    t072c_verdict(package_reader, archive, ".zip")$ok,
    t072c_verdict(generated_reader, archive, ".zip")$ok
  )
  package_dir <- file.path(root, "package")
  generated_dir <- file.path(root, "generated")
  dir.create(package_dir)
  dir.create(generated_dir)
  get(".archive_extract", asNamespace("bigbang"))(archive, ".zip", package_dir)
  get(".archive_extract", generated)(archive, ".zip", generated_dir)
  expect_identical(
    sort(list.files(package_dir, recursive = TRUE, include.dirs = TRUE)),
    sort(list.files(generated_dir, recursive = TRUE, include.dirs = TRUE))
  )
  expect_identical(
    readBin(file.path(package_dir, "demo", "file.txt"), "raw", 100L),
    readBin(file.path(generated_dir, "demo", "file.txt"), "raw", 100L)
  )
})

test_that("generated installer parity covers the complete hardening corpus", {
  root <- tempfile("round072c-parity-corpus-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(
      c(list(message = message, call = NULL), list(...)),
      class = c(class, "bigbang_error", "error", "condition")
    ))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), generated)
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  package_extract <- get(".archive_extract", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)
  generated_extract <- get(".archive_extract", generated)

  healthy <- file.path(root, "healthy.zip")
  t072c_zip_write(healthy, list(
    list(name = "demo/", mode = 16877L),
    list(name = "demo/file.txt", data = t072c_raw("same"), method = 8L)
  ))
  parent_file <- file.path(root, "parent-file.tar")
  t072c_write_tar(parent_file, list(
    list(name = "pkg/a/b", data = t072c_raw("child")),
    list(name = "pkg/a", data = t072c_raw("parent"))
  ))
  directory_data <- file.path(root, "directory-data.zip")
  t072c_zip_write(directory_data, list(
    list(name = "pkg/", mode = 16877L, data = t072c_raw("data"))
  ))
  dos_without_slash <- file.path(root, "dos-without-slash.zip")
  t072c_zip_write(dos_without_slash, list(
    list(name = "pkg/dir", dos_directory = TRUE)
  ))
  bad_crc <- file.path(root, "bad-crc.zip")
  t072c_zip_write(bad_crc, list(
    list(name = "pkg/file", data = t072c_raw("bad"), bad_crc = TRUE)
  ))
  case_collision <- file.path(root, "case-collision.tar")
  t072c_write_tar(case_collision, list(
    list(name = "pkg/Name.txt"), list(name = "pkg/name.txt")
  ))
  control <- file.path(root, "control.tar")
  t072c_write_tar(control, list(list(name = "pkg/bad\001.txt")))
  reserved <- file.path(root, "reserved.tar")
  t072c_write_tar(reserved, list(list(name = "pkg/CON.txt")))
  invalid_utf8 <- file.path(root, "invalid-utf8.zip")
  t072c_zip_write(invalid_utf8, list(list(
    name_raw = c(t072c_raw("pkg/bad"), as.raw(255L), t072c_raw(".txt"))
  )))
  damaged <- file.path(root, "damaged.zip")
  bytes <- readBin(healthy, "raw", file.size(healthy) - 3L)
  writeBin(bytes, damaged)
  corpus <- c(healthy, parent_file, directory_data, dos_without_slash,
              bad_crc, case_collision, control, reserved, invalid_utf8, damaged)

  tree <- function(path) {
    members <- list.files(path, recursive = TRUE, all.files = TRUE,
                          include.dirs = TRUE, no.. = TRUE)
    if (length(members) == 0L) return(data.frame())
    full <- file.path(path, members)
    info <- file.info(full)
    data.frame(
      name = members, type = info$isdir, size = info$size,
      stringsAsFactors = FALSE, row.names = NULL
    )
  }
  for (archive in corpus) {
    ext <- if (grepl("\\.zip$", archive)) ".zip" else ".tar"
    package_verdict <- t072c_verdict(package_reader, archive, ext)
    generated_verdict <- t072c_verdict(generated_reader, archive, ext)
    expect_identical(package_verdict$ok, generated_verdict$ok,
                     info = basename(archive))
    expect_identical(package_verdict$class, generated_verdict$class,
                     info = basename(archive))
    package_dir <- file.path(root, paste0("package-", basename(archive)))
    generated_dir <- file.path(root, paste0("generated-", basename(archive)))
    dir.create(package_dir)
    dir.create(generated_dir)
    package_extract_verdict <- t072c_verdict(
      package_extract, archive, ext, package_dir
    )
    generated_extract_verdict <- t072c_verdict(
      generated_extract, archive, ext, generated_dir
    )
    expect_identical(package_extract_verdict$ok, generated_extract_verdict$ok,
                     info = basename(archive))
    expect_identical(package_extract_verdict$class,
                     generated_extract_verdict$class,
                     info = basename(archive))
    if (package_extract_verdict$ok) {
      expect_identical(tree(package_dir), tree(generated_dir),
                       info = basename(archive))
    }
  }
})
