round072_raw <- function(text) charToRaw(enc2utf8(text))

round072_field <- function(text, width) {
  value <- round072_raw(text)
  if (length(value) > width) stop("fixture field is too long")
  c(value, raw(width - length(value)))
}

round072_octal <- function(value, width) {
  round072_field(sprintf(paste0("%0", width - 1L, "o"), value), width)
}

round072_tar_header <- function(name, size, type = "0", link = "", prefix = "",
                                mode = 420L) {
  header <- raw(512L)
  header[1:100] <- round072_field(name, 100L)
  header[101:108] <- round072_octal(mode, 8L)
  header[109:116] <- round072_octal(0L, 8L)
  header[117:124] <- round072_octal(0L, 8L)
  header[125:136] <- round072_octal(size, 12L)
  header[137:148] <- round072_octal(0L, 12L)
  header[149:156] <- round072_field("        ", 8L)
  header[157] <- round072_raw(type)
  header[158:257] <- round072_field(link, 100L)
  header[258:263] <- c(round072_raw("ustar"), as.raw(0L))
  header[264:265] <- round072_raw("00")
  if (nzchar(prefix)) header[346:500] <- round072_field(prefix, 155L)
  checksum <- sum(as.integer(header))
  header[149:156] <- c(round072_raw(sprintf("%06o", checksum)), as.raw(c(32L, 0L)))
  header
}

round072_rechecksum <- function(header) {
  header[149:156] <- round072_field("        ", 8L)
  checksum <- sum(as.integer(header))
  header[149:156] <- c(round072_raw(sprintf("%06o", checksum)), as.raw(c(32L, 0L)))
  header
}

round072_base256 <- function(value, width = 12L) {
  digits <- integer(width)
  remaining <- value
  for (index in width:1L) {
    digits[[index]] <- remaining %% 256
    remaining <- floor(remaining / 256)
  }
  if (remaining != 0) stop("base-256 fixture is too large")
  digits[[1L]] <- digits[[1L]] + 128L
  as.raw(digits)
}

round072_tar_write_raw <- function(path, blocks, compression = NULL) {
  connection <- if (is.null(compression)) file(path, open = "wb") else if (
    identical(compression, "gzip")
  ) gzfile(path, open = "wb") else if (identical(compression, "bzip2")) {
    bzfile(path, open = "wb")
  } else {
    xzfile(path, open = "wb")
  }
  on.exit(close(connection), add = TRUE)
  writeBin(do.call(c, blocks), connection)
  invisible(path)
}

round072_tar_write <- function(path, entries, compression = NULL) {
  connection <- if (is.null(compression)) {
    file(path, open = "wb")
  } else if (identical(compression, "gzip")) {
    gzfile(path, open = "wb")
  } else if (identical(compression, "bzip2")) {
    bzfile(path, open = "wb")
  } else {
    xzfile(path, open = "wb")
  }
  on.exit(close(connection), add = TRUE)
  for (entry in entries) {
    data <- if (is.null(entry$data)) raw() else entry$data
    writeBin(round072_tar_header(
      entry$name, length(data), entry$type %||% "0", entry$link %||% ""
    ), connection)
    if (length(data) > 0L) writeBin(data, connection)
    padding <- (512L - (length(data) %% 512L)) %% 512L
    if (padding > 0L) writeBin(raw(padding), connection)
  }
  writeBin(raw(1024L), connection)
  invisible(path)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

round072_pax <- function(key, value) {
  body <- paste0(key, "=", value, "\n")
  length_text <- nchar(body) + 3L
  repeat {
    candidate <- paste0(length_text, " ", body)
    if (nchar(candidate) == length_text) return(round072_raw(candidate))
    length_text <- nchar(candidate)
  }
}

round072_le16 <- function(value) as.raw(c(value %% 256, value %/% 256))

round072_le32 <- function(value) {
  as.raw(c(
    value %% 256, value %/% 256 %% 256, value %/% 65536 %% 256,
    value %/% 16777216 %% 256
  ))
}

round072_crc32 <- function(bytes) {
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

round072_zip_write <- function(path, entries, encrypted = FALSE, zip64 = FALSE) {
  local <- raw()
  central <- raw()
  offset <- 0L
  flags <- if (encrypted) 1L else 0L
  for (entry in entries) {
    name <- round072_raw(entry$name)
    data <- if (is.null(entry$data)) raw() else entry$data
    crc <- round072_crc32(data)
    mode <- entry$mode %||% 33188L
    external <- if (identical(entry$origin %||% "unix", "unix")) {
      as.raw(c(0L, 0L, mode %% 256L, mode %/% 256L %% 256L))
    } else {
      as.raw(c(16L, 0L, 0L, 0L))
    }
    local <- c(
      local, round072_raw("PK\003\004"), round072_le16(20L),
      round072_le16(flags), round072_le16(0L), round072_le16(0L), round072_le16(0L),
      round072_le32(crc), round072_le32(length(data)), round072_le32(length(data)),
      round072_le16(length(name)), round072_le16(0L), name, data
    )
    central <- c(
      central, round072_raw("PK\001\002"), round072_le16(0x0314),
      round072_le16(20L), round072_le16(flags), round072_le16(0L),
      round072_le16(0L), round072_le16(0L), round072_le32(crc),
      round072_le32(length(data)), round072_le32(length(data)),
      round072_le16(length(name)), round072_le16(0L), round072_le16(0L),
      round072_le16(0L), round072_le16(0L), external, round072_le32(offset), name
    )
    offset <- length(local)
  }
  eocd <- c(
    round072_raw("PK\005\006"), raw(4L), round072_le16(length(entries)),
    round072_le16(length(entries)), round072_le32(length(central)),
    round072_le32(length(local)), round072_le16(0L)
  )
  result <- c(local, central, eocd)
  if (zip64) {
    eocd_start <- length(local) + length(central) + 1L
    result[(eocd_start + 10L):(eocd_start + 11L)] <- as.raw(c(255L, 255L))
  }
  writeBin(result, path)
  invisible(path)
}

round072_reader_verdict <- function(reader, archive, ext) {
  value <- tryCatch(reader(archive, ext), error = identity)
  if (inherits(value, "error")) {
    list(
      ok = FALSE, class = class(value)[[1L]], entry = value$entry %||% NA_character_,
      message = conditionMessage(value)
    )
  } else {
    list(ok = TRUE, class = NA_character_, entry = NA_character_,
         message = NA_character_, members = if (identical(ext, ".zip")) {
           value$Name
         } else {
           value
         })
  }
}

test_that("the package and generated archive readers agree without extraction", {
  root <- tempfile("round072-corpus-")
  dir.create(root)
  tar_path <- file.path(root, "control.tar")
  round072_tar_write(tar_path, list(
    list(name = "pkg/DESCRIPTION", data = round072_raw("Package: pkg\nVersion: 1.0.0\n")),
    list(name = "pkg/", type = "5")
  ))
  zip_path <- file.path(root, "control.zip")
  round072_zip_write(zip_path, list(
    list(name = "pkg/", mode = 16877L),
    list(name = "pkg/DESCRIPTION", data = round072_raw("Package: pkg\nVersion: 1.0.0\n"))
  ))
  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(c(list(message = message, call = NULL), list(...)),
                   class = c(class, "bigbang_error", "error", "condition")))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), envir = generated)
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)
  for (fixture in list(list(tar_path, ".tar"), list(zip_path, ".zip"))) {
    expect_identical(
      round072_reader_verdict(package_reader, fixture[[1L]], fixture[[2L]]),
      round072_reader_verdict(generated_reader, fixture[[1L]], fixture[[2L]])
    )
  }
})

test_that("tar link and special-entry corpus is rejected before extraction", {
  root <- tempfile("round072-tar-")
  dir.create(root)
  sentinel <- file.path(root, "outside.txt")
  writeBin(charToRaw("do not overwrite"), sentinel)
  fixtures <- list(
    symlink_through = list(
      list(name = "pkg/DESCRIPTION", data = round072_raw("Package: pkg\nVersion: 1.0.0\n")),
      list(name = "pkg/link", type = "2", link = "../outside"),
      list(name = "pkg/link/x.txt", data = round072_raw("overwritten"))
    ),
    symlink_final = list(list(name = "pkg/link", type = "2", link = "../outside")),
    hardlink = list(list(name = "pkg/link", type = "1", link = "../outside")),
    pax_link = list(
      list(name = "PaxHeaders.0", type = "x", data = round072_pax("linkpath", "../outside")),
      list(name = "pkg/file", data = round072_raw("payload"))
    ),
    gnu_k = list(
      list(name = "././@LongLink", type = "K", data = round072_raw("../outside")),
      list(name = "pkg/file", data = round072_raw("payload"))
    ),
    long_directory = list(
      list(name = "././@LongLink", type = "L", data = round072_raw(strrep("d", 120L))),
      list(name = "x", type = "5")
    )
  )
  for (compression in c(NULL, "gzip", "bzip2", "xz")) {
    compression_name <- if (is.null(compression)) "tar" else compression
    for (fixture_name in names(fixtures)) {
      archive <- file.path(root, paste0(fixture_name, ".", compression_name))
      round072_tar_write(archive, fixtures[[fixture_name]], compression)
      verdict <- round072_reader_verdict(
        get(".archive_inspect", asNamespace("bigbang")), archive,
        if (is.null(compression)) ".tar" else switch(compression, gzip = ".tar.gz", bzip2 = ".tar.bz2", xz = ".tar.xz")
      )
      if (fixture_name == "long_directory") {
        expect_true(verdict$ok)
      } else {
        expect_false(verdict$ok)
        expect_identical(verdict$class, "bigbang_error_archive_link")
        expect_true(nzchar(verdict$entry))
      }
    }
  }
  expect_identical(readBin(sentinel, "raw", file.size(sentinel)), round072_raw("do not overwrite"))
})

test_that("ZIP links, ZIP64, encryption, truncation, and a healthy control agree", {
  root <- tempfile("round072-zip-")
  dir.create(root)
  healthy <- file.path(root, "healthy.zip")
  round072_zip_write(healthy, list(
    list(name = "pkg/", mode = 16877L),
    list(name = "pkg/DESCRIPTION", data = round072_raw("Package: pkg\nVersion: 1.0.0\n"))
  ))
  symlink <- file.path(root, "symlink.zip")
  round072_zip_write(symlink, list(list(name = "pkg/link", mode = 41471L, data = round072_raw("../outside"))))
  zip64 <- file.path(root, "zip64.zip")
  round072_zip_write(zip64, list(list(name = "pkg/file", data = round072_raw("x"))), zip64 = TRUE)
  encrypted <- file.path(root, "encrypted.zip")
  round072_zip_write(encrypted, list(list(name = "pkg/file", data = round072_raw("x"))), encrypted = TRUE)
  truncated <- file.path(root, "truncated.tar")
  round072_tar_write(truncated, list(list(name = "pkg/file", data = round072_raw("x"))))
  bytes <- readBin(truncated, "raw", file.size(truncated))
  writeBin(bytes[seq_len(length(bytes) - 512L)], truncated)

  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(c(list(message = message, call = NULL), list(...)),
                   class = c(class, "bigbang_error", "error", "condition")))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), envir = generated)
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)
  cases <- list(
    list(healthy, ".zip"), list(symlink, ".zip"), list(zip64, ".zip"),
    list(encrypted, ".zip"), list(truncated, ".tar")
  )
  for (case in cases) {
    expect_identical(
      round072_reader_verdict(package_reader, case[[1L]], case[[2L]]),
      round072_reader_verdict(generated_reader, case[[1L]], case[[2L]])
    )
  }
  expect_true(round072_reader_verdict(package_reader, healthy, ".zip")$ok)
  expect_false(round072_reader_verdict(package_reader, symlink, ".zip")$ok)
})

test_that("tar geometry traps are rejected before any path can escape", {
  root <- tempfile("round072-geometry-")
  dir.create(root)
  outside <- file.path(root, "outside")
  dir.create(outside)
  sentinel <- file.path(outside, "sentinel.txt")
  writeBin(round072_raw("keep"), sentinel)
  description <- round072_raw("Package: pkg\nVersion: 1.0.0\n")
  data_block <- function(data) c(data, raw(512L - length(data)))
  pax_block <- function(key, value) {
    payload <- round072_pax(key, value)
    list(
      round072_tar_header("PaxHeaders.0", length(payload), type = "x"),
      data_block(payload)
    )
  }
  end_blocks <- list(raw(1024L))
  fixtures <- list(
    pax_size_zero = c(
      pax_block("size", "0"),
      list(
        round072_tar_header("pkg/DESCRIPTION", 512L),
        round072_tar_header("pkg/link", 0L, type = "2", link = "../../outside"),
        round072_tar_header("pkg/link/x.txt", 5L), data_block(round072_raw("pwned"))
      ), end_blocks
    ),
    pax_size_greater_than_real = c(
      pax_block("size", "1536"),
      list(round072_tar_header("pkg/file", 512L), data_block(round072_raw("one"))),
      end_blocks
    ),
    directory_size = list(
      round072_tar_header("pkg", 512L, type = "5"),
      round072_tar_header("pkg/link", 0L, type = "2", link = "../../outside"),
      round072_tar_header("pkg/link/x.txt", 5L), data_block(round072_raw("pwned")),
      raw(1024L)
    ),
    prefix_absolute = list(
      round072_tar_header("pkg/DESCRIPTION", length(description)),
      data_block(description),
      round072_tar_header("evil.txt", 5L, prefix = outside),
      data_block(round072_raw("pwned")), raw(1024L)
    ),
    prefix_parent = list(
      round072_tar_header("evil.txt", 5L, prefix = "../.."),
      data_block(round072_raw("pwned")), raw(1024L)
    ),
    base_256 = list({
      header <- round072_tar_header("pkg/file", 1L)
      header[125:136] <- round072_base256(1L)
      round072_rechecksum(header)
    }, data_block(round072_raw("x")), raw(1024L)),
    bad_checksum = list({
      header <- round072_tar_header("pkg/file", 1L)
      header[1L] <- as.raw((as.integer(header[1L]) + 1L) %% 256L)
      header
    }, data_block(round072_raw("x")), raw(1024L)),
    unknown_geometry = c(
      pax_block("GNU.sparse.map", "0,1"),
      list(round072_tar_header("pkg/file", 1L), data_block(round072_raw("x"))),
      end_blocks
    ),
    crosses_file = list(
      round072_tar_header("pkg", 1L), data_block(round072_raw("x")),
      round072_tar_header("pkg/child", 1L), data_block(round072_raw("x")),
      raw(1024L)
    ),
    after_end = list(
      round072_tar_header("pkg/file", 1L), data_block(round072_raw("x")),
      raw(1024L), round072_tar_header("pkg/late", 1L), data_block(round072_raw("x"))
    )
  )

  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(c(list(message = message, call = NULL), list(...)),
                   class = c(class, "bigbang_error", "error", "condition")))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), envir = generated)
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)

  for (fixture_name in names(fixtures)) {
    for (compression in c(NULL, "gzip", "bzip2", "xz")) {
      suffix <- if (is.null(compression)) ".tar" else switch(
        compression, gzip = ".tar.gz", bzip2 = ".tar.bz2", xz = ".tar.xz"
      )
      archive <- file.path(root, paste0(fixture_name, suffix))
      round072_tar_write_raw(archive, fixtures[[fixture_name]], compression)
      package_verdict <- round072_reader_verdict(package_reader, archive, suffix)
      generated_verdict <- round072_reader_verdict(generated_reader, archive, suffix)
      expect_false(package_verdict$ok, info = paste(fixture_name, suffix))
      expect_identical(package_verdict$ok, generated_verdict$ok)
      extract_dir <- file.path(root, paste0("extract-", fixture_name, suffix))
      dir.create(extract_dir)
      expect_error(.archive_extract(archive, suffix, extract_dir))
      expect_identical(readBin(sentinel, "raw", file.size(sentinel)), round072_raw("keep"))
    }
  }
})

test_that("the generated reader has no unqualified non-base dependency and reads pax", {
  root <- tempfile("round072-pax-generated-")
  dir.create(root)
  archive <- file.path(root, "pax.tar")
  payload <- round072_pax("path", "pkg/long-name.txt")
  round072_tar_write_raw(archive, list(
    round072_tar_header("PaxHeaders.0", length(payload), type = "x"),
    c(payload, raw(512L - length(payload))),
    round072_tar_header("pkg/short.txt", 5L),
    c(round072_raw("hello"), raw(507L)), raw(1024L)
  ))
  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(c(list(message = message, call = NULL), list(...)),
                   class = c(class, "bigbang_error", "error", "condition")))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), envir = generated)
  functions <- ls(generated, all.names = TRUE)
  functions <- functions[startsWith(functions, ".archive_")]
  functions <- functions[vapply(functions, function(name) {
    is.function(get(name, generated))
  }, logical(1L))]
  unknown <- unique(unlist(lapply(functions, function(name) {
    contains_unqualified <- function(node) {
      if (!is.call(node)) return(FALSE)
      head <- node[[1L]]
      if (is.symbol(head) && as.character(head) %in% c("::", ":::")) {
        return(FALSE)
      }
      if (is.symbol(head) && identical(as.character(head), "modifyList")) {
        return(TRUE)
      }
      if (length(node) <= 1L) return(FALSE)
      any(vapply(as.list(node)[-1L], contains_unqualified, logical(1L)))
    }
    if (contains_unqualified(body(get(name, generated)))) "modifyList" else character()
  }), use.names = FALSE))
  expect_length(unknown, 0L)
  expect_true(round072_reader_verdict(
    get(".archive_inspect", generated), archive, ".tar"
  )$ok)
})

test_that("healthy R CMD build archives match untar in every compression", {
  testthat::skip_on_cran()
  root <- tempfile("round072-healthy-")
  dir.create(root)
  package_root <- file.path(root, "healthy-1.0.0")
  dir.create(file.path(package_root, "R"), recursive = TRUE)
  dir.create(file.path(package_root, "inst", "deep", "path"), recursive = TRUE)
  writeLines(c("Package: healthy", "Version: 1.0.0", "Description: Healthy archive."),
             file.path(package_root, "DESCRIPTION"))
  writeLines("export(hello)", file.path(package_root, "NAMESPACE"))
  writeLines("hello <- function() TRUE", file.path(package_root, "R", "hello.R"))
  configure <- file.path(package_root, "configure")
  writeLines("#!/bin/sh\nexit 0", configure)
  Sys.chmod(configure, "0755")
  long_file <- file.path(
    package_root, "inst", "deep", "path", paste0(strrep("a", 110L), ".txt")
  )
  writeBin(round072_raw("long"), long_file)

  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  system2(
    r_binary, c("CMD", "build", "--no-manual", shQuote(package_root)),
    stdout = TRUE, stderr = TRUE
  )
  built <- file.path(root, "healthy_1.0.0.tar.gz")
  expect_true(file.exists(built))
  compression_paths <- c(
    gzip = built,
    bzip2 = file.path(root, "healthy_1.0.0.tar.bz2"),
    xz = file.path(root, "healthy_1.0.0.tar.xz"),
    none = file.path(root, "healthy_1.0.0.tar")
  )
  for (compression in names(compression_paths)[-1L]) {
    suppressWarnings(utils::tar(
      compression_paths[[compression]], files = "healthy-1.0.0",
      compression = compression, tar = "internal"
    ))
  }

  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(c(list(message = message, call = NULL), list(...)),
                   class = c(class, "bigbang_error", "error", "condition")))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), envir = generated)
  generated_extract <- get(".archive_extract", generated)
  for (compression in names(compression_paths)) {
    archive <- compression_paths[[compression]]
    ext <- switch(compression, gzip = ".tar.gz", bzip2 = ".tar.bz2",
                  xz = ".tar.xz", none = ".tar")
    ours <- file.path(root, paste0("ours-", compression))
    reference <- file.path(root, paste0("reference-", compression))
    copied <- file.path(root, paste0("generated-", compression))
    dir.create(ours)
    dir.create(reference)
    dir.create(copied)
    .archive_extract(archive, ext, ours)
    utils::untar(archive, exdir = reference, tar = "internal")
    generated_extract(archive, ext, copied)
    for (path in c(reference, copied)) {
      expect_identical(
        sort(list.files(ours, recursive = TRUE, include.dirs = TRUE)),
        sort(list.files(path, recursive = TRUE, include.dirs = TRUE))
      )
    }
    files <- list.files(ours, recursive = TRUE, full.names = TRUE)
    files <- files[!dir.exists(files)]
    for (file in files) {
      relative <- substring(file, nchar(ours) + 2L)
      expect_identical(
        readBin(file, "raw", file.size(file)),
        readBin(file.path(reference, relative), "raw", file.size(file.path(reference, relative)))
      )
    }
    expect_identical(
      bitwAnd(file.info(file.path(ours, "healthy-1.0.0", "configure"))$mode, 73),
      bitwAnd(file.info(file.path(reference, "healthy-1.0.0", "configure"))$mode, 73)
    )
  }
})

test_that("the archive scanner reports an unsafe archive without extracting it", {
  root <- tempfile("round072-scanner-")
  dir.create(root)
  archive <- file.path(root, "unsafe.tar")
  round072_tar_write(archive, list(
    list(name = "pkg/link", type = "2", link = "../outside"),
    list(name = "pkg/link/x.txt", data = round072_raw("must not write"))
  ))
  extraction_calls <- 0L
  trace(
    ".archive_cache_extract", where = asNamespace("bigbang"),
    tracer = quote(extraction_calls <<- extraction_calls + 1L), print = FALSE
  )
  on.exit(untrace(".archive_cache_extract", where = asNamespace("bigbang")), add = TRUE)
  result <- scan_bigbang_artifact(archive)
  expect_true(result$vulnerable)
  expect_identical(result$type, "archive")
  expect_true("V8_archive_link" %in% result$signatures)
  expect_true(any(grepl("pkg/link", result$evidence$V8_archive_link, fixed = TRUE)))
  expect_identical(extraction_calls, 0L)
})

test_that("all archive entry points reject before untar or unzip", {
  root <- tempfile("round072-entry-points-")
  dir.create(root)
  archive <- file.path(root, "unsafe.tar")
  round072_tar_write(archive, list(
    list(name = "pkg/link", type = "2", link = "../outside"),
    list(name = "pkg/link/x.txt", data = round072_raw("must not write"))
  ))

  safe <- system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang")
  generated_root <- tempfile("round072-generated-")
  dir.create(generated_root)
  generated <- round075_create_metapackage(
    "round072verse", "toycomponent_0.1.0", pkg_dir = dirname(safe),
    dest_dir = generated_root, document = FALSE, verbose = FALSE,
    import_deps = character(),
    force_deps = character()
  )
  generated_archive <- file.path(
    generated$path, "inst", "archives", "toycomponent_0.1.0.tar.gz"
  )
  expect_true(file.copy(archive, generated_archive, overwrite = TRUE))

  untar_calls <- 0L
  unzip_calls <- 0L
  trace(
    "untar", where = asNamespace("utils"),
    tracer = quote(untar_calls <<- untar_calls + 1L), print = FALSE
  )
  trace(
    "unzip", where = asNamespace("utils"),
    tracer = quote(unzip_calls <<- unzip_calls + 1L), print = FALSE
  )
  on.exit(untrace("untar", where = asNamespace("utils")), add = TRUE)
  on.exit(untrace("unzip", where = asNamespace("utils")), add = TRUE)

  expect_error(
    round075_create_metapackage(
      "round072unsafe", archive, dest_dir = tempfile(), document = FALSE,
      verbose = FALSE, import_deps = character(), force_deps = character(),
      dry_run = TRUE
    ), class = "bigbang_error_archive_link"
  )
  expect_error(
    diagnose_dependencies(archive), class = "bigbang_error_archive_link"
  )
  install_result <- install_local_pkg(
    archive, lib = file.path(root, "local-lib"), verbose = FALSE
  )
  expect_length(install_result$failed, 1L)
  scan_result <- scan_bigbang_artifact(archive)
  expect_true(scan_result$vulnerable)

  generated_environment <- new.env(parent = globalenv())
  source(file.path(generated$path, "R", "utils.R"), generated_environment)
  source(file.path(generated$path, "R", "install_packages.R"), generated_environment)
  generated_result <- generated_environment$install_local_archive(
    "toycomponent_0.1.0", dirname(generated_archive),
    lib = file.path(root, "generated-lib"), cran_deps = "skip", verbose = FALSE
  )
  expect_false(generated_result$success)
  expect_true(grepl("symbolic links", generated_result$message, fixed = TRUE))
  expect_identical(untar_calls, 0L)
  expect_identical(unzip_calls, 0L)
})
