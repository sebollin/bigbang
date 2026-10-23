t072d_raw <- function(text) charToRaw(enc2utf8(text))

t072d_le16 <- function(value) as.raw(c(value %% 256, value %/% 256))
t072d_le32 <- function(value) {
  as.raw(c(
    value %% 256, value %/% 256 %% 256, value %/% 65536 %% 256,
    value %/% 16777216 %% 256
  ))
}

t072d_deflate <- function(data) {
  compressed <- memCompress(data, type = "gzip")
  compressed[3:(length(compressed) - 4L)]
}

t072d_zip_write <- function(path, entries) {
  local <- raw()
  central <- raw()
  offset <- 0
  for (entry in entries) {
    name <- if (is.null(entry$name_raw)) t072d_raw(entry$name) else entry$name_raw
    data <- if (is.null(entry$data)) raw() else entry$data
    method <- if (is.null(entry$method)) 0L else entry$method
    compressed <- if (!is.null(entry$compressed)) {
      entry$compressed
    } else if (method == 8L) {
      t072d_deflate(data)
    } else {
      data
    }
    crc <- if (is.null(entry$crc)) {
      bigbang:::.archive_crc32_unsigned(bigbang:::.archive_crc32(data))
    } else {
      entry$crc
    }
    local_name <- if (is.null(entry$local_name)) name else t072d_raw(entry$local_name)
    local_extra <- if (is.null(entry$local_extra)) raw() else entry$local_extra
    central_extra <- if (is.null(entry$central_extra)) raw() else entry$central_extra
    central_comment <- if (is.null(entry$central_comment)) {
      raw()
    } else {
      entry$central_comment
    }
    local_usize <- if (is.null(entry$local_usize)) length(data) else entry$local_usize
    local_csize <- if (is.null(entry$local_csize)) {
      length(compressed)
    } else {
      entry$local_csize
    }
    local <- c(
      local, t072d_raw("PK\003\004"), t072d_le16(20L), t072d_le16(0L),
      t072d_le16(method), raw(4L), t072d_le32(crc), t072d_le32(local_csize),
      t072d_le32(local_usize), t072d_le16(length(local_name)),
      t072d_le16(length(local_extra)), local_name, local_extra, compressed
    )

    mode <- if (is.null(entry$mode)) 33188L else entry$mode
    directory <- endsWith(enc2utf8(rawToChar(name)), "/")
    external <- as.raw(c(
      if (directory) c(16L, 0L) else c(0L, 0L),
      mode %% 256L, mode %/% 256L %% 256L
    ))
    central_usize <- if (is.null(entry$central_usize)) {
      length(data)
    } else {
      entry$central_usize
    }
    central_csize <- if (is.null(entry$central_csize)) {
      length(compressed)
    } else {
      entry$central_csize
    }
    central <- c(
      central, t072d_raw("PK\001\002"), t072d_le16(0x0314L), t072d_le16(20L),
      t072d_le16(0L), t072d_le16(method), raw(4L), t072d_le32(crc),
      t072d_le32(central_csize), t072d_le32(central_usize),
      t072d_le16(length(name)), t072d_le16(length(central_extra)),
      t072d_le16(length(central_comment)),
      raw(2L), raw(2L), external, t072d_le32(offset), name, central_extra,
      central_comment
    )
    offset <- length(local)
  }
  eocd <- c(
    t072d_raw("PK\005\006"), raw(4L), t072d_le16(length(entries)),
    t072d_le16(length(entries)), t072d_le32(length(central)),
    t072d_le32(length(local)), raw(2L)
  )
  writeBin(c(local, central, eocd), path)
  invisible(path)
}

t072d_field <- function(value, width) {
  value <- if (is.raw(value)) value else t072d_raw(value)
  c(value, raw(width - length(value)))
}

t072d_tar_header <- function(name, size, type = "0", mode = 420L) {
  header <- raw(512L)
  header[1:100] <- t072d_field(name, 100L)
  header[101:108] <- t072d_field(sprintf("%07o", mode), 8L)
  header[109:116] <- t072d_field("0000000", 8L)
  header[117:124] <- t072d_field("0000000", 8L)
  header[125:136] <- t072d_field(sprintf("%011o", size), 12L)
  header[137:148] <- t072d_field("00000000000", 12L)
  header[149:156] <- t072d_field("        ", 8L)
  header[157] <- t072d_raw(type)
  header[258:263] <- t072d_field("ustar", 6L)
  header[264:265] <- t072d_raw("00")
  header[149:156] <- c(
    t072d_raw(sprintf("%06o", sum(as.integer(header)))), as.raw(c(32L, 0L))
  )
  header
}

t072d_tar_write <- function(path, entries) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  for (entry in entries) {
    data <- if (is.null(entry$data)) raw() else entry$data
    writeBin(
      t072d_tar_header(
        entry$name, length(data),
        if (is.null(entry$type)) "0" else entry$type,
        if (is.null(entry$mode)) 420L else entry$mode
      ),
      connection
    )
    if (length(data) > 0L) writeBin(data, connection)
    padding <- (512L - length(data) %% 512L) %% 512L
    if (padding > 0L) writeBin(raw(padding), connection)
  }
  writeBin(raw(1024L), connection)
  invisible(path)
}

t072d_empty <- function(path) {
  length(list.files(
    path, all.files = TRUE, no.. = TRUE, include.dirs = TRUE,
    recursive = TRUE
  )) == 0L
}

t072d_generated <- function() {
  generated <- new.env(parent = baseenv())
  generated$.bigbang_abort <- function(class, message, ...) {
    stop(structure(
      c(list(message = message, call = NULL), list(...)),
      class = c(class, "bigbang_error", "error", "condition")
    ))
  }
  generated$.meta_trf <- function(format, ...) sprintf(format, ...)
  eval(parse(text = round075_archive_template()), generated)
  generated
}

t072d_bounded_h1 <- function(directory) {
  package_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  if (!file.exists(file.path(package_root, "DESCRIPTION"))) {
    package_root <- normalizePath(
      file.path(package_root, "00_pkg_src", "bigbang"), mustWork = TRUE
    )
  }
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "pkgload::load_all(args[[1L]], quiet = TRUE)",
    "archives <- sort(list.files(args[[2L]], full.names = TRUE))",
    "for (archive in archives) {",
    "  result <- tryCatch(bigbang:::.archive_inspect(archive, '.zip'), error = identity)",
    "  if (!inherits(result, 'error') && !is.data.frame(result)) quit(status = 20L)",
    "  cat(basename(archive), class(result)[[1L]], '\\n')",
    "}",
    "quit(status = 0L)"
  ), script)
  command <- paste(
    "ulimit -v 2000000; timeout 5s",
    shQuote(file.path(R.home("bin"), "Rscript")),
    "--vanilla", shQuote(script), shQuote(package_root), shQuote(directory)
  )
  output <- system2("sh", c("-c", shQuote(command)), stdout = TRUE, stderr = TRUE)
  list(output = output, status = attr(output, "status", exact = TRUE) %||% 0L)
}

`%||%` <- function(value, fallback) if (is.null(value)) fallback else value

test_that("ZIP deflate output is bounded for every false ISIZE", {
  skip_on_cran()
  skip_if(
    !identical(unname(Sys.info()[["sysname"]]), "Linux"),
    "this resource guard requires Linux sh, ulimit -v, and timeout"
  )
  skip_if(Sys.which("timeout") == "", "the timeout utility is unavailable")
  root <- tempfile("round072d-deflate-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  data <- t072d_raw(strrep("A", 1000L))
  for (declared in c(0L, 1L, 999L, 1000L, 1001L)) {
    t072d_zip_write(
      file.path(root, paste0("usize-", declared, ".zip")),
      list(list(
        name = "pkg/a.txt", data = data, method = 8L,
        local_usize = declared, central_usize = declared
      ))
    )
  }
  bounded <- t072d_bounded_h1(root)
  expect_identical(as.integer(bounded$status), 0L)
  expect_length(grep("usize-1000.zip", bounded$output), 1L)
  expect_length(grep("memory exhausted|TIMEOUT|segmentation", bounded$output,
                     ignore.case = TRUE), 0L)
})

test_that("truncated, trailing, and over-limit deflate streams fail quickly", {
  root <- tempfile("round072d-stream-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  data <- t072d_raw(strrep("A", 1000L))
  full <- t072d_deflate(data)
  t072d_zip_write(file.path(root, "truncated.zip"), list(list(
    name = "pkg/a", data = data, method = 8L, compressed = full[seq_len(5L)]
  )))
  t072d_zip_write(file.path(root, "trailing.zip"), list(list(
    name = "pkg/a", data = data, method = 8L,
    compressed = c(full, raw(20L))
  )))
  t072d_zip_write(file.path(root, "honest-bomb.zip"), list(list(
    name = "pkg/a", data = data, method = 8L, central_usize = 100000L,
    local_usize = 100000L
  )))
  withr::local_options(bigbang.archive.max_total_bytes = 10000)
  for (archive in list.files(root, full.names = TRUE)) {
    expect_error(bigbang:::.archive_inspect(archive, ".zip"))
  }
})

test_that("ZIP metadata and implicit directories consume the configured limits", {
  root <- tempfile("round072d-limits-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  central <- file.path(root, "central-extra.zip")
  t072d_zip_write(central, list(list(
    name = "pkg/file", data = t072d_raw("x"),
    central_extra = c(t072d_le16(0x5455L), t072d_le16(200L), raw(200L))
  )))
  withr::local_options(bigbang.archive.max_metadata_bytes = 100)
  expect_error(bigbang:::.archive_inspect(central, ".zip"),
               class = "bigbang_error_archive_limit")

  comment <- file.path(root, "central-comment.zip")
  t072d_zip_write(comment, list(list(
    name = "pkg/file", data = raw(), central_comment = raw(200L)
  )))
  expect_error(bigbang:::.archive_inspect(comment, ".zip"),
               class = "bigbang_error_archive_limit")

  local <- file.path(root, "local-extra.zip")
  t072d_zip_write(local, list(list(
    name = "pkg/file", data = t072d_raw("x"), local_extra = raw(200L)
  )))
  withr::local_options(bigbang.archive.max_metadata_bytes = 1000)
  withr::local_options(bigbang.archive.max_total_bytes = 100)
  expect_error(bigbang:::.archive_inspect(local, ".zip"),
               class = "bigbang_error_archive_limit")

  implicit <- file.path(root, "implicit.zip")
  t072d_zip_write(implicit, list(
    list(name = "a/b/c/file1", data = raw()),
    list(name = "d/e/f/file2", data = raw())
  ))
  withr::local_options(bigbang.archive.max_entries = 2)
  expect_error(bigbang:::.archive_inspect(implicit, ".zip"),
               class = "bigbang_error_archive_limit")
  destination <- file.path(root, "implicit-out")
  dir.create(destination)
  expect_error(bigbang:::.archive_extract(implicit, ".zip", destination),
               class = "bigbang_error_archive_limit")
  expect_true(t072d_empty(destination))
})

test_that("duplicate and mid-extraction failures leave destinations empty", {
  root <- tempfile("round072d-atomic-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  duplicate <- file.path(root, "duplicate.zip")
  t072d_zip_write(duplicate, list(
    list(name = "pkg/file", data = t072d_raw("first")),
    list(name = "pkg/file", data = t072d_raw("second"))
  ))
  duplicate_destination <- file.path(root, "duplicate-out")
  dir.create(duplicate_destination)
  expect_error(bigbang:::.archive_extract(
    duplicate, ".zip", duplicate_destination
  ))
  expect_true(t072d_empty(duplicate_destination))

  bad_crc <- file.path(root, "bad-crc.zip")
  t072d_zip_write(bad_crc, list(
    list(name = "pkg/first", data = raw(50L)),
    list(name = "pkg/second", data = raw(50L), crc = 1L)
  ))
  crc_destination <- file.path(root, "crc-out")
  dir.create(crc_destination)
  expect_error(bigbang:::.archive_extract(bad_crc, ".zip", crc_destination))
  expect_true(t072d_empty(crc_destination))

  limited <- file.path(root, "limited.zip")
  t072d_zip_write(limited, list(
    list(name = "pkg/first", data = raw(50L)),
    list(name = "pkg/second", data = raw(50L))
  ))
  limited_destination <- file.path(root, "limited-out")
  dir.create(limited_destination)
  withr::local_options(bigbang.archive.max_total_bytes = 230)
  expect_error(bigbang:::.archive_extract(limited, ".zip", limited_destination))
  expect_true(t072d_empty(limited_destination))
})

test_that("directory permissions remain usable and cleanup repairs them", {
  root <- tempfile("round072d-modes-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  tar <- file.path(root, "mode.tar")
  t072d_tar_write(tar, list(
    list(name = "pkg/", type = "5", mode = 0L),
    list(name = "pkg/file", data = t072d_raw("data"))
  ))
  tar_destination <- file.path(root, "tar-out")
  dir.create(tar_destination)
  bigbang:::.archive_extract(tar, ".tar", tar_destination)
  expect_equal(
    bitwAnd(as.integer(file.info(file.path(tar_destination, "pkg"))$mode), 448L),
    448L
  )
  expect_identical(
    rawToChar(readBin(file.path(tar_destination, "pkg/file"), "raw", 10L)),
    "data"
  )

  zip <- file.path(root, "mode.zip")
  t072d_zip_write(zip, list(
    list(name = "pkg/", mode = 16804L),
    list(name = "pkg/file", data = t072d_raw("data"))
  ))
  zip_destination <- file.path(root, "zip-out")
  dir.create(zip_destination)
  bigbang:::.archive_extract(zip, ".zip", zip_destination)
  expect_equal(
    bitwAnd(as.integer(file.info(file.path(zip_destination, "pkg"))$mode), 448L),
    448L
  )
  expect_identical(
    rawToChar(readBin(file.path(zip_destination, "pkg/file"), "raw", 10L)),
    "data"
  )
})

test_that("Windows-invalid characters and C1 controls are rejected", {
  limits <- bigbang:::.archive_limit_options()
  invalid <- c("a<b", "a>b", "a:b", "a\"b", "a|b", "a?b", "a*b")
  invalid <- c(invalid, paste0("a", intToUtf8(0x80L), "b"),
               paste0("a", intToUtf8(0x9fL), "b"))
  for (name in invalid) {
    expect_error(bigbang:::.archive_validate_name_text(name, limits))
  }
})

test_that("the generated installer receives the new ZIP guards", {
  root <- tempfile("round072d-parity-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  duplicate <- file.path(root, "duplicate.zip")
  t072d_zip_write(duplicate, list(
    list(name = "pkg/file", data = t072d_raw("one")),
    list(name = "pkg/file", data = t072d_raw("two"))
  ))
  invalid <- file.path(root, "invalid.zip")
  t072d_zip_write(invalid, list(list(name = "pkg/a?b", data = raw())))
  central <- file.path(root, "central-extra.zip")
  t072d_zip_write(central, list(list(
    name = "pkg/file", data = raw(),
    central_extra = c(t072d_le16(0x5455L), t072d_le16(200L), raw(200L))
  )))
  false_isize <- file.path(root, "false-isize.zip")
  false_data <- t072d_raw(strrep("A", 1000L))
  t072d_zip_write(false_isize, list(list(
    name = "pkg/false-isize", data = false_data, method = 8L,
    local_usize = 1L, central_usize = 1L
  )))
  generated <- t072d_generated()
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)
  for (archive in c(duplicate, invalid, central, false_isize)) {
    package_result <- tryCatch(package_reader(archive, ".zip"), error = identity)
    generated_result <- tryCatch(generated_reader(archive, ".zip"), error = identity)
    expect_identical(class(package_result), class(generated_result))
    if (inherits(package_result, "error")) {
      expect_identical(conditionMessage(package_result), conditionMessage(generated_result))
    }
  }
})
