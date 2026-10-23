t072e_raw <- function(value) {
  if (is.raw(value)) value else charToRaw(enc2utf8(value))
}

t072e_le16 <- function(value) {
  as.raw(c(value %% 256, value %/% 256))
}

t072e_le32 <- function(value) {
  as.raw(c(
    value %% 256, value %/% 256 %% 256,
    value %/% 65536 %% 256, value %/% 16777216 %% 256
  ))
}

t072e_crc <- function(bytes) {
  bigbang:::.archive_crc32_unsigned(bigbang:::.archive_crc32(bytes))
}

t072e_deflate <- function(bytes) {
  compressed <- memCompress(bytes, type = "gzip")
  compressed[3L:(length(compressed) - 4L)]
}

t072e_local <- function(name, compressed, method, flags, crc, usize,
                        local_crc = crc, local_csize = length(compressed),
                        local_usize = usize, descriptor = raw()) {
  c(
    t072e_raw("PK\003\004"), t072e_le16(20L), t072e_le16(flags),
    t072e_le16(method), raw(4L), t072e_le32(local_crc),
    t072e_le32(local_csize), t072e_le32(local_usize),
    t072e_le16(length(name)), raw(2L), name, compressed, descriptor
  )
}

t072e_central <- function(name, compressed, method, flags, crc, usize, offset) {
  c(
    t072e_raw("PK\001\002"), t072e_le16(0x0314), t072e_le16(20L),
    t072e_le16(flags), t072e_le16(method), raw(4L), t072e_le32(crc),
    t072e_le32(length(compressed)), t072e_le32(usize),
    t072e_le16(length(name)), raw(8L), raw(4L), t072e_le32(offset), name
  )
}

t072e_zip_parts <- function(entries, comment = raw()) {
  locals <- raw()
  centrals <- list()
  offsets <- numeric(length(entries))
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    name <- t072e_raw(entry$name)
    data <- if (is.null(entry$data)) raw() else entry$data
    method <- if (is.null(entry$method)) 0L else entry$method
    compressed <- if (!is.null(entry$compressed)) entry$compressed else if (
      method == 8L
    ) {
      t072e_deflate(data)
    } else {
      data
    }
    flags <- if (is.null(entry$flags)) 0L else entry$flags
    crc <- if (is.null(entry$crc)) t072e_crc(data) else entry$crc
    usize <- if (is.null(entry$usize)) length(data) else entry$usize
    descriptor <- if (is.null(entry$descriptor)) raw() else entry$descriptor
    offsets[[index]] <- length(locals)
    locals <- c(locals, t072e_local(
      name, compressed, method, flags, crc, usize,
      local_crc = if (is.null(entry$local_crc)) crc else entry$local_crc,
      local_csize = if (is.null(entry$local_csize)) {
        length(compressed)
      } else {
        entry$local_csize
      },
      local_usize = if (is.null(entry$local_usize)) usize else entry$local_usize,
      descriptor = descriptor
    ))
    centrals[[index]] <- t072e_central(
      name, compressed, method, flags, crc, usize, offsets[[index]]
    )
  }
  central <- if (length(centrals) == 0L) raw() else do.call(c, centrals)
  eocd <- c(
    t072e_raw("PK\005\006"), raw(4L),
    t072e_le16(length(entries)), t072e_le16(length(entries)),
    t072e_le32(length(central)), t072e_le32(length(locals)),
    t072e_le16(length(comment)), comment
  )
  list(locals = locals, central = central, eocd = eocd)
}

t072e_write_parts <- function(path, parts, prefix = raw(), tail = raw()) {
  writeBin(c(prefix, parts$locals, parts$central, parts$eocd, tail), path)
  path
}

t072e_tar_field <- function(value, width) {
  bytes <- t072e_raw(value)
  if (length(bytes) > width) stop("tar fixture field is too long")
  c(bytes, raw(width - length(bytes)))
}

t072e_tar_header <- function(name, size, type = "0") {
  header <- raw(512L)
  header[1:100] <- t072e_tar_field(name, 100L)
  header[101:108] <- t072e_tar_field("0000644", 8L)
  header[109:116] <- t072e_tar_field("0000000", 8L)
  header[117:124] <- t072e_tar_field("0000000", 8L)
  header[125:136] <- t072e_tar_field(sprintf("%011o", size), 12L)
  header[137:148] <- t072e_tar_field("00000000000", 12L)
  header[149:156] <- t072e_tar_field("        ", 8L)
  header[157] <- t072e_raw(type)
  header[258:263] <- t072e_tar_field("ustar", 6L)
  header[264:265] <- t072e_raw("00")
  header[149:156] <- c(
    t072e_raw(sprintf("%06o", sum(as.integer(header)))), as.raw(c(32L, 0L))
  )
  header
}

t072e_empty_pax_tar <- function(path, count, type = "x") {
  headers <- lapply(seq_len(count), function(index) {
    t072e_tar_header(paste0("PaxHeaders.", index), 0L, type = type)
  })
  file_header <- t072e_tar_header("pkg/file", 1L)
  writeBin(c(do.call(c, headers), file_header, t072e_raw("x"), raw(511L),
             raw(1024L)), path)
  path
}

t072e_verdict <- function(fun, ...) {
  value <- tryCatch(fun(...), error = identity)
  if (inherits(value, "error")) {
    list(ok = FALSE, class = class(value)[[1L]], message = conditionMessage(value))
  } else {
    list(ok = TRUE, class = NA_character_, message = NA_character_, value = value)
  }
}

t072e_generated_reader <- function() {
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

test_that("ZIP records cover the file exactly and reject structural duplicates", {
  root <- tempfile("round072e-geometry-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))

  normal <- file.path(root, "normal.zip")
  t072e_write_parts(normal, t072e_zip_parts(list(
    list(name = "pkg/file", data = t072e_raw("payload"))
  )))
  expect_true(t072e_verdict(reader, normal, ".zip")$ok)

  prefix <- file.path(root, "self-extracting.zip")
  t072e_write_parts(prefix, t072e_zip_parts(list(
    list(name = "pkg/file", data = t072e_raw("payload"))
  )), prefix = t072e_raw("SFX"))
  expect_false(t072e_verdict(reader, prefix, ".zip")$ok)

  trailing <- file.path(root, "trailing.zip")
  t072e_write_parts(trailing, t072e_zip_parts(list(
    list(name = "pkg/file", data = t072e_raw("payload"))
  )), tail = t072e_raw("trailing"))
  expect_false(t072e_verdict(reader, trailing, ".zip")$ok)

  name_b <- t072e_raw("pkg/b")
  data_b <- t072e_raw("nested")
  local_b <- t072e_local(name_b, data_b, 0L, 0L, t072e_crc(data_b), length(data_b))
  name_a <- t072e_raw("pkg/a")
  data_a <- c(t072e_raw("hole"), local_b)
  local_a <- t072e_local(name_a, data_a, 0L, 0L, t072e_crc(data_a), length(data_a))
  overlap <- file.path(root, "overlap.zip")
  central <- c(
    t072e_central(name_a, data_a, 0L, 0L, t072e_crc(data_a), length(data_a), 0L),
    t072e_central(name_b, data_b, 0L, 0L, t072e_crc(data_b), length(data_b),
                  length(local_a) + 4L)
  )
  eocd <- c(
    t072e_raw("PK\005\006"), raw(4L), t072e_le16(2L), t072e_le16(2L),
    t072e_le32(length(central)), t072e_le32(length(local_a)), raw(2L)
  )
  writeBin(c(local_a, central, eocd), overlap)
  expect_false(t072e_verdict(reader, overlap, ".zip")$ok)

  first <- t072e_zip_parts(list(list(name = "pkg/first", data = t072e_raw("1"))))
  second <- t072e_zip_parts(list(list(name = "pkg/second", data = t072e_raw("2"))))
  first_size <- length(first$locals) + length(first$central) + length(first$eocd)
  second$central[43L:46L] <- t072e_le32(first_size)
  second_central_offset <- first_size + length(second$locals)
  second$eocd[17L:20L] <- t072e_le32(second_central_offset)
  two_archives <- file.path(root, "two-archives.zip")
  writeBin(c(first$locals, first$central, first$eocd,
             second$locals, second$central, second$eocd), two_archives)
  expect_false(t072e_verdict(reader, two_archives, ".zip")$ok)
})

test_that("ZIP data descriptors are exact and agree with the central directory", {
  root <- tempfile("round072e-descriptor-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  data <- t072e_raw("descriptor payload")
  crc <- t072e_crc(data)
  fields <- c(t072e_le32(crc), t072e_le32(length(data)), t072e_le32(length(data)))
  descriptors <- list(
    unsigned = fields,
    signed = c(as.raw(c(80L, 75L, 7L, 8L)), fields)
  )
  for (label in names(descriptors)) {
    archive <- file.path(root, paste0(label, ".zip"))
    t072e_write_parts(archive, t072e_zip_parts(list(
      list(name = "pkg/file", data = data, flags = 8L,
           local_crc = 0L, local_csize = 0L, local_usize = 0L,
           descriptor = descriptors[[label]])
    )))
    result <- t072e_verdict(reader, archive, ".zip")
    expect_true(result$ok, info = label)
  }
  for (label in c("wrong-crc", "wrong-size", "missing")) {
    archive <- file.path(root, paste0(label, ".zip"))
    descriptor <- switch(
      label,
      `wrong-crc` = c(t072e_le32(0L), t072e_le32(length(data)), t072e_le32(length(data))),
      `wrong-size` = c(t072e_le32(crc), t072e_le32(0L), t072e_le32(length(data))),
      `missing` = raw()
    )
    t072e_write_parts(archive, t072e_zip_parts(list(
      list(name = "pkg/file", data = data, flags = 8L,
           local_crc = 0L, local_csize = 0L, local_usize = 0L,
           descriptor = descriptor)
    )))
    expect_false(t072e_verdict(reader, archive, ".zip")$ok, info = label)
  }
})

test_that("zlib verifies stored and deflate CRCs without an R CRC pass", {
  root <- tempfile("round072e-crc-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  for (method in c(0L, 8L)) {
    archive <- file.path(root, paste0("bad-", method, ".zip"))
    t072e_write_parts(archive, t072e_zip_parts(list(
      list(name = "pkg/file", data = t072e_raw("crc payload"), method = method,
           crc = 0L)
    )))
    result <- t072e_verdict(reader, archive, ".zip")
    expect_false(result$ok, info = method)
    expect_match(result$message, "CRC-32", fixed = TRUE, info = method)
  }
})

test_that("Deflate requires a final block and consumes its declared bytes", {
  root <- tempfile("round072e-deflate-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  data <- t072e_raw("not final")
  no_final <- c(
    as.raw(0L), t072e_le16(length(data)),
    t072e_le16(65535L - length(data)), data
  )
  archive <- file.path(root, "no-final.zip")
  t072e_write_parts(archive, t072e_zip_parts(list(
    list(name = "pkg/file", data = data, method = 8L, compressed = no_final)
  )))
  expect_false(t072e_verdict(reader, archive, ".zip")$ok)

  trailing <- file.path(root, "deflate-trailing.zip")
  compressed <- t072e_deflate(data)
  t072e_write_parts(trailing, t072e_zip_parts(list(
    list(name = "pkg/file", data = data, method = 8L,
         compressed = c(compressed, raw(1L)))
  )))
  expect_false(t072e_verdict(reader, trailing, ".zip")$ok)
})

test_that("metadata accounting includes TAR metadata headers and EOCD comments", {
  root <- tempfile("round072e-accounting-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  reader <- get(".archive_inspect", asNamespace("bigbang"))

  pax <- t072e_empty_pax_tar(file.path(root, "pax.tar"), 3L)
  withr::local_options(bigbang.archive.max_entries = 3L)
  expect_error(reader(pax, ".tar"), class = "bigbang_error_archive_limit")
  withr::local_options(bigbang.archive.max_entries = 100L)
  withr::local_options(bigbang.archive.max_metadata_bytes = 512L * 3L)
  expect_error(reader(pax, ".tar"), class = "bigbang_error_archive_limit")

  gnu <- t072e_empty_pax_tar(file.path(root, "gnu.tar"), 2L, type = "L")
  withr::local_options(bigbang.archive.max_entries = 1L)
  expect_error(reader(gnu, ".tar"), class = "bigbang_error_archive_limit")

  comment <- file.path(root, "comment.zip")
  t072e_write_parts(comment, t072e_zip_parts(
    list(list(name = "pkg/file", data = raw())), comment = raw(200L)
  ))
  withr::local_options(bigbang.archive.max_entries = 100L)
  withr::local_options(bigbang.archive.max_metadata_bytes = 100L)
  expect_error(reader(comment, ".zip"), class = "bigbang_error_archive_limit")
})

test_that("case folding is locale-independent and NFD names remain valid", {
  reader <- get(".archive_inspect", asNamespace("bigbang"))
  key <- bigbang:::.archive_case_key
  old <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  locales <- if (identical(.Platform$OS.type, "windows")) {
    "C"
  } else {
    c("C", "C.UTF-8")
  }
  for (locale in locales) {
    expect_false(is.na(Sys.setlocale("LC_CTYPE", locale)), info = locale)
    expect_identical(key("pkg/\u00c4.txt"), key("pkg/\u00e4.txt"), info = locale)
    expect_identical(key("pkg/\u0100.txt"), key("pkg/\u0101.txt"), info = locale)
    expect_identical(key("pkg/\u0391.txt"), key("pkg/\u03b1.txt"), info = locale)
    expect_identical(key("pkg/\u0416.txt"), key("pkg/\u0436.txt"), info = locale)
  }
  root <- tempfile("round072e-nfd-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- file.path(root, "nfd.zip")
  t072e_write_parts(archive, t072e_zip_parts(list(
    list(name = "pkg/cafe\u0301.txt", data = t072e_raw("nfd"))
  )))
  expect_true(t072e_verdict(reader, archive, ".zip")$ok)
})

test_that("the generated installer receives the complete 072e corpus", {
  root <- tempfile("round072e-parity-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  overlap <- file.path(root, "overlap.zip")
  normal <- t072e_zip_parts(list(list(name = "pkg/file", data = t072e_raw("ok"))))
  writeBin(c(normal$locals, normal$central, normal$eocd), overlap)
  bad_crc <- file.path(root, "bad-crc.zip")
  t072e_write_parts(bad_crc, t072e_zip_parts(list(
    list(name = "pkg/file", data = t072e_raw("bad"), crc = 0L)
  )))
  comment <- file.path(root, "comment.zip")
  t072e_write_parts(comment, t072e_zip_parts(
    list(list(name = "pkg/file", data = raw())), comment = raw(200L)
  ))
  generated <- t072e_generated_reader()
  package_reader <- get(".archive_inspect", asNamespace("bigbang"))
  generated_reader <- get(".archive_inspect", generated)
  for (archive in c(overlap, bad_crc, comment)) {
    package_result <- t072e_verdict(package_reader, archive, ".zip")
    generated_result <- t072e_verdict(generated_reader, archive, ".zip")
    expect_identical(package_result, generated_result, info = basename(archive))
  }
})
