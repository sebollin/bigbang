r32_tar_field <- function(value, width) {
  bytes <- charToRaw(enc2utf8(value))
  c(bytes, raw(width - length(bytes)))
}

r32_tar_header <- function(name, size = 0L) {
  header <- raw(512L)
  header[1:100] <- r32_tar_field(name, 100L)
  header[101:108] <- r32_tar_field("0000644", 8L)
  header[109:124] <- raw(16L)
  header[125:136] <- r32_tar_field(sprintf("%011o", size), 12L)
  header[137:148] <- r32_tar_field("00000000000", 12L)
  header[149:156] <- r32_tar_field("        ", 8L)
  header[157] <- charToRaw("0")
  header[258:263] <- r32_tar_field("ustar", 6L)
  header[264:265] <- charToRaw("00")
  header[149:156] <- c(
    charToRaw(sprintf("%06o", sum(as.integer(header)))), as.raw(c(32L, 0L))
  )
  header
}

r32_tar_write_gzip <- function(path, trailing = raw()) {
  bytes <- c(r32_tar_header("pkg/file", 1L), charToRaw("x"), raw(511L),
             raw(1024L), trailing)
  connection <- gzfile(path, open = "wb")
  writeBin(bytes, connection)
  close(connection)
  path
}

r32_zip_le16 <- function(value) as.raw(c(value %% 256, value %/% 256))
r32_zip_le32 <- function(value) {
  as.raw(c(value %% 256, value %/% 256 %% 256,
           value %/% 65536 %% 256, value %/% 16777216 %% 256))
}

r32_zip_stored <- function(path, name = "pkg/file", data = raw(4096L),
                           comment = raw()) {
  name_raw <- charToRaw(name)
  crc <- bigbang:::.archive_crc32_unsigned(bigbang:::.archive_crc32(data))
  local <- c(charToRaw("PK\003\004"), r32_zip_le16(20L), raw(2L),
             raw(2L), raw(4L), r32_zip_le32(crc), r32_zip_le32(length(data)),
             r32_zip_le32(length(data)), r32_zip_le16(length(name_raw)),
             raw(2L), name_raw, data)
  central <- c(charToRaw("PK\001\002"), r32_zip_le16(20L), r32_zip_le16(20L),
               raw(4L), raw(4L), r32_zip_le32(crc),
               r32_zip_le32(length(data)), r32_zip_le32(length(data)),
               r32_zip_le16(length(name_raw)), raw(8L), raw(4L),
               r32_zip_le32(0L), name_raw)
  eocd <- c(charToRaw("PK\005\006"), raw(4L), r32_zip_le16(1L),
            r32_zip_le16(1L), r32_zip_le32(length(central)),
            r32_zip_le32(length(local)), r32_zip_le16(length(comment)), comment)
  writeBin(c(local, central, eocd), path)
  path
}

r32_archive <- function(root, file_stem, package = file_stem,
                        version = "0.1.0", imports = NULL,
                        code = "value <- function() 1L") {
  package_root <- file.path(root, paste0("src-", file_stem))
  dir.create(file.path(package_root, "R"), recursive = TRUE)
  description <- c(
    paste0("Package: ", package), paste0("Version: ", version),
    "Title: Round 074 R32 fixture",
    "Description: Temporary fixture for the R32 regression tests.",
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    "License: MIT",
    if (is.null(imports)) character() else paste0("Imports: ", imports)
  )
  writeLines(description, file.path(package_root, "DESCRIPTION"))
  writeLines("export(value)", file.path(package_root, "NAMESPACE"))
  writeLines(code, file.path(package_root, "R", "value.R"))
  archive <- file.path(root, paste0(file_stem, "_", version, ".tar.gz"))
  withr::with_dir(root, utils::tar(archive, basename(package_root),
                                   compression = "gzip", tar = "internal"))
  archive
}

r32_generated_install <- function(generated, name, pkg_dir, lib, ...) {
  runtime <- new.env(parent = baseenv())
  files <- list.files(file.path(generated$path, "R"), full.names = TRUE)
  for (file in files) sys.source(file, runtime)
  runtime[[paste0(name, "_install")]](
    pkg_dir = pkg_dir, lib = lib, verbose = FALSE, ...
  )
}

test_that("case folding covers every table range and the U+0149-U+014F gap", {
  old_locale <- Sys.getlocale("LC_CTYPE")
  withr::defer(Sys.setlocale("LC_CTYPE", old_locale))
  locale <- Sys.setlocale("LC_CTYPE", "C.UTF-8")
  skip_if(identical(locale, ""), "no UTF-8 locale is available")

  table <- bigbang:::.archive_case_fold_table
  special <- c(0x017fL, 0x03c2L)
  for (index in seq_along(table)) {
    upper <- intToUtf8(as.integer(names(table)[[index]]))
    expected <- intToUtf8(unname(table[[index]]))
    expect_identical(bigbang:::.archive_case_key(upper), expected)
    if (!(as.integer(names(table)[[index]]) %in% special)) {
      expect_identical(tolower(upper), expected)
    }
  }

  expect_identical(bigbang:::.archive_case_key("\u014a"), "\u014b")
  expect_identical(bigbang:::.archive_case_key("\u014c"), "\u014d")
  expect_identical(bigbang:::.archive_case_key("\u014e"), "\u014f")
  expect_identical(bigbang:::.archive_case_key("\u0149"), "\u0149")
  expect_false(identical(bigbang:::.archive_case_key("\u014a"),
                         bigbang:::.archive_case_key("\u014c")))
})

test_that("all newly reserved Windows device spellings are rejected", {
  limits <- bigbang:::.archive_limit_options()
  devices <- c("CONIN$", "CONOUT$", "COM0", "LPT0", "COM\u00b9", "COM\u00b2",
               "COM\u00b3", "LPT\u00b9", "LPT\u00b2", "LPT\u00b3")
  for (device in devices) {
    expect_error(
      bigbang:::.archive_validate_name_text(paste0("pkg/", device, ".dat"), limits),
      class = "bigbang_archive_parse_error"
    )
  }
})

test_that("compressed ZIP bytes are charged before Deflate is read", {
  root <- tempfile("round074-r32-zip-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r32_zip_stored(file.path(root, "large.zip"))
  withr::local_options(
    bigbang.archive.max_total_bytes = 100,
    bigbang.archive.max_metadata_bytes = 1e6
  )
  started <- proc.time()[["elapsed"]]
  condition <- expect_error(
    bigbang:::.archive_inspect(archive, ".zip"),
    class = "bigbang_error_archive_limit"
  )
  expect_lt(proc.time()[["elapsed"]] - started, 1)
  expect_match(conditionMessage(condition), "total archive payload bytes", fixed = TRUE)
})

test_that("the structurally consistent EOCD wins over an EOCD in its comment", {
  root <- tempfile("round074-r32-eocd-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  plain <- file.path(root, "plain.zip")
  r32_zip_stored(plain, data = charToRaw("x"))
  plain_bytes <- readBin(plain, "raw", file.size(plain))
  comment <- plain_bytes[(length(plain_bytes) - 21L):length(plain_bytes)]
  archive <- file.path(root, "comment.zip")
  r32_zip_stored(archive, data = charToRaw("x"), comment = comment)
  expect_s3_class(bigbang:::.archive_inspect(archive, ".zip"), "data.frame")
})

test_that("gzip TAR validation checks CRC and ISIZE and charges trailing zeros", {
  root <- tempfile("round074-r32-tar-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  intact <- r32_tar_write_gzip(file.path(root, "intact.tar.gz"))
  bytes <- readBin(intact, "raw", file.size(intact))
  bytes[[length(bytes)]] <- as.raw(bitwXor(as.integer(bytes[[length(bytes)]]), 1L))
  corrupt <- file.path(root, "bad-isize.tar.gz")
  writeBin(bytes, corrupt)
  expect_error(bigbang:::.archive_inspect(corrupt, ".tar.gz"),
               class = "bigbang_error_archive_format")

  trailing <- r32_tar_write_gzip(file.path(root, "trailing.tar.gz"), raw(512L))
  withr::local_options(
    bigbang.archive.max_total_bytes = 2048,
    bigbang.archive.max_metadata_bytes = 2048
  )
  expect_error(bigbang:::.archive_inspect(trailing, ".tar.gz"),
               class = "bigbang_error_archive_limit")
})

test_that("installation diagnostics are byte-safe and use the final error block", {
  output <- c("configure: failed program was:", "SECRET_SHOULD_NOT_BE_SELECTED",
              sprintf("ordinary line %02d", 3:17),
              "Error: first failure", "more context",
              "ERROR: fatal configuration error")
  excerpt <- bigbang:::.installation_error_excerpt(output)
  expect_match(excerpt, "fatal configuration error", fixed = TRUE)
  expect_false(grepl("SECRET_SHOULD_NOT_BE_SELECTED", excerpt, fixed = TRUE))

  tail_output <- sprintf("line %02d", 1:25)
  expect_match(bigbang:::.installation_error_excerpt(tail_output), "line 25", fixed = TRUE)
  expect_false(grepl("line 01", bigbang:::.installation_error_excerpt(tail_output), fixed = TRUE))

  invalid <- rawToChar(as.raw(c(charToRaw("ERROR: bad byte "), 0xffL)))
  expect_no_error(bigbang:::.installation_error_excerpt(invalid))
})

test_that("metapackage names compare with archive case folding and visible locals", {
  root <- tempfile("round074-r32-names-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  component <- r32_archive(root, "component074", package = "component074")
  expect_error(
    create_metapackage("Component074", component, dest_dir = root,
                       document = FALSE, verbose = FALSE, force_deps = character()),
    class = "bigbang_error_metapackage_component_collision"
  )

  local_dep <- r32_archive(root, "localdep074", package = "localdep074")
  expect_error(
    create_metapackage(
      "localdep074", component, pkg_dir = root, dest_dir = file.path(root, "dep-meta"),
      document = FALSE, verbose = FALSE, force_deps = character(),
      tolerate = "unincluded_local_dep"
    ),
    class = "bigbang_error_metapackage_component_collision"
  )

  renamed <- r32_archive(root, "metaname074", package = "embedded074")
  expect_error(
    create_metapackage("metaname074", renamed, dest_dir = file.path(root, "file-meta"),
                       document = FALSE, verbose = FALSE, force_deps = character()),
    class = "bigbang_error_metapackage_component_collision"
  )
  expect_true(file.exists(local_dep))
})

test_that("failed and skipped diamonds classify every reachable node in both installers", {
  make_diamond <- function(root, broken = FALSE, missing = FALSE) {
    base <- r32_archive(
      root, "r32base074", imports = if (missing) "r32missing074" else NULL,
      code = if (broken) "value <- function( {" else "value <- function() 1L"
    )
    left <- r32_archive(root, "r32left074", imports = "r32base074")
    right <- r32_archive(root, "r32right074", imports = "r32base074")
    top <- r32_archive(root, "r32top074", imports = "r32left074, r32right074")
    c(base = base, left = left, right = right, top = top)
  }
  categories <- function(result) {
    c(names(result$installed), names(result$unchanged), names(result$failed),
      names(result$skipped))
  }

  root <- tempfile("round074-r32-diamond-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archives <- make_diamond(root, broken = TRUE)
  direct <- suppressWarnings(install_local_pkg(
    archives[["top"]], pkg_dir = root, lib = file.path(root, "direct"),
    upgrade = "always", verbose = FALSE
  ))
  expect_setequal(names(direct$failed), "r32base074_0.1.0")
  expect_setequal(names(direct$skipped), c(
    "r32left074_0.1.0", "r32right074_0.1.0", "r32top074_0.1.0"
  ))
  expect_length(categories(direct), 4L)
  expect_length(unique(categories(direct)), 4L)

  generated <- create_metapackage(
    "r32meta074", unname(archives), dest_dir = file.path(root, "generated"),
    document = FALSE, verbose = FALSE, force_deps = character()
  )
  generated_error <- suppressWarnings(tryCatch(r32_generated_install(
    generated, "r32meta074", root, file.path(root, "generated-lib"),
    upgrade = "always"
  ), error = identity))
  expect_s3_class(generated_error, "bigbang_error_install")
  expect_setequal(names(generated_error$failures), "r32base074_0.1.0")

  skipped_root <- tempfile("round074-r32-skipped-")
  dir.create(skipped_root)
  withr::defer(unlink(skipped_root, recursive = TRUE, force = TRUE))
  skipped_archives <- make_diamond(skipped_root, missing = TRUE)
  skipped <- suppressWarnings(install_local_pkg(
    skipped_archives[["top"]], pkg_dir = skipped_root,
    lib = file.path(skipped_root, "lib"), cran_deps = "skip",
    upgrade = "always", verbose = FALSE
  ))
  expect_length(skipped$failed, 0L)
  expect_length(skipped$skipped, 4L)
  expect_length(unique(categories(skipped)), 4L)

  only <- suppressWarnings(install_local_pkg(
    archives[["left"]], pkg_dir = root, lib = file.path(root, "only-lib"),
    upgrade = "always", verbose = FALSE
  ))
  expect_setequal(names(only$failed), "r32base074_0.1.0")
  expect_setequal(names(only$skipped), "r32left074_0.1.0")
  expect_length(unique(categories(only)), 2L)
})

test_that("blank repositories, including NA, stay offline", {
  withr::local_language("C")
  root <- tempfile("round074-r32-repos-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- r32_archive(root, "r32repo074", imports = "r32missing074")
  for (repos in list("   ", NA_character_)) {
    result <- install_local_pkg(
      archive, lib = file.path(root, paste0("lib-", length(list.files(root)))),
      cran_deps = "install", repos = repos, upgrade = "always", verbose = FALSE
    )
    expect_match(result$failed[[1L]], "configured repository", ignore.case = TRUE)
  }
})
