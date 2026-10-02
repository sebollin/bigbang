copy_toy_archive <- function(destination) {
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  source <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(source)) {
    source <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  file.copy(source, destination)
  invisible(destination)
}

generate_toy_metapackage <- function(name, archives, destination,
                                     document = FALSE) {
  create_metapackage(
    name = name,
    packages = "toycomponent_0.1.0",
    pkg_dir = archives,
    dest_dir = destination,
    document = document,
    verbose = FALSE,
    import_deps = character(),
    force_deps = character()
  )
}

test_that("relative destination and archive paths generate a complete project", {
  sandbox <- tempfile("bigbang-relative-generation-")
  dir.create(sandbox)
  copy_toy_archive(file.path(sandbox, "archives"))

  result <- withr::with_dir(sandbox, {
    dir.create("output")
    cwd_before <- getwd()
    generated <- generate_toy_metapackage(
      "relverse", "archives", "output", document = FALSE
    )
    expect_identical(getwd(), cwd_before)
    generated
  })

  expect_identical(
    result$path,
    normalizePath(file.path(sandbox, "output", "relverse"), winslash = "/")
  )
  expect_true(file.exists(file.path(result$path, "DESCRIPTION")))
  expect_true(file.exists(file.path(result$path, "NAMESPACE")))
  expect_setequal(
    list.files(file.path(result$path, "R")),
    c("attach.R", "install_packages.R", "utils.R", "zzz.R")
  )

  text_files <- list.files(
    result$path,
    pattern = "(\\.(R|Rd|Rmd|po|pot)$|DESCRIPTION$|NAMESPACE$)",
    recursive = TRUE,
    full.names = TRUE
  )
  generated_text <- unlist(lapply(
    text_files, readLines, warn = FALSE, encoding = "UTF-8"
  ), use.names = FALSE)
  helper_identifiers <- regmatches(
    generated_text,
    gregexpr(
      "\\b[A-Za-z][A-Za-z0-9.]*verse_(load_all|detach|packages|attach_all)\\b",
      generated_text,
      perl = TRUE
    )
  )
  helper_identifiers <- unique(unlist(helper_identifiers, use.names = FALSE))
  expect_true(length(helper_identifiers) > 0L)
  expect_true(all(startsWith(helper_identifiers, "relverse_")))
})

test_that("failed generation restores cwd and rolls back only its own project", {
  sandbox <- tempfile("bigbang-generation-rollback-")
  dir.create(sandbox)
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(archives)
  dir.create(destination)
  cwd_before <- getwd()

  expect_error(
    create_metapackage(
      "retryverse", "missing_0.1.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "archives were not found"
  )
  expect_identical(getwd(), cwd_before)
  expect_false(dir.exists(file.path(destination, "retryverse")))

  newly_created_parent <- file.path(sandbox, "output-created-by-call")
  expect_error(
    create_metapackage(
      "parentverse", "missing_0.1.0", archives,
      dest_dir = newly_created_parent, document = FALSE, verbose = FALSE
    ),
    "archives were not found"
  )
  expect_false(dir.exists(newly_created_parent))

  copy_toy_archive(archives)
  result <- generate_toy_metapackage(
    "retryverse", archives, destination, document = FALSE
  )
  expect_true(file.exists(file.path(result$path, "R", "attach.R")))
  expect_identical(getwd(), cwd_before)

  preexisting <- file.path(destination, "preexistingverse")
  dir.create(preexisting)
  expect_error(
    create_metapackage(
      "preexistingverse", "missing_0.1.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE
    ),
    "archives were not found"
  )
  expect_true(dir.exists(preexisting))
  # Component resolution is a preflight step, so a failed call does not create
  # a partial project inside a pre-existing empty directory.
  expect_false(dir.exists(file.path(preexisting, "R")))
  expect_identical(getwd(), cwd_before)
})

test_that("post-scaffold failures exercise the project and parent rollback", {
  sandbox <- tempfile("bigbang-post-scaffold-rollback-")
  dir.create(sandbox)
  archives <- file.path(sandbox, "archives")
  copy_toy_archive(archives)

  testthat::local_mocked_bindings(
    write_basic_vignette = function(...) stop("forced post-scaffold failure"),
    .package = "bigbang"
  )

  existing_parent <- file.path(sandbox, "existing-parent")
  dir.create(existing_parent)
  expect_error(
    create_metapackage(
      "postscaffoldverse", "toycomponent_0.1.0", archives,
      dest_dir = existing_parent, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character()
    ),
    "forced post-scaffold failure"
  )
  expect_false(dir.exists(file.path(existing_parent, "postscaffoldverse")))
  expect_true(dir.exists(existing_parent))

  new_parent <- file.path(sandbox, "new-parent")
  expect_error(
    create_metapackage(
      "newpostscaffoldverse", "toycomponent_0.1.0", archives,
      dest_dir = new_parent, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character()
    ),
    "forced post-scaffold failure"
  )
  expect_false(dir.exists(new_parent))
})

test_that("rollback reports an unsuccessful unlink", {
  sandbox <- tempfile("bigbang-unlink-rollback-")
  dir.create(sandbox)
  archives <- file.path(sandbox, "archives")
  copy_toy_archive(archives)
  parent <- file.path(sandbox, "parent")
  dir.create(parent)

  testthat::local_mocked_bindings(
    write_basic_vignette = function(...) stop("forced unlink failure"),
    .rollback_unlink = function(...) 1L,
    .package = "bigbang"
  )
  expect_warning(
    expect_error(
      create_metapackage(
        "unlinkfailureverse", "toycomponent_0.1.0", archives,
        dest_dir = parent, document = FALSE, verbose = FALSE,
        import_deps = character(), force_deps = character()
      ),
      "forced unlink failure"
    ),
    "Could not remove completely"
  )

  # The mocked function deliberately left the tree in place; clean it with the
  # base binding after the safety branch has been exercised.
  base::unlink(file.path(parent, "unlinkfailureverse"),
               recursive = TRUE, force = TRUE)
  expect_true(dir.exists(parent))
})

test_that("safe_unlink uses temporary location rather than a basename", {
  sandbox <- tempfile("bigbang-safe-unlink-")
  source_tree <- file.path(sandbox, "templates")
  dir.create(file.path(source_tree, "R"), recursive = TRUE)
  writeLines(c("Package: templates", "Version: 1.0.0"),
             file.path(source_tree, "DESCRIPTION"))
  # A basename such as 'templates' is acceptable when the complete path is
  # inside the session's temporary directory.
  expect_identical(safe_unlink(source_tree, recursive = TRUE, force = TRUE), 0L)
  expect_false(dir.exists(source_tree))

  temporary_tree <- file.path(tempdir(), paste0("bigbang-safe-template-", as.integer(Sys.time())))
  dir.create(file.path(temporary_tree, "R"), recursive = TRUE)
  writeLines(c("Package: temporary", "Version: 1.0.0"),
             file.path(temporary_tree, "DESCRIPTION"))
  expect_identical(safe_unlink(temporary_tree, recursive = TRUE, force = TRUE), 0L)
  expect_false(dir.exists(temporary_tree))
})

test_that("successful documentation restores the caller session", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-document-session-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(destination, recursive = TRUE)
  copy_toy_archive(archives)

  search_before <- search()
  namespaces_before <- loadedNamespaces()
  cwd_before <- getwd()
  result <- suppressWarnings(generate_toy_metapackage(
    "docverse", archives, destination, document = TRUE
  ))

  expect_true(result$documented)
  expect_identical(search(), search_before)
  expect_setequal(loadedNamespaces(), namespaces_before)
  expect_identical(getwd(), cwd_before)
  expect_false("docverse" %in% loadedNamespaces())
  expect_false("package:docverse" %in% search())
  if (!"devtools_shims" %in% search_before) {
    expect_false("devtools_shims" %in% search())
  }
})

test_that("failed documentation is reported and restores the caller session", {
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-document-failure-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(destination, recursive = TRUE)
  copy_toy_archive(archives)

  search_before <- search()
  namespaces_before <- loadedNamespaces()
  cwd_before <- getwd()
  result <- NULL
  expect_warning(
    result <- testthat::with_mocked_bindings(
      generate_toy_metapackage(
        "faileddocverse", archives, destination, document = TRUE
      ),
      document = function(pkg, ...) {
        suppressPackageStartupMessages(devtools::load_all(pkg, quiet = TRUE))
        stop("forced documentation failure")
      },
      .package = "devtools"
    ),
    "Error generating documentation: forced documentation failure"
  )

  expect_false(result$documented)
  expect_identical(search(), search_before)
  expect_setequal(loadedNamespaces(), namespaces_before)
  expect_identical(getwd(), cwd_before)
  expect_false("faileddocverse" %in% loadedNamespaces())
  expect_false("package:faileddocverse" %in% search())
  if (!"devtools_shims" %in% search_before) {
    expect_false("devtools_shims" %in% search())
  }
})

test_that("a promotion failure rolls the project back completely", {
  skip_on_cran()
  skip_if_not_installed("devtools")
  sandbox <- tempfile("bigbang-document-promotion-rollback-")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "output")
  dir.create(destination, recursive = TRUE)
  copy_toy_archive(archives)
  initial <- generate_toy_metapackage(
    "promotionverse", archives, destination, document = FALSE
  )
  documentation <- .planned_documentation_files("promotionverse")
  for (relative in documentation) {
    writeLines(paste("old", relative), file.path(initial$path, relative),
               useBytes = TRUE)
  }
  manifest <- .read_generation_manifest(initial$path)
  .atomic_save_rds(
    .manifest_records(initial$path, unique(c(manifest$files, documentation))),
    file.path(initial$path, .generation_manifest_name)
  )
  promotion_snapshot <- function(path) {
    relative <- list.files(path, all.files = TRUE, recursive = TRUE,
                           no.. = TRUE, include.dirs = TRUE)
    full <- file.path(path, relative)
    info <- file.info(full)
    hashes <- rep(NA_character_, length(full))
    hashes[!info$isdir] <- unname(as.character(tools::md5sum(full[!info$isdir])))
    data.frame(path = relative, directory = info$isdir, hash = hashes,
               stringsAsFactors = FALSE)
  }
  before <- promotion_snapshot(initial$path)
  original_copy <- .atomic_copy
  calls <- 0L
  testthat::local_mocked_bindings(
    document = function(pkg, ...) {
      for (relative in documentation) {
        writeLines(paste("new", relative), file.path(pkg, relative),
                   useBytes = TRUE)
      }
      invisible(TRUE)
    },
    .package = "devtools"
  )
  failed <- NULL
  expect_error(failed <- testthat::with_mocked_bindings(
    create_metapackage(
      "promotionverse", "toycomponent_0.1.0", pkg_dir = archives,
      dest_dir = destination, document = TRUE, update = TRUE,
      verbose = FALSE, import_deps = character(), force_deps = character()
    ),
    .atomic_copy = function(source, destination) {
      if (grepl("bigbang-document-staging-", source, fixed = TRUE) &&
            startsWith(destination, initial$path) &&
            grepl("/man/", destination, fixed = TRUE)) {
        calls <<- calls + 1L
        if (calls == 2L) stop("forced documentation promotion failure")
      }
      original_copy(source, destination)
    },
    .package = "bigbang"
  ), "forced documentation promotion failure", fixed = TRUE)
  expect_identical(promotion_snapshot(initial$path), before)
  expect_true(.manifest_matches_project(initial$path))
  expect_false(dir.exists(.update_journal_path(initial$path)))
})

test_that("illegal package names are rejected before anything is written", {
  sandbox <- tempfile("bigbang-name-validation-")
  dir.create(sandbox)
  copy_toy_archive(file.path(sandbox, "archives"))
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(destination)
  neighbour <- file.path(sandbox, "neighbour")
  dir.create(neighbour)

  # A name carrying a parent reference or a separator would otherwise place the
  # generated tree outside the requested destination.
  illegal_names <- c(
    "../escaped", "sub/nested", "./here", "a/../b",
    "1leading", "trailing.", ".hidden", "x", ""
  )

  for (illegal in illegal_names) {
    expect_error(
      generate_toy_metapackage(illegal, archives, destination),
      regexp = "valid R package name|one character string",
      info = illegal
    )
  }

  # Nothing was created, neither inside the destination nor beside it.
  expect_length(list.files(destination, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(list.files(neighbour, all.files = TRUE, no.. = TRUE), 0L)
  expect_setequal(
    basename(list.dirs(sandbox, recursive = FALSE)),
    c("archives", "destination", "neighbour")
  )
})

test_that("the underscore message keeps precedence over the generic one", {
  sandbox <- tempfile("bigbang-name-underscore-")
  dir.create(sandbox)
  copy_toy_archive(file.path(sandbox, "archives"))
  destination <- file.path(sandbox, "destination")
  dir.create(destination)

  expect_error(
    generate_toy_metapackage(
      "team_verse", file.path(sandbox, "archives"), destination
    ),
    regexp = "underscores"
  )
})

test_that("legal package names spanning the grammar are accepted", {
  skip_on_cran()
  sandbox <- tempfile("bigbang-name-legal-")
  dir.create(sandbox)
  copy_toy_archive(file.path(sandbox, "archives"))
  archives <- file.path(sandbox, "archives")

  for (legal in c("ab", "teamverse", "team.verse", "verse2", "A9.b")) {
    destination <- file.path(sandbox, paste0("out-", legal))
    dir.create(destination)
    result <- generate_toy_metapackage(legal, archives, destination)
    expect_true(dir.exists(result$path), info = legal)
    expect_true(
      file.exists(file.path(result$path, "R", "install_packages.R")),
      info = legal
    )
  }
})

test_that("rollback works when a path component is a symbolic link", {
  # This is the macOS situation on every platform: tempdir() there sits under
  # /var, a link to /private/var. Both sides of the ownership comparison must
  # be normalised symmetrically after the project has been created.
  sandbox <- tempfile("bigbang-rollback-symlink-")
  dir.create(sandbox)
  real <- file.path(sandbox, "real")
  link <- file.path(sandbox, "link")
  archives <- file.path(sandbox, "archives")
  dir.create(real)
  dir.create(archives)
  linked <- bb_dir_link(real, link)
  skip_if_not(isTRUE(linked), "This platform cannot create directory links.")

  copy_toy_archive(archives)
  destination <- file.path(link, "created-by-the-call")
  testthat::local_mocked_bindings(
    write_basic_vignette = function(...) stop("forced symlink rollback failure"),
    .package = "bigbang"
  )
  expect_error(
    create_metapackage(
      "linkverse", "toycomponent_0.1.0", archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character()
    ),
    "forced symlink rollback failure"
  )
  expect_false(dir.exists(file.path(destination, "linkverse")))
  expect_false(dir.exists(destination))

  # A destination the call did not create is still left alone.
  preexisting <- file.path(link, "preexisting")
  dir.create(preexisting)
  expect_error(
    create_metapackage(
      "linkverse", "missing_0.1.0", archives,
      dest_dir = preexisting, document = FALSE, verbose = FALSE
    ),
    "archives were not found"
  )
  expect_false(dir.exists(file.path(preexisting, "linkverse")))
  expect_true(dir.exists(preexisting))
})

test_that("create_metapackage help is generated from roxygen comments", {
  skip_on_cran()
  skip_if_not_installed("roxygen2")
  package_root <- normalizePath(testthat::test_path("..", ".."),
                                winslash = "/", mustWork = TRUE)
  if (!file.exists(file.path(package_root, "DESCRIPTION")) ||
        !file.exists(file.path(package_root, "R", "create_metapackage.R")) ||
        !file.exists(file.path(package_root, "man", "create_metapackage.Rd"))) {
    # R CMD check runs tests from an installed copy, which intentionally has
    # no source tree for roxygen2 to parse. The source-copy comparison runs
    # in the repository and in the standalone source test above.
    expect_true(TRUE)
    return(invisible(NULL))
  }
  copy_root <- tempfile("bigbang-roxygen-")
  unlink(copy_root, recursive = TRUE, force = TRUE)
  dir.create(copy_root, recursive = TRUE)
  on.exit(unlink(copy_root, recursive = TRUE, force = TRUE), add = TRUE)
  for (entry in c("DESCRIPTION", "NAMESPACE", "R", "man")) {
    source_entry <- file.path(package_root, entry)
    target_entry <- file.path(copy_root, entry)
    if (!file.exists(source_entry) && !dir.exists(source_entry)) next
    if (dir.exists(source_entry)) {
      dir.create(target_entry, recursive = TRUE)
      children <- list.files(source_entry, all.files = TRUE, no.. = TRUE,
                             full.names = TRUE)
      if (length(children) > 0L) {
        file.copy(children, target_entry, recursive = TRUE)
      }
    } else {
      file.copy(source_entry, target_entry)
    }
  }
  root_literal <- encodeString(normalizePath(copy_root, winslash = "/",
                                             mustWork = TRUE), quote = "\"")
  expected_path <- file.path(package_root, "man", "create_metapackage.Rd")
  if (file.exists(expected_path)) {
    source_literal <- encodeString(package_root, quote = "\"")
    code <- paste0(
      "root <- ", root_literal, "; ",
      "source_root <- ", source_literal, "; ",
      "roxygen2::roxygenise(root, roclets = 'rd'); ",
      "expected <- readLines(file.path(source_root, 'man', 'create_metapackage.Rd'), warn = FALSE); ",
      "generated <- readLines(file.path(root, 'man', 'create_metapackage.Rd'), warn = FALSE); ",
      "if (!identical(generated, expected)) quit(status = 1L)"
    )
  } else {
    code <- paste0(
      "root <- ", root_literal, "; ",
      "roxygen2::roxygenise(root, roclets = 'rd'); ",
      "generated <- paste(readLines(file.path(root, 'man', 'create_metapackage.Rd'), ",
      "warn = FALSE), collapse = '\\n'); ",
      "if (!grepl('reexport_verification', generated, fixed = TRUE)) quit(status = 1L)"
    )
  }
  r_binary <- file.path(
    R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R"
  )
  output <- system2(r_binary, c("--vanilla", "-e", shQuote(code)),
                    stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L, info = paste(output, collapse = "\n"))
})
