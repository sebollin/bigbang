round074_archive <- function(root, name, version = "0.1.0", imports = NULL,
                             code = "value <- function() 1L",
                             namespace = "export(value)") {
  package_root <- file.path(root, name)
  dir.create(file.path(package_root, "R"), recursive = TRUE)
  description <- c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    "Title: Round 074 fixture",
    "Description: Temporary fixture for round 074.",
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    "License: MIT",
    if (is.null(imports)) character() else paste0("Imports: ", imports)
  )
  writeLines(description, file.path(package_root, "DESCRIPTION"))
  writeLines(namespace, file.path(package_root, "NAMESPACE"))
  writeLines(code, file.path(package_root, "R", "value.R"))
  archive <- file.path(root, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(root, utils::tar(archive, name, compression = "gzip"))
  archive
}

round074_local_repo <- function(root, archive) {
  contrib <- file.path(root, "src", "contrib")
  dir.create(contrib, recursive = TRUE)
  file.copy(archive, contrib)
  tools::write_PACKAGES(contrib, type = "source")
  paste0("file://", root)
}

test_that("metapackage/component name collisions fail before writes", {
  skip_on_cran()
  root <- tempfile("bigbang-round074-collision-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- round074_archive(root, "samepkg074")
  destination <- file.path(root, "destination")
  dir.create(destination)

  for (reexport in c(FALSE, TRUE)) {
    target <- file.path(destination, paste0("samepkg074-", reexport))
    condition <- expect_error(
      create_metapackage(
        "samepkg074", archive, dest_dir = target,
        reexport = reexport, document = FALSE, verbose = FALSE,
        force_deps = character()
      ),
      class = "bigbang_error_metapackage_component_collision"
    )
    expect_match(conditionMessage(condition), "samepkg074", fixed = TRUE)
    expect_false(dir.exists(file.path(target, "samepkg074")))
  }

  safe <- create_metapackage(
    "update074", packages = archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, force_deps = character()
  )
  update_archive <- round074_archive(root, "update074")
  project_before <- list.files(
    safe$path, recursive = TRUE, all.files = TRUE, no.. = TRUE
  )
  condition <- expect_error(
    create_metapackage(
      "update074", update_archive, dest_dir = destination, update = TRUE,
      document = FALSE, verbose = FALSE, force_deps = character()
    ),
    class = "bigbang_error_metapackage_component_collision"
  )
  expect_match(conditionMessage(condition), "update074", fixed = TRUE)
  expect_identical(
    list.files(safe$path, recursive = TRUE, all.files = TRUE, no.. = TRUE),
    project_before
  )
})

test_that("direct installation retains the cause and complete log path", {
  skip_on_cran()
  root <- tempfile("bigbang-round074-diagnostics-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  archive <- round074_archive(
    root, "badload074",
    code = c(
      ".onLoad <- function(lib, pkg) stop('Database configuration is missing: set DEMO_CONFIG')",
      "value <- function() 1L"
    )
  )
  lib <- file.path(root, "library")
  result <- expect_no_warning(install_local_pkg(
    archive, lib = lib, verbose = FALSE, upgrade = "always"
  ))
  message <- result$failed[[1L]]
  expect_match(message, "Database configuration is missing", fixed = TRUE)
  expect_match(message, "full installation log", fixed = TRUE)
  log_path <- sub("^.*full installation log: ", "", message)
  expect_true(file.exists(log_path))
  expect_lt(nchar(message), 6000L)

  destination <- file.path(root, "destination")
  dir.create(destination)
  generated <- create_metapackage(
    "diagnosticsverse074", archive, dest_dir = destination,
    document = FALSE, verbose = FALSE, force_deps = character()
  )
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(generated$path, "R", "utils.R"), runtime)
  sys.source(file.path(generated$path, "R", "install_packages.R"), runtime)
  generated_library <- file.path(root, "generated-library")
  dir.create(generated_library)
  generated_result <- runtime$install_local_archive(
    "badload074_0.1.0", file.path(generated$path, "inst", "archives"),
    upgrade = "always", lib = generated_library,
    verbose = FALSE
  )
  expect_false(isTRUE(generated_result$success))
  normalize_install_failure <- function(value) {
    value <- sub("; full installation log: .*$", "", value)
    lines <- strsplit(value, "\\n", fixed = TRUE)[[1L]]
    paste(lines[!grepl("* removing ", lines, fixed = TRUE)], collapse = "\\n")
  }
  direct_without_path <- normalize_install_failure(message)
  generated_without_path <- normalize_install_failure(generated_result$message)
  expect_identical(generated_without_path, direct_without_path)
})

test_that("external version constraints use every operator and explicit repos", {
  skip_on_cran()
  root <- tempfile("bigbang-round074-versions-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  old_dir <- file.path(root, "old")
  repo_root <- file.path(root, "repo")
  dir.create(old_dir)
  old <- round074_archive(old_dir, "externaldep074", "0.1.0")
  new_source <- file.path(root, "new-source")
  new <- round074_archive(new_source, "externaldep074", "0.2.0")
  local_dir <- file.path(root, "local")
  dir.create(local_dir)
  consumer <- round074_archive(
    local_dir, "needsnew074", imports = "externaldep074 (>= 0.2.0)"
  )
  install_lib <- file.path(root, "install-lib")
  dir.create(install_lib)
  withr::local_libpaths(c(install_lib, .libPaths()))
  expect_length(install_local_pkg(old, lib = install_lib, verbose = FALSE)$failed, 0L)

  first_library <- file.path(root, "first-library")
  second_library <- file.path(root, "second-library")
  dir.create(first_library)
  dir.create(second_library)
  expect_length(install_local_pkg(old, lib = first_library, verbose = FALSE)$failed, 0L)
  expect_length(install_local_pkg(new, lib = second_library, verbose = FALSE)$failed, 0L)
  ordered_consumer <- round074_archive(
    local_dir, "ordered074", imports = "externaldep074 (>= 0.2.0)"
  )
  withr::local_libpaths(c(first_library, second_library, .libPaths()))
  first_loaded <- install_local_pkg(
    ordered_consumer, pkg_dir = local_dir, lib = file.path(root, "ordered-lib"),
    cran_deps = "error", upgrade = "always", verbose = FALSE
  )
  expect_match(first_loaded$failed[[1L]], "0.1.0", fixed = TRUE)

  repo <- round074_local_repo(repo_root, new)
  skipped <- install_local_pkg(
    consumer, pkg_dir = local_dir, lib = install_lib,
    cran_deps = "skip", upgrade = "always", verbose = FALSE
  )
  expect_match(skipped$skipped[[1L]], "externaldep074 >= 0.2.0", fixed = TRUE)
  expect_match(skipped$skipped[[1L]], "0.1.0", fixed = TRUE)

  errored <- install_local_pkg(
    consumer, pkg_dir = local_dir, lib = install_lib,
    cran_deps = "error", upgrade = "always", verbose = FALSE
  )
  expect_match(errored$failed[[1L]], "externaldep074 >= 0.2.0", fixed = TRUE)

  installed <- install_local_pkg(
    consumer, pkg_dir = local_dir, lib = install_lib,
    cran_deps = "install", repos = repo, upgrade = "always", verbose = FALSE
  )
  expect_length(installed$failed, 0L)
  expect_identical(
    as.character(packageVersion("externaldep074", lib.loc = install_lib)),
    "0.2.0"
  )

  operator_constraints <- c(
    operatorgt = "> 0.1.0",
    operatoreq = "== 0.2.0",
    operatorle = "<= 0.2.0",
    operatorlt = "< 0.3.0"
  )
  expect_true(all(vapply(names(operator_constraints), function(package) {
    constraint <- round074_archive(
      local_dir, package,
      imports = paste0("externaldep074 (", operator_constraints[[package]], ")")
    )
    result <- install_local_pkg(
      constraint, pkg_dir = local_dir, lib = install_lib,
      cran_deps = "error", upgrade = "always", verbose = FALSE
    )
    length(result$failed) == 0L
  }, logical(1L))))

  withr::local_options(repos = c(CRAN = "https://invalid.example.invalid"))
  no_repo_consumer <- round074_archive(
    local_dir, "norepo074", imports = "externaldep074 (>= 0.3.0)"
  )
  no_repo <- install_local_pkg(
    no_repo_consumer, pkg_dir = local_dir,
    lib = file.path(root, "no-repo-lib"),
    cran_deps = "install", upgrade = "always", verbose = FALSE
  )
  expect_match(
    no_repo$failed[[1L]], "configured repository", ignore.case = TRUE
  )
})

test_that("diamond installation has one final category per node", {
  skip_on_cran()
  root <- tempfile("bigbang-round074-diamond-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))
  round074_archive(root, "diamondbase074")
  round074_archive(root, "diamondleft074", imports = "diamondbase074")
  round074_archive(root, "diamondright074", imports = "diamondbase074")
  top <- round074_archive(
    root, "diamondtop074", imports = "diamondleft074, diamondright074"
  )
  lib <- file.path(root, "library")
  dir.create(lib)
  output <- capture.output(
    result <- install_local_pkg(top, pkg_dir = root, lib = lib,
                                upgrade = "always", verbose = TRUE)
  )
  expect_equal(sum(grepl("^\\* DONE", output)), 4L)
  expect_setequal(names(result$installed), c(
    "diamondbase074_0.1.0", "diamondleft074_0.1.0",
    "diamondright074_0.1.0", "diamondtop074_0.1.0"
  ))
  categories <- c(
    names(result$installed), names(result$unchanged), names(result$failed),
    names(result$skipped)
  )
  expect_length(categories, 4L)
  expect_length(unique(categories), 4L)
})
