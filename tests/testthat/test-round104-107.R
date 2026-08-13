round104_make_archive <- function(source_root, archive_dir, name,
                                  version = "1.0.0") {
  package_dir <- file.path(source_root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  writeLines(c(
    paste0("Package: ", name),
    paste0("Version: ", version),
    paste0("Title: Round 104 fixture ", name),
    "Description: Temporary component for update ownership tests.",
    "License: MIT",
    "Author: Test Author",
    "Maintainer: Test Author <test@example.org>"
  ), file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(value)", file.path(package_dir, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("value <- function() 1L",
             file.path(package_dir, "R", "value.R"), useBytes = TRUE)
  archive <- file.path(archive_dir, paste0(name, "_", version, ".tar.gz"))
  withr::with_dir(source_root, utils::tar(
    archive, name, compression = "gzip"
  ))
  archive
}

round104_generate <- function(name, packages, destination, ...) {
  create_metapackage(
    name, packages, dest_dir = destination,
    document = FALSE, verbose = FALSE, import_deps = character(),
    force_deps = character(), ...
  )
}

test_that("two updates never absorb or remove files owned by the user", {
  root <- tempfile("bigbang-update-owned-plan-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  component <- round104_make_archive(source_root, archives, "ownedplan")
  initial <- round104_generate("ownedverse", component, destination)

  notes <- file.path(initial$path, "NOTES.md")
  script <- file.path(initial$path, "scripts", "mine.R")
  git_head <- file.path(initial$path, ".git", "HEAD")
  git_config <- file.path(initial$path, ".git", "config")
  git_ref <- file.path(initial$path, ".git", "refs", "heads", "main")
  dir.create(dirname(script), recursive = TRUE)
  dir.create(dirname(git_head), recursive = TRUE)
  dir.create(dirname(git_ref), recursive = TRUE)
  writeLines("user notes", notes, useBytes = TRUE)
  writeLines("user_value <- 1L", script, useBytes = TRUE)
  writeLines("ref: refs/heads/main", git_head, useBytes = TRUE)
  writeLines("[core]", git_config, useBytes = TRUE)
  writeLines(strrep("0", 40L), git_ref, useBytes = TRUE)
  user_files <- c(notes, script, git_head, git_config, git_ref)
  before <- unname(tools::md5sum(user_files))

  first <- round104_generate(
    "ownedverse", component, destination, update = TRUE
  )
  second <- round104_generate(
    "ownedverse", component, destination, update = TRUE
  )

  expect_length(first$removed_files, 0L)
  expect_length(second$removed_files, 0L)
  expect_true(all(file.exists(user_files)))
  expect_identical(unname(tools::md5sum(user_files)), before)
  manifest <- readRDS(file.path(initial$path, .generation_manifest_name))
  expect_false(any(c(
    "NOTES.md", "scripts/mine.R", ".git/HEAD", ".git/config",
    ".git/refs/heads/main"
  ) %in% manifest$files))
})

test_that("legacy scanned manifests discard entries outside the generation plan", {
  root <- tempfile("bigbang-legacy-scanned-manifest-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  initial <- round104_generate("legacyplanverse", archive, destination)
  user_file <- file.path(initial$path, ".git", "HEAD")
  dir.create(dirname(user_file), recursive = TRUE)
  writeLines("ref: refs/heads/main", user_file, useBytes = TRUE)
  current <- readRDS(file.path(initial$path, .generation_manifest_name))
  legacy <- .manifest_records(
    initial$path, c(current$files, ".git/HEAD")
  )
  legacy$schema <- 1L
  .atomic_save_rds(legacy, file.path(initial$path, .generation_manifest_name))

  updated <- round104_generate(
    "legacyplanverse", archive, destination, update = TRUE
  )

  expect_true(file.exists(user_file))
  expect_length(updated$removed_files, 0L)
  migrated <- readRDS(file.path(initial$path, .generation_manifest_name))
  expect_identical(migrated$schema, 2L)
  expect_false(".git/HEAD" %in% migrated$files)
})

test_that("the generation manifest enumerates every generated file", {
  skip_if_not_installed("devtools")
  root <- tempfile("bigbang-manifest-plan-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  for (reexport in c(FALSE, TRUE)) {
    name <- if (reexport) "planfilesreexport" else "planfilesattach"
    generated <- create_metapackage(
      name,
      system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"),
      dest_dir = destination, document = TRUE, verbose = FALSE,
      import_deps = character(), force_deps = character(),
      reexport = reexport
    )
    manifest <- readRDS(file.path(generated$path, .generation_manifest_name))
    actual <- list.files(
      generated$path, recursive = TRUE, all.files = TRUE,
      no.. = TRUE, include.dirs = FALSE
    )
    expect_setequal(actual, c(manifest$files, .generation_manifest_name))
  }
})

test_that("manifest recording rejects a planned file that was not written", {
  project <- tempfile("bigbang-missing-planned-file-")
  dir.create(project)
  expect_error(
    .manifest_records(project, "R/missing.R"),
    "Generated files were not written as planned: R/missing.R",
    fixed = TRUE
  )
})

test_that("update manifests reject paths outside the generated project", {
  project <- tempfile("bigbang-invalid-manifest-path-")
  dir.create(project)
  manifest <- list(
    schema = 2L,
    files = "../outside.txt",
    hashes = stats::setNames("not-used", "../outside.txt")
  )
  saveRDS(manifest, file.path(project, .generation_manifest_name))
  expect_error(
    .validate_update_manifest(project),
    class = "bigbang_error_modified_generated_file"
  )
})

test_that("omitted archive preservation fails toward retaining data", {
  no_omissions <- data.frame(
    component = character(), input = character(), reason = character(),
    stringsAsFactors = FALSE
  )
  omission <- data.frame(
    component = "unknown", input = "unknown", reason = "unreadable",
    stringsAsFactors = FALSE
  )
  expect_length(
    .preserve_omitted_archives("DESCRIPTION", omission), 0L
  )
  expect_length(
    .preserve_omitted_archives("inst/archives/known_1.0.tar.gz", no_omissions),
    0L
  )
  expect_identical(
    .preserve_omitted_archives("inst/archives/unknown", omission),
    "inst/archives/unknown"
  )
})

test_that("an omitted update input cannot delete its shipped archive", {
  root <- tempfile("bigbang-update-omitted-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  keep <- round104_make_archive(source_root, archives, "keptcomp")
  victim <- round104_make_archive(source_root, archives, "victim")
  initial <- round104_generate(
    "omitverse", c(keep, victim), destination
  )
  shipped_victim <- file.path(
    initial$path, "inst", "archives", basename(victim)
  )
  unlink(victim)

  updated <- suppressWarnings(round104_generate(
    "omitverse", c(keep, "victi_1.0.0"), destination,
    update = TRUE, on_component_error = "skip"
  ))

  expect_true(isTRUE(updated$updated))
  expect_true(file.exists(shipped_victim))
  expect_false(file.path("inst", "archives", basename(victim)) %in%
                 updated$removed_files)
})

test_that("identified omitted archives are preserved while deliberate removals proceed", {
  root <- tempfile("bigbang-update-omitted-mixed-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  keep <- round104_make_archive(source_root, archives, "mixedkeep")
  drop <- round104_make_archive(source_root, archives, "mixeddrop")
  victim <- round104_make_archive(source_root, archives, "mixedvictim")
  initial <- round104_generate(
    "mixedverse", c(keep, drop, victim), destination
  )
  shipped_drop <- file.path(initial$path, "inst", "archives", basename(drop))
  shipped_victim <- file.path(
    initial$path, "inst", "archives", basename(victim)
  )
  missing_victim <- file.path(
    archives, "misspelled-directory", basename(victim)
  )
  unlink(c(drop, victim))

  updated <- suppressWarnings(round104_generate(
    "mixedverse", c(keep, missing_victim), destination,
    update = TRUE, on_component_error = "skip"
  ))

  expect_false(file.exists(shipped_drop))
  expect_true(file.exists(shipped_victim))
  expect_true(file.path("inst", "archives", basename(drop)) %in%
                updated$removed_files)
  expect_false(file.path("inst", "archives", basename(victim)) %in%
                 updated$removed_files)
})

test_that("update rejects a symbolic project root before writing", {
  skip_on_os("windows")
  root <- tempfile("bigbang-update-root-link-")
  source_root <- file.path(root, "sources")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  actual <- file.path(root, "actual-project")
  dir.create(source_root, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  component <- round104_make_archive(source_root, archives, "rootlink")
  initial <- round104_generate("rootverse", component, destination)
  expect_true(file.rename(initial$path, actual))
  expect_true(file.symlink(actual, initial$path))
  description <- file.path(actual, "DESCRIPTION")
  before <- unname(tools::md5sum(description))

  expect_error(
    round104_generate(
      "rootverse", component, destination, update = TRUE, version = "9.9.9"
    ),
    class = "bigbang_error_symlink_generated_path"
  )
  expect_identical(unname(tools::md5sum(description)), before)
})

test_that("installer transcripts follow verbose and failures retain ERROR", {
  run_checks <- function(install_function, environment) {
    environment$system2 <- function(command, args, stdout, stderr, ...) {
      writeLines(c("* installing fixture", "* DONE (fixture)"), stdout)
      0L
    }
    expect_output(
      install_function("fixture.tar.gz", tempdir(), verbose = FALSE),
      NA
    )
    expect_output(
      install_function("fixture.tar.gz", tempdir(), verbose = TRUE),
      "installing fixture",
      fixed = TRUE
    )
    environment$system2 <- function(command, args, stdout, stderr, ...) {
      writeLines("ERROR: deterministic install failure", stdout)
      1L
    }
    expect_error(
      install_function("fixture.tar.gz", tempdir(), verbose = FALSE),
      "ERROR: deterministic install failure",
      fixed = TRUE
    )
    environment$system2 <- function(command, args, stdout, stderr, ...) {
      unlink(stdout, force = TRUE)
      7L
    }
    expect_error(
      install_function("fixture.tar.gz", tempdir(), verbose = FALSE),
      "R CMD INSTALL exited with status 7",
      fixed = TRUE
    )
  }

  direct_environment <- new.env(parent = environment(.install_source_component))
  direct_install <- .install_source_component
  environment(direct_install) <- direct_environment
  run_checks(direct_install, direct_environment)

  root <- tempfile("bigbang-generated-installer-verbose-")
  destination <- file.path(root, "destination")
  dir.create(destination, recursive = TRUE)
  generated <- round104_generate(
    "verboseverse",
    system.file("extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"),
    destination
  )
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(generated$path, "R", "install_packages.R"), runtime)
  run_checks(runtime$install_source_component, runtime)
})

test_that("public and generated installers pass verbose to source installs", {
  skip_on_cran()
  root <- tempfile("bigbang-install-verbose-integration-")
  destination <- file.path(root, "destination")
  direct_lib <- file.path(root, "direct-lib")
  generated_lib <- file.path(root, "generated-lib")
  dir.create(destination, recursive = TRUE)
  dir.create(direct_lib)
  dir.create(generated_lib)
  archive <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )

  expect_output(
    direct <- install_local_pkg(
      archive, verbose = FALSE, upgrade = "always", lib = direct_lib
    ),
    NA
  )
  expect_length(direct$failed, 0L)

  generated <- round104_generate("quietverse", archive, destination)
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(generated$path, "R", "utils.R"), runtime)
  sys.source(file.path(generated$path, "R", "install_packages.R"), runtime)
  expect_output(
    emitted <- runtime$install_packages_in_order(
      runtime$.component_names,
      file.path(generated$path, "inst", "archives"),
      verbose = FALSE, cran_deps = "skip", upgrade = "always",
      lib = generated_lib
    ),
    NA
  )
  expect_length(emitted$failed, 0L)
})
