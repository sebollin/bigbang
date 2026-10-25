.r076_make_archive <- function(name, version, archive_path, root,
                               imports = NULL) {
  package_root <- file.path(root, paste0(name, "-r076"))
  dir.create(file.path(package_root, "R"), recursive = TRUE,
             showWarnings = FALSE)
  fields <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: R076 fixture ", name),
    paste0("Version: ", version),
    "Authors@R: person('Test', 'Author', email = 'test@example.org', role = c('aut', 'cre'))",
    paste0("Description: Temporary R076 fixture for ", name, "."),
    "License: MIT"
  )
  if (!is.null(imports)) fields <- c(fields, paste0("Imports: ", imports))
  writeLines(fields, file.path(package_root, "DESCRIPTION"), useBytes = TRUE)
  writeLines("export(r076_value)", file.path(package_root, "NAMESPACE"),
             useBytes = TRUE)
  writeLines("r076_value <- function() 76L",
             file.path(package_root, "R", "value.R"), useBytes = TRUE)
  withr::with_dir(root, utils::tar(
    archive_path, basename(package_root), compression = "gzip"
  ))
  archive_path
}

.r076_toy_archive <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      source <- system.file(
        "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
      )
      if (!nzchar(source)) {
        source <- testthat::test_path(
          "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
        )
      }
      cached_path <- file.path(
        tempfile("bigbang-r076-toy-"), "toycomponent_0.1.0.tar.gz"
      )
      dir.create(dirname(cached_path), recursive = TRUE)
      file.copy(source, cached_path)
      cached <<- cached_path
    }
    cached
  }
})

.r076_source_runtime <- function(project) {
  runtime <- new.env(parent = baseenv())
  for (file in list.files(file.path(project, "R"), pattern = "[.]R$",
                          full.names = TRUE)) {
    sys.source(file, runtime)
  }
  runtime
}

test_that("the recipient plan is read-only and agrees with installation", {
  skip_on_cran()
  root <- tempfile("bigbang-r076-plan-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  library_dir <- file.path(root, "library")
  source_root <- file.path(root, "sources")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(library_dir)
  dir.create(source_root)
  archive <- .r076_make_archive(
    "planpkg", "1.0.0", file.path(archives, "planpkg_1.0.0.tar.gz"),
    source_root, imports = "r076_missing (>= 99.0.0)"
  )
  result <- round075_create_metapackage(
    "planverse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  runtime <- .r076_source_runtime(result$path)
  before_files <- list.files(library_dir, all.files = TRUE, recursive = TRUE)
  before_namespaces <- loadedNamespaces()
  plan <- runtime$planverse_deps(
    pkg_dir = archives, plan = TRUE, cran_deps = "skip", lib = library_dir
  )
  after_files <- list.files(library_dir, all.files = TRUE, recursive = TRUE)
  expect_s3_class(plan, "bigbang_dependency_plan")
  expect_identical(plan$components$action, "skip")
  expect_identical(plan$external$action, "skip")
  expect_identical(before_files, after_files)
  expect_false("planpkg" %in% loadedNamespaces())
  expect_setequal(before_namespaces[before_namespaces %in% loadedNamespaces()],
                  before_namespaces)

  captured <- capture_warnings(runtime$planverse_install(
    pkg_dir = archives, cran_deps = "skip", verbose = FALSE,
    lib = library_dir
  ))
  installed <- captured$value
  expect_warning_set(captured$warnings, c(
    "Skipped planpkg_1.0.0: Skipped because non-local dependencies are missing",
    "Some components were skipped because non-local dependencies are missing"
  ))
  expect_true("planpkg_1.0.0" %in% names(installed$skipped))
  expect_identical(plan$components$action, "skip")
})

test_that("plan and installer defaults follow the generated upgrade policy", {
  skip_on_cran()
  root <- tempfile("bigbang-r076-upgrade-default-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  source_root <- file.path(root, "sources")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(source_root)
  archive_old <- .r076_make_archive(
    "upgradepkg", "1.0.0", file.path(archives, "upgradepkg_1.0.0.tar.gz"),
    source_root
  )
  archive_new <- .r076_make_archive(
    "upgradepkg", "2.0.0", file.path(archives, "upgradepkg_2.0.0.tar.gz"),
    source_root
  )
  for (policy in c("always", "never")) {
    name <- paste0("upgrade", policy, "verse")
    generated <- round075_create_metapackage(
      name, archive_new, dest_dir = destination, document = FALSE,
      verbose = FALSE, import_deps = character(), force_deps = character(),
      install_upgrade = policy
    )
    runtime <- .r076_source_runtime(generated$path)
    generated_text <- paste(
      unlist(lapply(list.files(file.path(generated$path, "R"),
                               pattern = "[.]R$", full.names = TRUE),
                    readLines, warn = FALSE), use.names = FALSE),
      collapse = "\n"
    )
    expect_match(generated_text, paste0('upgrade = "', policy, '"'),
                 fixed = TRUE)
    lib <- file.path(root, paste0("lib-", policy))
    dir.create(lib)
    # The same-version case distinguishes always from newer, while the older
    # installed version distinguishes never from newer.
    seed <- if (identical(policy, "always")) archive_new else archive_old
    install_local_pkg(seed, lib = lib, upgrade = "always", verbose = FALSE)
    plan <- runtime[[paste0(name, "_deps")]](
      pkg_dir = archives, plan = TRUE, cran_deps = "skip", lib = lib
    )
    expect_identical(plan$upgrade, policy)
    expected_action <- if (identical(policy, "never")) "keep" else "install"
    expect_identical(plan$components$action, expected_action)
    result <- expect_no_warning(runtime[[paste0(name, "_install")]](
      pkg_dir = archives, cran_deps = "skip", verbose = FALSE, lib = lib
    ))
    result_field <- if (identical(expected_action, "keep")) {
      "unchanged"
    } else {
      "installed"
    }
    expect_true("upgradepkg_2.0.0" %in% names(result[[result_field]]))
  }
})

test_that("matching archive identity is silent in both installers", {
  skip_on_cran()
  root <- tempfile("bigbang-r076-identity-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  source_root <- file.path(root, "sources")
  direct_lib <- file.path(root, "direct-lib")
  generated_lib <- file.path(root, "generated-lib")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(source_root)
  dir.create(direct_lib)
  dir.create(generated_lib)
  archive <- .r076_make_archive(
    "identitypkg", "1.0.0",
    file.path(archives, "identitypkg_1.0.0.tar.gz"), source_root
  )
  expect_no_warning(install_local_pkg(
    archive, lib = direct_lib, upgrade = "always", verbose = FALSE
  ))
  generated <- round075_create_metapackage(
    "identityverse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  runtime <- .r076_source_runtime(generated$path)
  expect_no_warning(runtime$install_packages_in_order(
    generated$archives, archives, NULL, verbose = FALSE,
    upgrade = "always", lib = generated_lib
  ))
  expect_match(
    bigbang:::.installation_error_excerpt(
      c("Error: fallo real", strrep("x", 5000L))
    ),
    "Error: fallo real", fixed = TRUE
  )
  expect_match(
    runtime$installation_error_excerpt(
      c("Error: fallo real", strrep("x", 5000L))
    ),
    "Error: fallo real", fixed = TRUE
  )
})

test_that("the plan reports repository availability like its external action", {
  skip_on_cran()
  root <- tempfile("bigbang-r076-repositories-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  source_root <- file.path(root, "sources")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(source_root)
  archive <- .r076_make_archive(
    "repositorypkg", "1.0.0",
    file.path(archives, "repositorypkg_1.0.0.tar.gz"), source_root,
    imports = "r076_missing (>= 99.0.0)"
  )
  generated <- round075_create_metapackage(
    "repositoryverse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  runtime <- .r076_source_runtime(generated$path)
  repos <- list(cran = "@CRAN@", blank = "", usable = "https://x.invalid")
  for (label in names(repos)) {
    plan <- runtime$repositoryverse_deps(
      pkg_dir = archives, plan = TRUE, cran_deps = "install",
      repos = repos[[label]], lib = file.path(root, paste0("lib-", label))
    )
    configured <- identical(label, "usable")
    expect_identical(plan$repositories_configured, configured)
    expect_identical(
      plan$external$action,
      if (configured) "install_unverified" else "fail"
    )
  }
})

test_that("generated installation sees a component in a secondary library", {
  skip_on_cran()
  root <- tempfile("bigbang-r076-secondary-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  secondary <- file.path(root, "secondary")
  target <- file.path(root, "target")
  source_root <- file.path(root, "sources")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  dir.create(secondary)
  dir.create(target)
  dir.create(source_root)
  archive <- .r076_make_archive(
    "secondarypkg", "1.0.0", file.path(archives, "secondarypkg_1.0.0.tar.gz"),
    source_root
  )
  direct <- install_local_pkg(archive, lib = secondary, upgrade = "always",
                              verbose = FALSE)
  expect_length(direct$failed, 0L)
  withr::local_libpaths(c(secondary, .libPaths()))
  result <- round075_create_metapackage(
    "secondaryverse", archive, dest_dir = destination, document = FALSE,
    verbose = FALSE, import_deps = character(), force_deps = character()
  )
  runtime <- .r076_source_runtime(result$path)
  installed <- expect_no_warning(runtime$secondaryverse_install(
    pkg_dir = archives, upgrade = "never", verbose = FALSE, lib = target
  ))
  expect_true("secondarypkg_1.0.0" %in% names(installed$installed))
  expect_true(dir.exists(file.path(target, "secondarypkg")))
})

test_that("governance is opt-in and update tracks its files", {
  skip_on_cran()
  withr::local_envvar(c(LC_ALL = "", LANG = "C", LANGUAGE = "en"))
  withr::local_language("en")
  root <- tempfile("bigbang-r076-governance-")
  archives <- file.path(root, "archives")
  destination <- file.path(root, "destination")
  dir.create(archives, recursive = TRUE)
  dir.create(destination)
  archive <- file.copy(.r076_toy_archive(), archives)
  plain <- round075_create_metapackage(
    "plainverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = FALSE
  )
  plain_files <- sort(list.files(plain$path, all.files = TRUE, recursive = TRUE,
                                 include.dirs = FALSE))
  expect_false(any(grepl("CONTRIBUTING|CODE_OF_CONDUCT|package-inclusion", plain_files)))

  governed <- round075_create_metapackage(
    "governedverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = TRUE
  )
  expect_true(file.exists(file.path(governed$path, "CONTRIBUTING.md")))
  expect_true(file.exists(file.path(governed$path, "CODE_OF_CONDUCT.md")))
  expect_true(file.exists(file.path(
    governed$path, ".github", "ISSUE_TEMPLATE", "package-inclusion-request.md"
  )))
  expect_true(any(grepl("CONTRIBUTING", readLines(
    file.path(governed$path, ".Rbuildignore")
  ), fixed = TRUE)))
  expect_true(any(grepl("Contributor Covenant", readLines(
    file.path(governed$path, "CODE_OF_CONDUCT.md")
  ), fixed = TRUE)))
  update <- round075_create_metapackage(
    "governedverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = TRUE,
    update = TRUE
  )
  manifest <- readRDS(file.path(governed$path, ".bigbang-manifest.rds"))
  expect_true("CONTRIBUTING.md" %in% manifest$files)
  expect_true(file.exists(file.path(governed$path, "CONTRIBUTING.md")))

  toggled <- round075_create_metapackage(
    "toggledverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = TRUE
  )
  round075_create_metapackage(
    "toggledverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = FALSE,
    update = TRUE
  )
  expect_false(dir.exists(file.path(toggled$path, ".github", "ISSUE_TEMPLATE")))
  expect_false(dir.exists(file.path(toggled$path, ".github")))

  user_dirs <- round075_create_metapackage(
    "userdirsverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = FALSE
  )
  dir.create(file.path(user_dirs$path, ".github", "ISSUE_TEMPLATE"), recursive = TRUE)
  round075_create_metapackage(
    "userdirsverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = TRUE,
    update = TRUE
  )
  round075_create_metapackage(
    "userdirsverse", "toycomponent_0.1.0", pkg_dir = archives,
    dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character(), governance = FALSE,
    update = TRUE
  )
  expect_true(dir.exists(file.path(user_dirs$path, ".github", "ISSUE_TEMPLATE")))

  governance_root <- file.path(root, "governance-probes")
  dir.create(governance_root)
  expect_no_error(bigbang:::.prune_governance_dirs(
    governance_root, c("missing", "also-missing")
  ))
  nonempty <- file.path(governance_root, "nonempty")
  dir.create(nonempty)
  writeLines("user-owned", file.path(nonempty, "keep.txt"))
  expect_no_error(bigbang:::.prune_governance_dirs(governance_root, "nonempty"))
  expect_true(file.exists(file.path(nonempty, "keep.txt")))
  if (!nzchar(Sys.readlink(governance_root))) {
    symlink_target <- file.path(governance_root, "symlink-target")
    symlink_path <- file.path(governance_root, "symlink-dir")
    dir.create(symlink_target)
    can_make_link <- !identical(.Platform$OS.type, "windows") ||
      exists("Sys.junction", mode = "function")
    made_link <- can_make_link && bb_dir_link(symlink_target, symlink_path)
    cleanup_link <- function() {
      if (file.exists(symlink_path) || dir.exists(symlink_path) ||
            bigbang:::.path_is_symlink(symlink_path)) {
        suppressWarnings(unlink(symlink_path, recursive = FALSE, force = TRUE))
      }
      invisible(NULL)
    }
    if (made_link) {
      withr::defer(cleanup_link())
      expect_error(
        bigbang:::.prune_governance_dirs(governance_root, "symlink-dir"),
        class = "bigbang_error_symlink_generated_path"
      )
      cleanup_link()
      expect_false(file.exists(symlink_path) || dir.exists(symlink_path) ||
                     bigbang:::.path_is_symlink(symlink_path))
    }
  }
  expect_error(
    round075_create_metapackage(
      "invalidgovernanceverse", "toycomponent_0.1.0", pkg_dir = archives,
      dest_dir = destination, document = FALSE, verbose = FALSE,
      import_deps = character(), force_deps = character(), governance = "yes"
    ),
    regexp = "'governance' must be TRUE or FALSE"
  )
})
