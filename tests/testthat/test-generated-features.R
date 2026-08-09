generate_feature_metapackage <- function(name = "featureverse", workflow = NULL) {
  root <- tempfile("bigbang-generated-features-")
  destination <- file.path(root, "output")
  dir.create(destination, recursive = TRUE)
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  if (!nzchar(fixture)) {
    fixture <- testthat::test_path(
      "..", "..", "inst", "extdata", "toycomponent_0.1.0.tar.gz"
    )
  }
  create_metapackage(
    name = name,
    packages = "toycomponent_0.1.0",
    pkg_dir = dirname(fixture),
    dest_dir = destination,
    document = FALSE,
    verbose = FALSE,
    import_deps = character(),
    force_deps = character(),
    workflow = workflow
  )
}

test_that("generated startup supports cli formatting, fallback, and quiet mode", {
  result <- generate_feature_metapackage()
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), envir = runtime)
  sys.source(file.path(result$path, "R", "attach.R"), envir = runtime)
  sys.source(file.path(result$path, "R", "zzz.R"), envir = runtime)

  component <- list2env(list(shared_name = function() "component"))
  competitor <- list2env(list(shared_name = function() "competitor"))
  attach(
    competitor, name = "package:featurecompetitor", pos = 2L,
    warn.conflicts = FALSE
  )
  on.exit(detach("package:featurecompetitor"), add = TRUE)
  attach(
    component, name = "package:toycomponent", pos = 2L,
    warn.conflicts = FALSE
  )
  on.exit(detach("package:toycomponent"), add = TRUE)

  withr::local_options(featureverse.quiet = TRUE)
  quiet_output <- capture.output(
    runtime$.onAttach(NULL, "featureverse"), type = "message"
  )
  expect_length(quiet_output, 0L)

  options(featureverse.quiet = FALSE)
  cli_output <- capture.output(
    runtime$.onAttach(NULL, "featureverse"), type = "message"
  )
  expect_match(paste(cli_output, collapse = "\n"), "Attaching packages")
  expect_match(paste(cli_output, collapse = "\n"), "toycomponent")
  version_table <- runtime$format_cli_startup("bigbang", c("stats", "utils"))
  expect_match(version_table, as.character(utils::packageVersion("stats")), fixed = TRUE)
  expect_match(version_table, as.character(utils::packageVersion("utils")), fixed = TRUE)

  runtime$requireNamespace <- function(package, quietly = TRUE) {
    if (identical(package, "cli")) FALSE else base::requireNamespace(package, quietly)
  }
  fallback_output <- capture.output(
    runtime$.onAttach(NULL, "featureverse"), type = "message"
  )
  expect_match(paste(fallback_output, collapse = "\n"), "featureverse")
  expect_match(paste(fallback_output, collapse = "\n"), "={20}")
})

test_that("generated conflict reports include component masking", {
  result <- generate_feature_metapackage("conflictverse")
  runtime <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), envir = runtime)
  sys.source(file.path(result$path, "R", "attach.R"), envir = runtime)

  component <- list2env(list(shared_name = function() "component"))
  competitor <- list2env(list(shared_name = function() "competitor"))
  attach(
    competitor, name = "package:conflictcompetitor", pos = 2L,
    warn.conflicts = FALSE
  )
  on.exit(detach("package:conflictcompetitor"), add = TRUE)
  attach(
    component, name = "package:toycomponent", pos = 2L,
    warn.conflicts = FALSE
  )
  on.exit(detach("package:toycomponent"), add = TRUE)

  conflicts <- runtime$conflictverse_conflicts()
  expect_s3_class(conflicts, "conflictverse_conflicts")
  expect_named(conflicts, "shared_name")
  expect_setequal(
    conflicts$shared_name,
    c("package:toycomponent", "package:conflictcompetitor")
  )
  output <- capture.output(returned <- runtime$print.conflictverse_conflicts(conflicts))
  expect_identical(returned, conflicts)
  expect_match(paste(output, collapse = "\n"), "shared_name")
})

test_that("generated metadata and base test agree on component identity", {
  result <- generate_feature_metapackage("consistentverse")
  description <- read.dcf(file.path(result$path, "DESCRIPTION"))
  expect_identical(
    unname(description[1L, "Config/bigbang/packages"]), "toycomponent"
  )
  imports <- trimws(strsplit(description[1L, "Imports"], ",", fixed = TRUE)[[1L]])
  expect_false("toycomponent" %in% imports)

  consistency_test <- file.path(
    result$path, "tests", "component-consistency.R"
  )
  expect_true(file.exists(consistency_test))
  expect_match(
    paste(readLines(consistency_test, warn = FALSE), collapse = "\n"),
    "Config/bigbang/packages",
    fixed = TRUE
  )
  namespace <- readLines(file.path(result$path, "NAMESPACE"), warn = FALSE)
  expect_true("export(consistentverse_conflicts)" %in% namespace)
  expect_true("S3method(print,consistentverse_conflicts)" %in% namespace)

  readme <- paste(
    readLines(file.path(result$path, "README.md"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(readme, "consistentverse.quiet", fixed = TRUE)
  # The archives ship inside the meta-package here, so the documented call
  # takes no argument. The variant that does is covered in
  # test-self-contained-metapackage.R.
  expect_match(readme, "consistentverse_install()", fixed = TRUE)
  expect_no_match(readme, "pkg_dir =", fixed = TRUE)
})

test_that("workflow creates an ordered vignette and validates all components", {
  workflow <- c("Import data" = "toycomponent")
  result <- generate_feature_metapackage("workflowverse", workflow)
  expect_identical(result$workflow, workflow)
  path <- file.path(
    result$path, "vignettes", "workflow-workflowverse.Rmd"
  )
  expect_true(file.exists(path))
  content <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(content, "## Import data", fixed = TRUE)
  expect_match(content, "Pipeline component: `toycomponent`", fixed = TRUE)
  expect_match(content, "workflowverse_packages(), citation", fixed = TRUE)

  root <- tempfile("bigbang-invalid-workflow-")
  dir.create(root)
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  expect_error(
    create_metapackage(
      "badworkflowverse", "toycomponent_0.1.0", dirname(fixture),
      dest_dir = root, workflow = c("Wrong" = "otherpackage"),
      document = FALSE, verbose = FALSE
    ),
    class = "bigbang_error_workflow"
  )
  expect_false(dir.exists(file.path(root, "badworkflowverse")))
})

test_that("generated installer honors force and upgrade policies", {
  skip_on_cran()
  result <- generate_feature_metapackage("upgradeverse")
  runtime <- new.env(parent = baseenv())
  for (file in c("utils.R", "install_packages.R", "attach.R")) {
    sys.source(file.path(result$path, "R", file), envir = runtime)
  }
  library <- tempfile("bigbang-generated-upgrade-library-")
  dir.create(library)
  withr::local_libpaths(c(library, .libPaths()))
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )

  first <- runtime$upgradeverse_install(dirname(fixture), verbose = FALSE)
  expect_named(first$installed, "toycomponent_0.1.0")
  runtime$upgradeverse_detach()

  unchanged <- runtime$upgradeverse_install(dirname(fixture), verbose = FALSE)
  expect_named(unchanged$unchanged, "toycomponent_0.1.0")
  runtime$upgradeverse_detach()

  forced <- runtime$upgradeverse_install(
    dirname(fixture), force = TRUE, verbose = FALSE
  )
  expect_named(forced$installed, "toycomponent_0.1.0")
  runtime$upgradeverse_detach()

  expect_error(
    runtime$upgradeverse_install(
      dirname(fixture), force = TRUE, upgrade = "never", verbose = FALSE
    ),
    class = "bigbang_error_install_policy"
  )
  if ("package:toycomponent" %in% search()) runtime$upgradeverse_detach()
  if ("toycomponent" %in% loadedNamespaces()) unloadNamespace("toycomponent")
})

test_that("generated package installs and attaches without cli", {
  skip_on_cran()
  skip_if(
    identical(Sys.getenv("R_COVR"), "true"),
    "covr injects instrumented dependencies into child library paths"
  )
  result <- generate_feature_metapackage("fallbackverse")
  child_root <- dirname(dirname(result$path))
  library <- tempfile("bigbang-no-cli-library-")
  dir.create(library)
  writeLines(
    c(paste0("R_LIBS_USER=", library), "R_LIBS_SITE=", "R_LIBS="),
    file.path(child_root, ".Renviron")
  )
  fixture <- system.file(
    "extdata", "toycomponent_0.1.0.tar.gz", package = "bigbang"
  )
  code <- paste0(
    ".libPaths(c(", deparse(library), ", .Library)); ",
    "stopifnot(!requireNamespace('cli', quietly = TRUE)); ",
    "utils::install.packages(", deparse(fixture),
    ", repos = NULL, type = 'source', lib = ", deparse(library), "); ",
    "utils::install.packages(", deparse(result$path),
    ", repos = NULL, type = 'source', lib = ", deparse(library), "); ",
    "options(fallbackverse.quiet = FALSE); ",
    "library(fallbackverse, lib.loc = ", deparse(library), "); ",
    "stopifnot('package:toycomponent' %in% search()); ",
    "writeLines('NO_CLI_OK')"
  )
  output <- withr::with_dir(child_root, system2(
    file.path(R.home("bin"), "Rscript"),
    c("--no-site-file", "--no-init-file", "--no-restore", "-e", shQuote(code)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  expect_identical(status, 0L, info = paste(output, collapse = "\n"))
  expect_true("NO_CLI_OK" %in% output)
  expect_true(any(grepl("={20}", output)))
})
