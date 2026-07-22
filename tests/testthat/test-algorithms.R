make_algorithm_component <- function(name, root, imports = NULL) {
  package_dir <- file.path(root, name)
  dir.create(file.path(package_dir, "R"), recursive = TRUE)
  description <- c(
    paste0("Package: ", name),
    "Type: Package",
    paste0("Title: Algorithm Fixture ", name),
    "Version: 0.1.0",
    "Authors@R: person('Test', 'Author', email='test@example.org', role=c('aut','cre'))",
    "Description: A temporary component for pure graph tests.",
    "License: MIT",
    if (!is.null(imports)) paste0("Imports: ", imports)
  )
  writeLines(description, file.path(package_dir, "DESCRIPTION"), useBytes = TRUE)
  writeLines(character(), file.path(package_dir, "NAMESPACE"), useBytes = TRUE)
  writeLines("fixture_value <- 1L", file.path(package_dir, "R", "value.R"), useBytes = TRUE)
  package_dir
}

algorithm_fixture <- function() {
  sandbox <- tempfile("bigbang-algorithms-")
  sources <- file.path(sandbox, "sources")
  archives <- file.path(sandbox, "archives")
  destination <- file.path(sandbox, "destination")
  dir.create(sources, recursive = TRUE)
  dir.create(archives)
  dir.create(destination)
  make_algorithm_component("grapha", sources, "graphb")
  make_algorithm_component("graphb", sources, "graphc")
  make_algorithm_component("graphc", sources)
  for (name in c("grapha", "graphb", "graphc")) {
    withr::with_dir(sources, utils::tar(
      file.path(archives, paste0(name, "_0.1.0.tar.gz")),
      files = name, compression = "gzip"
    ))
  }
  result <- create_metapackage(
    "graphverse", paste0(c("grapha", "graphb", "graphc"), "_0.1.0"),
    archives, dest_dir = destination, document = FALSE, verbose = FALSE,
    import_deps = character(), force_deps = character()
  )
  environment <- new.env(parent = baseenv())
  sys.source(file.path(result$path, "R", "utils.R"), envir = environment)
  sys.source(file.path(result$path, "R", "install_packages.R"), envir = environment)
  list(root = sandbox, archives = archives, result = result, env = environment)
}

test_that("dependency classification recognizes versioned local archives", {
  archives <- tempfile("bigbang-classification-")
  dir.create(archives)
  file.create(file.path(archives, c("mi.pkg_0.1.0.tar.gz", "localb_2.0.0.tar.gz")))
  result <- clasificar_dependencias(
    c("mi.pkg", "localb", "stats", "mi.pkg_0.1.0"), archives, ".tar.gz"
  )
  expect_setequal(result$locales, c("mi.pkg", "localb", "mi.pkg_0.1.0"))
  expect_identical(result$cran, "stats")
})

test_that("generated graph and topological sort put dependencies first", {
  fixture <- algorithm_fixture()
  packages <- paste0(c("grapha", "graphb", "graphc"), "_0.1.0")
  graph <- fixture$env$crear_grafo_dependencias(
    packages, fixture$archives, ".tar.gz"
  )
  expect_identical(unname(graph[1L, ]), c(0, 1, 0))
  expect_identical(unname(graph[2L, ]), c(0, 0, 1))
  order <- fixture$env$ordenamiento_topologico(graph)
  expect_identical(packages[order], rev(packages))
})

test_that("generated cycle detection reports a cycle", {
  fixture <- algorithm_fixture()
  cyclic <- matrix(c(0, 1, 1, 0), nrow = 2L, byrow = TRUE)
  cycles <- fixture$env$detect_cycles(cyclic)
  expect_length(cycles, 1L)
  expect_setequal(cycles[[1L]], c(1L, 2L))
})

test_that("generated LICENSE derives its holder and current year from Authors@R", {
  fixture <- algorithm_fixture()
  license <- readLines(file.path(fixture$result$path, "LICENSE"), warn = FALSE)
  expect_identical(license[[1L]], paste0("YEAR: ", format(Sys.Date(), "%Y")))
  expect_identical(license[[2L]], "COPYRIGHT HOLDER: First Last")
  expect_identical(
    .copyright_holders(paste0(
      "c(person(given='A', family='One'), ",
      "person('B', 'Two'))"
    )),
    "A One, B Two"
  )
})

test_that("verbosity is quiet in non-interactive workflows when disabled", {
  archive_dir <- tempfile("bigbang-quiet-install-")
  dir.create(archive_dir)
  expect_silent(
    result <- install_local_pkg(
      "missing_0.1.0", archive_dir, verbose = FALSE
    )
  )
  expect_named(result$failed, "missing_0.1.0")
  expect_message(
    install_local_pkg("missing_0.1.0", archive_dir, verbose = TRUE),
    "Packages that failed"
  )
})
