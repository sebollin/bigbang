# Empirical verification: generation, source installation, library(), and the
# explicit installer must preserve every cwd decoy.
#
# Run from the bigbang project root:
#   Rscript tests/integration/data-loss-verification.R
# Requires whisker, withr, and brio.
options(warn = 1)
gen_src <- if (dir.exists("R")) "R" else "../../R"
sandbox <- tempfile("verify-data-loss-fix-")
dir.create(sandbox)
cat("SANDBOX:", sandbox, "\n")

# Source the generator directly without installing bigbang.
for (f in c(
  "fs-utils.R", "i18n-tools.R", "translations.R", "dependencies.R",
  "scaffold.R", "templates-engine.R", "results.R", "create_metapackage.R",
  "install_local_pkg.R"
)) {
  sys.source(file.path(gen_src, f), envir = globalenv())
}

# --- 1. Build and archive dummy component packages ---
sources <- file.path(sandbox, "sources")
dir.create(sources)
archives <- file.path(sandbox, "archives")
dir.create(archives)

make_dummy_package <- function(name, source_dir, with_r = TRUE, imports = NULL) {
  path <- file.path(source_dir, name)
  dir.create(path)
  desc <- c(paste0("Package: ", name), "Type: Package",
            paste0("Title: Dummy ", name), "Version: 0.1.0",
            "Authors@R: person('T','D', email='t@e.com', role=c('aut','cre'))",
            "Description: Temporary component package.", "License: GPL (>= 3)", "Encoding: UTF-8",
            if (!is.null(imports)) paste0("Imports: ", imports))
  writeLines(desc, file.path(path, "DESCRIPTION"))
  writeLines(if (is.null(imports)) character() else paste0("import(", imports, ")"),
             file.path(path, "NAMESPACE"))
  if (with_r) {
    dir.create(file.path(path, "R"))
    writeLines(c("#' f", "#' @export", paste0("f_", name, " <- function() '", name, "'")),
               file.path(path, "R", "f.R"))
    writeLines(c(readLines(file.path(path, "NAMESPACE"), warn = FALSE),
                 paste0("export(f_", name, ")")), file.path(path, "NAMESPACE"))
  } else {
    dir.create(file.path(path, "data"))
    writeLines("component data", file.path(path, "data", "data.txt"))
  }
  path
}
make_dummy_package("bbb", sources, with_r = FALSE)
make_dummy_package("aaa", sources, imports = "bbb")
make_dummy_package("tmpcomponent", sources)
for (pkg in c("aaa", "bbb", "tmpcomponent")) {
  withr::with_dir(sources, utils::tar(
    file.path(archives, paste0(pkg, "_0.1.0.tar.gz")),
    files = pkg, compression = "gzip"
  ))
}

# --- 2. Prepare a user cwd containing decoy directories ---
user_cwd <- file.path(sandbox, "user_cwd")
dir.create(user_cwd)
decoys <- c("aaa", "bbb", "tmpcomponent", "aaa_extra", "tmp_analysis",
            "doc", "file2024", "Meta", "libs")
# aaa: same-name source project (DESCRIPTION + R/)
make_dummy_package("aaa", user_cwd)
# bbb: data-only package
dir.create(file.path(user_cwd, "bbb", "data"), recursive = TRUE)
writeLines(c("Package: bbb", "Version: 0.1.0"), file.path(user_cwd, "bbb", "DESCRIPTION"))
writeLines("valuable data", file.path(user_cwd, "bbb", "data", "important.txt"))
# tmpcomponent: a real component whose name starts with tmp
make_dummy_package("tmpcomponent", user_cwd)
# Other common work directories
for (d in c("aaa_extra", "tmp_analysis", "doc", "file2024", "Meta", "libs")) {
  dir.create(file.path(user_cwd, d))
  writeLines("valuable content", file.path(user_cwd, d, "important.txt"))
}
for (d in decoys) {
  writeLines(paste("valuable hash", d), file.path(user_cwd, d, "sentinel-hash.txt"))
}
sentinel_files <- file.path(user_cwd, decoys, "sentinel-hash.txt")
snapshot <- function() vapply(decoys, function(d) dir.exists(file.path(user_cwd, d)), logical(1))
snapshot_hash <- function() unname(tools::md5sum(sentinel_files))
before <- snapshot()
hash_before <- snapshot_hash()

# --- 3. Generate the metapackage while the decoy directory is the cwd ---
lib <- file.path(sandbox, "lib")
dir.create(lib)
.libPaths(c(lib, .libPaths()))
withr::with_dir(user_cwd, {
  create_metapackage(
    name = "testverse",
    packages = c("aaa_0.1.0", "bbb_0.1.0", "tmpcomponent_0.1.0"),
    pkg_dir = archives,
    dest_dir = file.path(sandbox, "project"),
    document = FALSE,
    verbose = FALSE
  )
})
after_generation <- snapshot()

# --- 4. Inspect generated code for destructive startup paths ---
proj <- file.path(sandbox, "project", "testverse")
emitted_files <- list.files(file.path(proj, "R"), full.names = TRUE)
cat("\n--- Generated R files:", paste(basename(emitted_files), collapse = ", "), "---\n")
cat("cleanup exists:", file.exists(file.path(proj, "cleanup")),
    "| cleanup.win exists:", file.exists(file.path(proj, "cleanup.win")), "\n")

code <- unlist(lapply(emitted_files, readLines, warn = FALSE))
zzz_code <- readLines(file.path(proj, "R", "zzz.R"), warn = FALSE)
startup_code <- sub("#.*$", "", zzz_code)
dangerous_patterns <- c("clean_pkg_dirs", "rm -rf", "rmdir", "list.files\\(tempdir",
                        "install.packages", "safe_unlink\\(pkg", "unlink\\(")
cat("\n--- Destructive pattern scan in startup hooks ---\n")
startup_clean <- TRUE
for (p in dangerous_patterns) {
  hits <- grep(p, startup_code, value = TRUE)
  cat(sprintf("  %-28s: %d occurrence(s)\n", p, length(hits)))
  startup_clean <- startup_clean && length(hits) == 0L
}

# --- 5. Install from source and load while the decoy directory is the cwd ---
withr::with_dir(user_cwd, {
  utils::install.packages(proj, repos = NULL, type = "source", lib = lib, quiet = TRUE)
})
after_metapackage_install <- snapshot()

sentinel <- file.path(tempdir(), "sentinel-v3.txt")
writeLines("preserve me", sentinel)
withr::with_dir(user_cwd, {
  suppressWarnings(suppressMessages(library("testverse", character.only = TRUE, lib.loc = lib)))
})
after_library <- snapshot()
sentinel_survives <- file.exists(sentinel)

# --- 6. Explicitly install and attach components ---
install_count <- 0L
trace("install_local_archive",
      tracer = quote(assign(
        "install_count", get("install_count", envir = .GlobalEnv) + 1L,
        envir = .GlobalEnv
      )),
      where = asNamespace("testverse"), print = FALSE)
install_result <- withr::with_dir(user_cwd, {
  getExportedValue("testverse", "testverse_install")(archives, ".tar.gz")
})
untrace("install_local_archive", where = asNamespace("testverse"))
after_explicit_install <- snapshot()
hash_after <- snapshot_hash()
components <- c("aaa", "bbb", "tmpcomponent")
components_installed <- vapply(components, requireNamespace, logical(1), quietly = TRUE)
components_attached <- paste0("package:", components) %in% search()

# --- 7. Report ---
table <- data.frame(
  decoy = decoys,
  before = before,
  after_generation = after_generation,
  after_metapackage_install = after_metapackage_install,
  after_library = after_library,
  after_explicit_install = after_explicit_install,
  row.names = NULL
)
cat("\n===== DECOY SURVIVAL (TRUE = intact) =====\n")
print(table)
cat("\nTemporary sentinel survives library():", sentinel_survives, "\n")
cat("Installer calls:", install_count, "\n")
cat("Order:", paste(install_result$order, collapse = " -> "), "\n")
cat("Components installed:", paste(components_installed, collapse = ", "), "\n")
cat("Components attached:", paste(components_attached, collapse = ", "), "\n")
cat("Decoy content and hashes intact:", identical(hash_before, hash_after), "\n")

all_ok <- all(after_explicit_install) && sentinel_survives &&
  identical(hash_before, hash_after) &&
  startup_clean &&
  !file.exists(file.path(proj, "cleanup")) &&
  !file.exists(file.path(proj, "cleanup.win")) &&
  install_count == length(components) &&
  all(components_installed) && all(components_attached) &&
  identical(install_result$order,
            c("bbb_0.1.0", "aaa_0.1.0", "tmpcomponent_0.1.0"))
cat("\n>>> RESULT:",
    if (all_ok) "OK - SAFE AND FUNCTIONAL" else "FAILURE - REGRESSION DETECTED",
    "<<<\n")
if (!all_ok) stop("Integration verification failed.", call. = FALSE)
