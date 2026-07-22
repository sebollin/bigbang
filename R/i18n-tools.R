.bb_tr <- function(message) {
  gettext(message, domain = "R-bigbang")
}

.bb_trf <- function(format, ...) {
  gettextf(format, ..., domain = "R-bigbang")
}

.po_quote <- function(x) {
  escaped <- gsub("\\\\", "\\\\\\\\", enc2utf8(x))
  escaped <- gsub("\"", "\\\\\"", escaped, fixed = TRUE)
  escaped <- gsub("\n", "\\\\n", escaped, fixed = TRUE)
  paste0("\"", escaped, "\"")
}

.write_po_catalog <- function(messages, translations = NULL, path,
                              project = "bigbang") {
  if (is.null(translations)) translations <- stats::setNames(rep("", length(messages)), messages)
  translations <- translations[messages]
  header <- c(
    'msgid ""',
    'msgstr ""',
    paste0('"Project-Id-Version: ', project, '\\n"'),
    '"MIME-Version: 1.0\\n"',
    '"Content-Type: text/plain; charset=UTF-8\\n"',
    '"Content-Transfer-Encoding: 8bit\\n"',
    '"Language: es\\n"',
    '"Plural-Forms: nplurals=2; plural=(n != 1);\\n"',
    ""
  )
  entries <- unlist(Map(
    function(id, translation) {
      c(paste("msgid", .po_quote(id)), paste("msgstr", .po_quote(translation)), "")
    }, messages, unname(translations)
  ), use.names = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  .write_utf8(c(header, entries), path)
}

.write_mo_catalog <- function(messages, translations, path) {
  header <- paste0(
    "Project-Id-Version: bigbang\n",
    "MIME-Version: 1.0\n",
    "Content-Type: text/plain; charset=UTF-8\n",
    "Content-Transfer-Encoding: 8bit\n",
    "Language: es\n",
    "Plural-Forms: nplurals=2; plural=(n != 1);\n"
  )
  ids <- c("", enc2utf8(messages))
  values <- c(header, enc2utf8(unname(translations[messages])))
  ordering <- order(ids)
  ids <- ids[ordering]
  values <- values[ordering]
  id_raw <- lapply(ids, charToRaw)
  value_raw <- lapply(values, charToRaw)
  n <- length(ids)
  original_table <- 28L
  translation_table <- original_table + 8L * n
  original_strings <- translation_table + 8L * n
  original_offsets <- original_strings + c(0L, cumsum(lengths(id_raw) + 1L)[-n])
  translation_strings <- original_strings + sum(lengths(id_raw) + 1L)
  translation_offsets <- translation_strings + c(0L, cumsum(lengths(value_raw) + 1L)[-n])

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(as.raw(c(0xde, 0x12, 0x04, 0x95)), connection)
  writeBin(
    as.integer(c(0L, n, original_table, translation_table, 0L, 0L)),
    connection, size = 4L, endian = "little"
  )
  writeBin(as.integer(as.vector(rbind(lengths(id_raw), original_offsets))),
           connection, size = 4L, endian = "little")
  writeBin(as.integer(as.vector(rbind(lengths(value_raw), translation_offsets))),
           connection, size = 4L, endian = "little")
  for (value in id_raw) writeBin(c(value, as.raw(0)), connection)
  for (value in value_raw) writeBin(c(value, as.raw(0)), connection)
  invisible(path)
}

.drop_regular_comment_lines <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
  regular_comment <- grepl("^[[:space:]]*#(?!')", lines, perl = TRUE)
  paste(lines[!regular_comment], collapse = "\n")
}
