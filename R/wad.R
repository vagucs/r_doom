# DOOM generic portado do python_doom para R com SDL2.
#
# Por Wagner Nunes da Silva
#
# vagucs@bol.com.br
# vagucs@vagucs.com.br
# vagucs@gmail.com
#
# www.vagucs.com.br
#
# Leitor de WAD (nomes de 8 bytes, sem diferenciar maiusculas).

name8 <- function(raw) {
  if (!length(raw)) return("")
  z <- which(raw == as.raw(0L))
  if (length(z)) raw <- raw[seq_len(z[1] - 1L)]
  toupper(trimws(rawToChar(raw), whitespace = " "))
}

wad_new <- function() {
  e <- new.env(parent = emptyenv())
  e$lumps <- list()
  e$index <- new.env(parent = emptyenv())
  e
}

wad_add_file <- function(wad, path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  header <- readBin(con, "raw", 12L)
  ident <- rawToChar(header[1:4])
  if (!ident %in% c("IWAD", "PWAD")) stop("not a WAD: ", path)
  num <- readBin(header[5:8], "integer", size = 4L, endian = "little")
  ofs <- readBin(header[9:12], "integer", size = 4L, endian = "little")
  if (num < 0L) stop("WAD directory too large: ", path)
  seek(con, ofs)
  directory <- readBin(con, "raw", num * 16L)
  start <- length(wad$lumps)
  for (i in seq_len(num)) {
    off <- (i - 1L) * 16L
    pos <- readBin(directory[off + 1:4], "integer", size = 4L, endian = "little")
    size <- readBin(directory[off + 5:8], "integer", size = 4L, endian = "little")
    if (pos < 0L) pos <- as.numeric(as_u32(pos))
    if (size < 0L) size <- as.numeric(as_u32(size))
    name <- name8(directory[off + 9:16])
    wad$lumps[[start + i]] <- list(
      name = name, position = pos, size = size, cache = NULL, wad_path = path
    )
    wad$index[[name]] <- start + i
  }
  invisible(wad)
}

wad_check_num <- function(wad, name) {
  key <- toupper(trimws(substr(name, 1L, 8L), whitespace = " "))
  if (exists(key, envir = wad$index, inherits = FALSE)) {
    get(key, envir = wad$index, inherits = FALSE) - 1L
  } else {
    -1L
  }
}

wad_num_for_name <- function(wad, name) {
  n <- wad_check_num(wad, name)
  if (n < 0L) stop("lump not found: ", name)
  n
}

wad_cache_num <- function(wad, num) {
  lump <- wad$lumps[[num + 1L]]
  if (is.null(lump$cache)) {
    con <- file(lump$wad_path, "rb")
    on.exit(close(con), add = TRUE)
    seek(con, lump$position)
    lump$cache <- readBin(con, "raw", as.integer(lump$size))
    wad$lumps[[num + 1L]] <- lump
  }
  lump$cache
}

wad_cache_name <- function(wad, name) {
  wad_cache_num(wad, wad_num_for_name(wad, name))
}

wad_num_lumps <- function(wad) length(wad$lumps)

wad_lump_name <- function(wad, num) wad$lumps[[num + 1L]]$name
