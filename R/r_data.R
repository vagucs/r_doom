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
# Texturas, flats e COLORMAP (r_data). O patch da abertura e a mesma
# coluna de posts: topo, tamanho, pixels, fim em 255.

tex_name_at <- function(buf, off) name8(buf[off + seq_len(8L)])

resources_new <- function(wad) {
  e <- new.env(parent = emptyenv())
  e$wad <- wad
  e$textures <- list()
  e$tex_index <- new.env(parent = emptyenv())
  e$patchlookup <- integer()
  e$patch_cache <- new.env(parent = emptyenv())
  e$sample <- new.env(parent = emptyenv())
  e$flats_first <- 0L
  e$flats_last <- 0L
  e$flattranslation <- integer()
  e$texturetranslation <- integer()
  e$colormaps <- raw()
  e$skytexture <- 0L
  e$skyflatnum <- 0L
  e
}

resources_init <- function(res) {
  init_textures(res)
  init_flats(res)
  res$colormaps <- wad_cache_name(res$wad, "COLORMAP")
  res$skyflatnum <- flat_num_for_name(res, "F_SKY1")
  resources_set_sky(res, 1L, 1L)
  res$sprites <- init_sprite_defs(res$wad)
  invisible(res)
}

resources_set_sky <- function(res, episode, mapn) {
  commercial <- wad_check_num(res$wad, "MAP01") >= 0L
  name <- if (commercial) {
    if (mapn < 12L) "SKY1" else if (mapn < 21L) "SKY2" else "SKY3"
  } else {
    c("SKY1", "SKY2", "SKY3", "SKY4")[min(max(as.integer(episode), 1L), 4L)]
  }
  res$skytexture <- texture_num_for_name(res, name)
  invisible(res)
}

colormap_bytes <- function(res, level) {
  level <- as.integer(level)
  if (is.na(level) || level < 0L) level <- 0L
  if (level > 32L) level <- 32L
  off <- level * 256L
  if (length(res$colormaps) < off + 256L) return(raw(256L))
  res$colormaps[off + seq_len(256L)]
}

init_flats <- function(res) {
  res$flats_first <- wad_num_for_name(res$wad, "F_START") + 1L
  res$flats_last <- wad_num_for_name(res$wad, "F_END") - 1L
  n <- res$flats_last - res$flats_first + 1L
  if (n < 0L) n <- 0L
  res$flattranslation <- seq_len(n) - 1L
  invisible(res)
}

flat_count <- function(res) length(res$flattranslation)

flat_num_for_name <- function(res, name) {
  i <- wad_check_num(res$wad, name)
  if (i < 0L) return(0L)
  i - res$flats_first
}

flat_lump <- function(res, flatnum) {
  n <- flat_count(res)
  if (!n) return(res$flats_first)
  if (is.na(flatnum) || flatnum < 0L || flatnum >= n) flatnum <- 0L
  res$flats_first + res$flattranslation[flatnum + 1L]
}

flat_pixels <- function(res, flatnum) {
  data <- wad_cache_num(res$wad, flat_lump(res, flatnum))
  if (length(data) >= 4096L) return(data[seq_len(4096L)])
  c(data, raw(4096L - length(data)))
}

cache_patch <- function(res, lump) {
  if (lump < 0L) return(raw())
  key <- as.character(lump)
  hit <- res$patch_cache[[key]]
  if (!is.null(hit)) return(hit)
  data <- wad_cache_num(res$wad, lump)
  res$patch_cache[[key]] <- data
  data
}

init_textures <- function(res) {
  pnames <- wad_cache_name(res$wad, "PNAMES")
  count <- as.integer(as_i32(u32_at(pnames, 0)))
  if (is.na(count) || count < 0L) count <- 0L
  lookup <- integer(count)
  for (i in seq_len(count)) {
    lookup[i] <- wad_check_num(res$wad, tex_name_at(pnames, 4L + (i - 1L) * 8L))
  }
  res$patchlookup <- lookup
  maptex <- list(wad_cache_name(res$wad, "TEXTURE1"))
  if (wad_check_num(res$wad, "TEXTURE2") >= 0L) {
    maptex[[2]] <- wad_cache_name(res$wad, "TEXTURE2")
  }
  totals <- vapply(maptex, function(buf) as.integer(as_i32(u32_at(buf, 0))), integer(1))
  totals[is.na(totals) | totals < 0L] <- 0L
  made <- 0L
  for (part in seq_along(maptex)) {
    src <- maptex[[part]]
    ntex <- totals[part]
    for (i in seq_len(ntex)) {
      offset <- as.integer(as_i32(u32_at(src, 4L + (i - 1L) * 4L)))
      if (is.na(offset) || offset < 0L) next
      tex <- new.env(parent = emptyenv())
      tex$name <- tex_name_at(src, offset)
      tex$width <- i16_at(src, offset + 12L)
      tex$height <- i16_at(src, offset + 14L)
      patchcount <- i16_at(src, offset + 20L)
      tex$patches <- vector("list", max(patchcount, 0L))
      poff <- offset + 22L
      for (p in seq_len(patchcount)) {
        pidx <- i16_at(src, poff + 4L)
        lump <- if (pidx >= 0L && pidx < length(lookup)) lookup[pidx + 1L] else -1L
        tex$patches[[p]] <- list(
          originx = i16_at(src, poff),
          originy = i16_at(src, poff + 2L),
          patch = lump
        )
        poff <- poff + 10L
      }
      j <- 1L
      width <- max(tex$width, 0L)
      while (j <= width %/% 2L) j <- j * 2L
      tex$widthmask <- j - 1L
      tex$col_lump <- rep(-1L, width)
      tex$col_ofs <- numeric(width)
      tex$composite <- NULL
      generate_lookup(res, tex)
      res$tex_index[[tex$name]] <- made
      made <- made + 1L
      res$textures[[made]] <- tex
    }
  }
  res$texturetranslation <- seq_len(made) - 1L
  invisible(res)
}

generate_lookup <- function(res, tex) {
  width <- tex$width
  if (width <= 0L) return(invisible(tex))
  patchcount <- integer(width)
  for (mp in tex$patches) {
    if (is.null(mp) || mp$patch < 0L) next
    pdata <- cache_patch(res, mp$patch)
    if (length(pdata) < 8L) next
    pw <- i16_at(pdata, 0)
    x1 <- mp$originx
    x2 <- x1 + pw
    x <- if (x1 < 0L) 0L else x1
    if (x2 > width) x2 <- width
    while (x < x2) {
      src_col <- x - mp$originx
      patchcount[x + 1L] <- patchcount[x + 1L] + 1L
      tex$col_lump[x + 1L] <- mp$patch
      tex$col_ofs[x + 1L] <- u32_at(pdata, 8L + src_col * 4L)
      x <- x + 1L
    }
  }
  multi <- patchcount > 1L
  if (any(multi)) tex$col_lump[multi] <- -1L
  invisible(tex)
}

generate_composite <- function(res, tex) {
  if (!is.null(tex$composite)) return(invisible(tex))
  buf <- raw(tex$width * tex$height)
  for (mp in tex$patches) {
    if (is.null(mp) || mp$patch < 0L) next
    pdata <- cache_patch(res, mp$patch)
    if (length(pdata) < 8L) next
    pw <- i16_at(pdata, 0)
    x1 <- mp$originx
    x2 <- x1 + pw
    x <- if (x1 < 0L) 0L else x1
    if (x2 > tex$width) x2 <- tex$width
    while (x < x2) {
      colofs <- u32_at(pdata, 8L + (x - mp$originx) * 4L)
      buf <- draw_column_in_cache(pdata, colofs, buf, x, mp$originy, tex$height)
      x <- x + 1L
    }
  }
  tex$composite <- buf
  neg <- tex$col_lump < 0L
  if (any(neg)) {
    xs <- which(neg) - 1L
    tex$col_ofs[neg] <- xs * tex$height
  }
  invisible(tex)
}

draw_column_in_cache <- function(patch, column, cache, x, originy, height) {
  column <- as.numeric(column)
  n <- length(patch)
  while (column < n) {
    topdelta <- as.integer(patch[column + 1])
    if (is.na(topdelta) || topdelta == 255L) break
    length_run <- as.integer(patch[column + 2L])
    if (is.na(length_run) || length_run < 0L) break
    source <- column + 3
    pos <- originy + topdelta
    count <- length_run
    if (pos < 0L) {
      count <- count + pos
      source <- source - pos
      pos <- 0L
    }
    if (pos + count > height) count <- height - pos
    if (count > 0L && source >= 0 && source + count <= n) {
      dest <- x * height + pos
      cache[dest + seq_len(count)] <- patch[source + seq_len(count)]
    }
    column <- column + length_run + 4
  }
  cache
}

texture_num_for_name <- function(res, name) {
  key <- toupper(trimws(substr(as.character(name), 1L, 8L), whitespace = " "))
  if (!nzchar(key) || key == "-") return(0L)
  if (!exists(key, envir = res$tex_index, inherits = FALSE)) return(0L)
  get(key, envir = res$tex_index, inherits = FALSE)
}

texture_height <- function(res, texnum) {
  if (texnum < 0L || texnum >= length(res$textures)) return(0)
  res$textures[[texnum + 1L]]$height * FRACUNIT
}

texture_width <- function(res, texnum) {
  if (texnum < 0L || texnum >= length(res$textures)) return(0L)
  res$textures[[texnum + 1L]]$width
}

column_posts <- function(res, texnum, col) {
  if (texnum <= 0L || texnum >= length(res$textures)) return(list())
  tex <- res$textures[[texnum + 1L]]
  col <- bitwAnd(as.integer(col), tex$widthmask)
  if (is.na(col) || col < 0L) col <- 0L
  lump <- tex$col_lump[col + 1L]
  posts <- list()
  if (lump >= 0L) {
    patch <- cache_patch(res, lump)
    column <- tex$col_ofs[col + 1L]
    n <- length(patch)
    while (column < n) {
      topdelta <- as.integer(patch[column + 1])
      if (is.na(topdelta) || topdelta == 255L) break
      length_run <- as.integer(patch[column + 2L])
      if (is.na(length_run) || length_run < 0L) break
      source <- column + 3
      posts[[length(posts) + 1L]] <- list(
        top = topdelta,
        pixels = patch[source + seq_len(length_run)]
      )
      column <- column + length_run + 4
    }
    return(posts)
  }
  generate_composite(res, tex)
  ofs <- tex$col_ofs[col + 1L]
  pixels <- tex$composite[ofs + seq_len(tex$height)]
  if (length(pixels)) list(list(top = 0L, pixels = pixels)) else list()
}

get_column <- function(res, texnum, col) {
  empty <- raw(128L)
  if (texnum < 0L || texnum >= length(res$textures)) return(empty)
  tex <- res$textures[[texnum + 1L]]
  col <- bitwAnd(as.integer(col), tex$widthmask)
  if (is.na(col) || col < 0L) col <- 0L
  lump <- tex$col_lump[col + 1L]
  if (lump >= 0L) {
    return(column_to_source(cache_patch(res, lump), tex$col_ofs[col + 1L]))
  }
  generate_composite(res, tex)
  ofs <- tex$col_ofs[col + 1L]
  repeat_column(tex$composite[ofs + seq_len(tex$height)])
}

column_to_source <- function(patch, column) {
  buf <- raw(128L)
  column <- as.numeric(column)
  n <- length(patch)
  while (column < n) {
    topdelta <- as.integer(patch[column + 1])
    if (is.na(topdelta) || topdelta == 255L) break
    length_run <- as.integer(patch[column + 2L])
    if (is.na(length_run) || length_run < 0L) break
    source <- column + 3
    ys <- topdelta + seq_len(length_run) - 1L
    ok <- ys >= 0L & ys < 128L
    if (any(ok)) buf[ys[ok] + 1L] <- patch[source + which(ok)]
    column <- column + length_run + 4
  }
  buf
}

repeat_column <- function(colbytes) {
  buf <- raw(128L)
  n <- length(colbytes)
  if (!n) return(buf)
  buf[] <- colbytes[((seq_len(128L) - 1L) %% n) + 1L]
  buf
}

texture_sample <- function(res, texnum) {
  if (is.na(texnum) || texnum < 0L || texnum >= length(res$textures)) return(as.raw(0))
  key <- as.character(texnum)
  hit <- res$sample[[key]]
  if (!is.null(hit)) return(hit)
  tex <- res$textures[[texnum + 1L]]
  col <- get_column(res, texnum, tex$width %/% 2L)
  nz <- which(col != as.raw(0))
  pix <- if (length(nz)) col[nz[(length(nz) + 1L) %/% 2L]] else as.raw(0)
  res$sample[[key]] <- pix
  pix
}
