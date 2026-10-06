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
# V_DrawPatch no framebuffer de 320x200.

i16_at <- function(buf, off) {
  lo <- as.integer(buf[off + 1L])
  hi <- as.integer(buf[off + 2L])
  v <- lo + hi * 256L
  if (v >= 32768L) v - 65536L else v
}

u32_at <- function(buf, off) {
  b <- as.numeric(buf[off + 1:4])
  b[1] + b[2] * 256 + b[3] * 65536 + b[4] * 16777216
}

draw_patch <- function(fb, x, y, patch) {
  w <- i16_at(patch, 0)
  left <- i16_at(patch, 4)
  top <- i16_at(patch, 6)
  x <- x - left
  y <- y - top
  desttop <- y * SCREENWIDTH + x
  n <- length(patch)
  for (col in seq_len(w) - 1L) {
    if (8 + col * 4 + 3 < n) {
    column <- u32_at(patch, 8 + col * 4)
    while (column + 3 < n) {
      topdelta <- as.integer(patch[column + 1])
      if (is.na(topdelta) || topdelta == 255L) break
      run <- as.integer(patch[column + 2L])
      if (is.na(run) || run <= 0L || column + 3 + run > n) break
      source <- column + 3
      dest <- desttop + topdelta * SCREENWIDTH
      rows <- seq_len(run) - 1L
      dests <- dest + rows * SCREENWIDTH
      srcs <- source + rows + 1L
      ok <- dests >= 0 & dests < SCREENPIXELS & srcs >= 1L & srcs <= n
      if (any(ok)) fb[dests[ok] + 1L] <- patch[srcs[ok]]
      column <- column + run + 4
    }
    }
    desttop <- desttop + 1
  }
  fb
}
