# Derretimento da tela. A imagem antiga escorre e a nova entra por cima.

wipe_new <- function() {
  w <- new.env(parent = emptyenv())
  w$start <- raw(SCREENPIXELS)
  w$end <- raw(SCREENPIXELS)
  w$y <- integer(SCREENWIDTH %/% 2L)
  w$active <- FALSE
  w
}

wipe_begin <- function(w, old_fb, new_fb) {
  if (length(old_fb) < SCREENPIXELS || length(new_fb) < SCREENPIXELS) return(invisible(FALSE))
  w$start <- c(as.raw(old_fb[seq_len(SCREENPIXELS)]))
  w$end <- c(as.raw(new_fb[seq_len(SCREENPIXELS)]))
  n <- SCREENWIDTH %/% 2L
  y <- integer(n)
  y[1] <- -(sample.int(16L, 1L) - 1L)
  for (i in 2:n) {
    ny <- y[i - 1L] + sample.int(3L, 1L) - 2L
    if (ny > 0L) ny <- 0L
    else if (ny == -16L) ny <- -15L
    y[i] <- ny
  }
  w$y <- y
  w$active <- TRUE
  invisible(TRUE)
}

wipe_column <- function(fb, src, x0, y0, y1) {
  if (y1 < y0) return(fb)
  rows <- y0:y1
  idx <- rows * SCREENWIDTH + x0 + 1L
  fb[idx] <- src[idx]
  fb[idx + 1L] <- src[idx + 1L]
  fb
}

wipe_shift <- function(fb, src, x0, dest_y, src_y, nrows) {
  if (nrows < 1L) return(fb)
  off <- seq_len(nrows) - 1L
  di <- (dest_y + off) * SCREENWIDTH + x0 + 1L
  si <- (src_y + off) * SCREENWIDTH + x0 + 1L
  fb[di] <- src[si]
  fb[di + 1L] <- src[si + 1L]
  fb
}

wipe_tick <- function(w, fb) {
  if (!isTRUE(w$active)) return(fb)
  h <- SCREENHEIGHT
  done <- TRUE
  n <- length(w$y)
  for (i in seq_len(n)) {
    yi <- w$y[i]
    x0 <- (i - 1L) * 2L
    if (yi < 0L) {
      fb <- wipe_column(fb, w$start, x0, 0L, h - 1L)
      w$y[i] <- yi + 1L
      done <- FALSE
      next
    }
    if (yi >= h) next
    dy <- if (yi < 16L) yi + 1L else 8L
    if (yi + dy > h) dy <- h - yi
    fb <- wipe_column(fb, w$end, x0, yi, yi + dy - 1L)
    yi <- yi + dy
    w$y[i] <- yi
    rem <- h - yi
    if (rem > 0L) fb <- wipe_shift(fb, w$start, x0, yi, 0L, rem)
    done <- FALSE
  }
  if (done) {
    fb[seq_len(SCREENPIXELS)] <- w$end
    w$active <- FALSE
  }
  fb
}
