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
# Janela SDL2. O framebuffer continua raw(320 * 200), um indice de PLAYPAL por pixel.

sdl_state <- new.env(parent = emptyenv())
sdl_state$loaded <- FALSE

sdl_ensure <- function() {
  if (isTRUE(sdl_state$loaded)) return(invisible(NULL))
  bin <- file.path(getwd(), "bin")
  bridge <- file.path(bin, "rdoom_sdl.dll")
  sdl <- file.path(bin, "SDL2.dll")
  if (!file.exists(bridge)) stop("Falta bin/rdoom_sdl.dll. Rode build_sdl.bat.")
  if (!file.exists(sdl)) stop("Falta bin/SDL2.dll ao lado de rdoom_sdl.dll.")
  bin_native <- normalizePath(bin, winslash = "\\", mustWork = TRUE)
  Sys.setenv(PATH = paste(bin_native, Sys.getenv("PATH"), sep = ";"))
  dyn.load(normalizePath(sdl, winslash = "/", mustWork = TRUE))
  dyn.load(normalizePath(bridge, winslash = "/", mustWork = TRUE))
  sdl_state$loaded <- TRUE
  invisible(NULL)
}

video_new <- function(scale = 2L) {
  e <- new.env(parent = emptyenv())
  e$fb <- raw(SCREENPIXELS)
  e$pal <- NULL
  e$pal_bytes <- NULL
  e$scale <- as.integer(scale)
  e$open <- FALSE
  e
}

video_init <- function(video) {
  sdl_ensure()
  scale <- as.integer(video$scale)
  if (is.na(scale) || scale < 1L) scale <- 2L
  .Call("rdoom_video_init", SCREENWIDTH, SCREENHEIGHT, scale, PACKAGE = "rdoom_sdl")
  video$open <- TRUE
  if (!is.null(video$pal_bytes)) {
    .Call("rdoom_video_set_palette", video$pal_bytes, PACKAGE = "rdoom_sdl")
  }
  if (isTRUE(video$crt)) .Call("rdoom_set_crt", TRUE, PACKAGE = "rdoom_sdl")
  invisible(video)
}

video_set_playpal <- function(video, playpal) {
  video$playpal_all <- as.raw(as.integer(playpal) %% 256L)
  if (length(video$playpal_all) < 768L) stop("PLAYPAL curto")
  video_select_palette(video, 0L)
}

video_select_palette <- function(video, index) {
  index <- as.integer(index)
  if (is.na(index) || index < 0L) index <- 0L
  off <- index * 768L
  if (off + 768L > length(video$playpal_all)) {
    index <- 0L
    off <- 0L
  }
  bytes <- video$playpal_all[off + seq_len(768L)]
  video$pal_bytes <- bytes
  video$pal_index <- index
  ints <- as.integer(bytes)
  video$pal <- list(
    r = ints[seq(1, 768, 3)],
    g = ints[seq(2, 768, 3)],
    b = ints[seq(3, 768, 3)]
  )
  if (isTRUE(video$open)) {
    .Call("rdoom_video_set_palette", video$pal_bytes, PACKAGE = "rdoom_sdl")
  }
  invisible(video)
}

video_present <- function(video) {
  if (!isTRUE(video$open)) return(invisible(NULL))
  .Call("rdoom_video_present", video$fb, PACKAGE = "rdoom_sdl")
  invisible(NULL)
}

video_blit <- function(video) video_present(video)

video_toggle_fullscreen <- function(video) {
  if (!isTRUE(video$open)) return(invisible(FALSE))
  sdl_ensure()
  .Call("rdoom_toggle_fullscreen", PACKAGE = "rdoom_sdl")
}

video_poll_events <- function() {
  if (!isTRUE(sdl_state$loaded)) return(list())
  .Call("rdoom_poll_events", PACKAGE = "rdoom_sdl")
}

video_shutdown <- function(video) {
  if (!isTRUE(sdl_state$loaded)) return(invisible(NULL))
  if (!is.null(video)) video$open <- FALSE
  .Call("rdoom_video_shutdown", PACKAGE = "rdoom_sdl")
  invisible(NULL)
}

video_raster <- function(video) {
  idx <- as.integer(video$fb) + 1L
  r <- matrix(video$pal$r[idx], nrow = SCREENHEIGHT, byrow = TRUE)
  g <- matrix(video$pal$g[idx], nrow = SCREENHEIGHT, byrow = TRUE)
  b <- matrix(video$pal$b[idx], nrow = SCREENHEIGHT, byrow = TRUE)
  as.raster(array(c(r, g, b), dim = c(SCREENHEIGHT, SCREENWIDTH, 3)), max = 255)
}

video_paint <- function(video) {
  ras <- video_raster(video)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot(NA, xlim = c(0, SCREENWIDTH), ylim = c(0, SCREENHEIGHT),
       xlab = "", ylab = "", axes = FALSE, xaxs = "i", yaxs = "i")
  rasterImage(ras, 0, 0, SCREENWIDTH, SCREENHEIGHT, interpolate = FALSE)
  invisible(ras)
}

video_snapshot <- function(video, path) {
  grDevices::png(path, width = SCREENWIDTH * 2L, height = SCREENHEIGHT * 2L)
  on.exit(grDevices::dev.off(), add = TRUE)
  video_paint(video)
  invisible(path)
}
