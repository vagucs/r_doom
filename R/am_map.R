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
# Automapa. Tab abre e fecha. Paredes seguem ML_MAPPED, como no vanilla.

AM_WALL <- 176L
AM_WALL_TELE <- 184L
AM_TS <- 96L
AM_FD <- 64L
AM_CD <- 231L
AM_THING <- 112L
AM_GRAY <- 99L
AM_WHITE <- 209L
AM_GRID <- 104L
AM_FH <- 168L

am_new <- function() {
  e <- new.env(parent = emptyenv())
  e$active <- FALSE
  e$follow <- TRUE
  e$grid <- FALSE
  e$cheating <- 0L
  e$scale <- 0.2
  e$cx <- 0
  e$cy <- 0
  e
}

am_reset <- function(am) {
  if (is.null(am)) return(invisible())
  am$active <- FALSE
  am$follow <- TRUE
  am$grid <- FALSE
  am$cheating <- 0L
  am$scale <- 0.2
  invisible()
}

am_cycle <- function(am) {
  if (is.null(am) || !isTRUE(am$active)) return(FALSE)
  am$cheating <- (as.integer(am$cheating) + 1L) %% 3L
  TRUE
}

am_responder <- function(game, key) {
  am <- game$am
  if (is.null(am) || !identical(game$gamestate, GS_LEVEL) || is.null(game$player)) return(FALSE)
  if (key == "tab") {
    am$active <- !isTRUE(am$active)
    if (isTRUE(am$active)) {
      mo <- game$player$mo
      if (!is.null(mo)) {
        am$cx <- mo$x / FRACUNIT
        am$cy <- mo$y / FRACUNIT
      }
    } else {
      game$need_view <- TRUE
    }
    return(TRUE)
  }
  if (!isTRUE(am$active)) return(FALSE)
  if (key == "f") {
    am$follow <- !isTRUE(am$follow)
    mo <- game$player$mo
    if (isTRUE(am$follow) && !is.null(mo)) {
      am$cx <- mo$x / FRACUNIT
      am$cy <- mo$y / FRACUNIT
    }
    set_player_message(game$player, if (isTRUE(am$follow)) "FOLLOW ON" else "FOLLOW OFF")
    return(TRUE)
  }
  if (key == "g") {
    am$grid <- !isTRUE(am$grid)
    return(TRUE)
  }
  if (key == "minus" || key == "equals") {
    factor <- if (key == "equals") 1.05 else 1 / 1.05
    am$scale <- min(4, max(0.02, am$scale * factor))
    return(TRUE)
  }
  if (!isTRUE(am$follow) && key %in% c("up", "down", "left", "right")) {
    step <- 8 / am$scale
    if (key == "up") am$cy <- am$cy + step
    if (key == "down") am$cy <- am$cy - step
    if (key == "left") am$cx <- am$cx - step
    if (key == "right") am$cx <- am$cx + step
    return(TRUE)
  }
  FALSE
}

am_project <- function(am, x, y) {
  c(160 + (x / FRACUNIT - am$cx) * am$scale, (AM_FH / 2) - (y / FRACUNIT - am$cy) * am$scale)
}

am_collect <- function(buckets, x0, y0, x1, y1, col) {
  if (!is.finite(x0) || !is.finite(y0) || !is.finite(x1) || !is.finite(y1)) return(buckets)
  if (max(x0, x1) < 0 || min(x0, x1) >= SCREENWIDTH || max(y0, y1) < 0 || min(y0, y1) >= AM_FH) return(buckets)
  px <- segment_pixels(x0, y0, x1, y1)
  if (!length(px)) return(buckets)
  px <- px[px %/% SCREENWIDTH < AM_FH]
  if (!length(px)) return(buckets)
  key <- as.character(col)
  buckets[[key]] <- c(buckets[[key]], px)
  buckets
}

am_draw <- function(game) {
  am <- game$am
  world <- game$world
  fb <- raw(SCREENPIXELS)
  if (is.null(am) || is.null(world)) return(fb)
  player <- game$player
  mo <- if (is.null(player)) NULL else player$mo
  if (isTRUE(am$follow) && !is.null(mo)) {
    am$cx <- mo$x / FRACUNIT
    am$cy <- mo$y / FRACUNIT
  }
  cheating <- as.integer(am$cheating)
  allmap <- !is.null(player) && player$powers[PW_ALLMAP + 1L] != 0
  buckets <- list()
  if (isTRUE(am$grid)) {
    block <- 128
    x0 <- am$cx - 160 / am$scale
    x1 <- am$cx + 160 / am$scale
    y0 <- am$cy - (AM_FH / 2) / am$scale
    y1 <- am$cy + (AM_FH / 2) / am$scale
    orgx <- world$bmaporgx / FRACUNIT
    orgy <- world$bmaporgy / FRACUNIT
    gx <- orgx + ceiling((x0 - orgx) / block) * block
    while (gx < x1) {
      a <- am_project(am, gx * FRACUNIT, y0 * FRACUNIT)
      b <- am_project(am, gx * FRACUNIT, y1 * FRACUNIT)
      buckets <- am_collect(buckets, a[1], a[2], b[1], b[2], AM_GRID)
      gx <- gx + block
    }
    gy <- orgy + ceiling((y0 - orgy) / block) * block
    while (gy < y1) {
      a <- am_project(am, x0 * FRACUNIT, gy * FRACUNIT)
      b <- am_project(am, x1 * FRACUNIT, gy * FRACUNIT)
      buckets <- am_collect(buckets, a[1], a[2], b[1], b[2], AM_GRID)
      gy <- gy + block
    }
  }
  for (ln in world$lines) {
    flags <- as.integer(ln$flags)
    mapped <- bitwAnd(flags, ML_MAPPED) != 0L || cheating != 0L
    if (!mapped) {
      if (allmap && bitwAnd(flags, ML_DONTDRAW) == 0L) {
        a <- am_project(am, ln$v1$x, ln$v1$y)
        b <- am_project(am, ln$v2$x, ln$v2$y)
        buckets <- am_collect(buckets, a[1], a[2], b[1], b[2], AM_GRAY)
      }
      next
    }
    if (bitwAnd(flags, ML_DONTDRAW) != 0L && cheating == 0L) next
    if (is.null(ln$backsector)) {
      col <- AM_WALL
    } else if (is.null(ln$frontsector)) {
      next
    } else if (as.integer(ln$special) == 39L) {
      col <- AM_WALL_TELE
    } else if (bitwAnd(flags, ML_SECRET) != 0L && cheating == 0L) {
      col <- AM_WALL
    } else if (ln$backsector$floorheight != ln$frontsector$floorheight) {
      col <- AM_FD
    } else if (ln$backsector$ceilingheight != ln$frontsector$ceilingheight) {
      col <- AM_CD
    } else if (cheating != 0L) {
      col <- AM_TS
    } else {
      next
    }
    a <- am_project(am, ln$v1$x, ln$v1$y)
    b <- am_project(am, ln$v2$x, ln$v2$y)
    buckets <- am_collect(buckets, a[1], a[2], b[1], b[2], col)
  }
  if (cheating == 2L && length(world$mobjs)) {
    for (th in world$mobjs) {
      if (identical(th, mo)) next
      if (!has_flag(th$flags, MF_SHOOTABLE + MF_SPECIAL)) next
      p <- am_project(am, th$x, th$y)
      buckets <- am_collect(buckets, p[1] - 1, p[2], p[1] + 1, p[2], AM_THING)
      buckets <- am_collect(buckets, p[1], p[2] - 1, p[1], p[2] + 1, AM_THING)
    }
  }
  if (!is.null(mo)) {
    ang <- (as.numeric(mo$angle) %% 4294967296) / 4294967296 * 2 * pi
    ca <- cos(ang)
    sa <- sin(ang)
    p <- am_project(am, mo$x, mo$y)
    tip <- c(p[1] + 7 * ca, p[2] - 7 * sa)
    left <- c(p[1] + 3 * ca - 2 * sa, p[2] - 3 * sa - 2 * ca)
    right <- c(p[1] + 3 * ca + 2 * sa, p[2] - 3 * sa + 2 * ca)
    buckets <- am_collect(buckets, p[1], p[2], tip[1], tip[2], AM_WHITE)
    buckets <- am_collect(buckets, tip[1], tip[2], left[1], left[2], AM_WHITE)
    buckets <- am_collect(buckets, tip[1], tip[2], right[1], right[2], AM_WHITE)
  }
  for (key in names(buckets)) {
    px <- buckets[[key]]
    if (length(px)) fb[px + 1L] <- as.raw(as.integer(key))
  }
  fb
}
