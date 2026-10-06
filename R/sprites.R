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
# Sprites do WAD (S_START..S_END) e desenho no framebuffer.

MAX_SPRITE_FRAMES <- 29L

init_sprite_defs <- function(wad) {
  start <- wad_check_num(wad, "S_START")
  end <- wad_check_num(wad, "S_END")
  if (start < 0L) start <- wad_check_num(wad, "SS_START")
  if (end < 0L) end <- wad_check_num(wad, "SS_END")
  if (start >= 0L && end > start) {
    first <- start + 1L
    last <- end - 1L
  } else {
    first <- 0L
    last <- wad_num_lumps(wad) - 1L
  }
  buckets <- new.env(parent = emptyenv())
  if (last >= first) {
    for (lump in first:last) {
      name <- wad_lump_name(wad, lump)
      if (nchar(name) < 6L) next
      key <- substr(name, 1L, 4L)
      buckets[[key]] <- c(buckets[[key]], lump)
    }
  }
  result <- new.env(parent = emptyenv())
  for (sprname in ls(buckets)) {
    frames <- lapply(seq_len(MAX_SPRITE_FRAMES), function(i) {
      e <- new.env(parent = emptyenv())
      e$rotate <- -1L
      e$lump <- rep(-1L, 8L)
      e$flip <- rep(0L, 8L)
      e
    })
    maxframe <- -1L
    for (lump in buckets[[sprname]]) {
      name <- wad_lump_name(wad, lump)
      frame <- utf8ToInt(substr(name, 5L, 5L)) - utf8ToInt("A")
      rotation <- utf8ToInt(substr(name, 6L, 6L)) - utf8ToInt("0")
      if (install_sprite_lump(frames, lump, frame, rotation, FALSE)) {
        maxframe <- max(maxframe, frame)
      }
      if (nchar(name) >= 8L) {
        code <- utf8ToInt(substr(name, 7L, 8L))
        if (length(code) == 2L && code[1] >= 65L && code[1] <= 93L && code[2] >= 48L && code[2] <= 56L) {
          frame2 <- code[1] - 65L
          rotation2 <- code[2] - 48L
          if (install_sprite_lump(frames, lump, frame2, rotation2, TRUE)) {
            maxframe <- max(maxframe, frame2)
          }
        }
      }
    }
    if (maxframe >= 0L) result[[sprname]] <- frames[seq_len(maxframe + 1L)]
  }
  result
}

install_sprite_lump <- function(frames, lump, frame, rotation, flipped) {
  if (is.na(frame) || is.na(rotation)) return(FALSE)
  if (frame < 0L || frame >= MAX_SPRITE_FRAMES || rotation < 0L || rotation > 8L) return(FALSE)
  sf <- frames[[frame + 1L]]
  if (rotation == 0L) {
    if (sf$rotate == 1L) return(TRUE)
    sf$rotate <- 0L
    sf$lump[] <- lump
    sf$flip[] <- as.integer(flipped)
    return(TRUE)
  }
  if (sf$rotate == 0L) return(TRUE)
  sf$rotate <- 1L
  idx <- rotation
  if (sf$lump[idx] < 0L) {
    sf$lump[idx] <- lump
    sf$flip[idx] <- as.integer(flipped)
  }
  TRUE
}

lookup_sprite <- function(res, name, ang_to_thing, moangle, frame) {
  base <- toupper(substr(name, 1L, 4L))
  frames <- res$sprites[[base]]
  if (is.null(frames)) return(NULL)
  fi <- bitwAnd(as.integer(frame), 32767L)
  if (is.na(fi) || fi < 0L || fi >= length(frames)) return(NULL)
  sf <- frames[[fi + 1L]]
  if (sf$rotate == 1L) {
    rot <- bitwAnd(as.integer(u32_shr(as_u32(ang_to_thing - moangle + (ANG45 / 2) * 9), 29L)), 7L) + 1L
    lump <- sf$lump[rot]
    flip <- sf$flip[rot]
  } else {
    lump <- sf$lump[1]
    flip <- sf$flip[1]
  }
  if (is.na(lump) || lump < 0L) return(NULL)
  list(lump = lump, flip = flip)
}

draw_psprite <- function(r, name, sx, sy) {
  if (!nzchar(name) || nchar(name) < 6L) return(invisible())
  frame <- utf8ToInt(substr(name, 5L, 5L)) - utf8ToInt("A")
  found <- lookup_sprite(r$res, substr(name, 1L, 4L), 0, 0, frame)
  if (is.null(found)) return(invisible())
  patch <- wad_cache_num(r$res$wad, found$lump)
  w <- i16_at(patch, 0L)
  left <- i16_at(patch, 4L)
  top <- i16_at(patch, 6L)
  scale <- if (is.null(r$pspritescale)) FRACUNIT else r$pspritescale
  iscale <- if (is.null(r$pspriteiscale)) FRACUNIT else r$pspriteiscale
  tx <- sx - 160 * FRACUNIT - left * FRACUNIT
  x1 <- shar(r$centerxfrac + fixed_mul(tx, scale), FRACBITS)
  if (x1 > r$viewwidth) return(invisible())
  tx <- tx + w * FRACUNIT
  x2 <- shar(r$centerxfrac + fixed_mul(tx, scale), FRACBITS) - 1L
  if (x2 < 0L) return(invisible())
  vis_x1 <- max(x1, 0L)
  vis_x2 <- min(x2, r$viewwidth - 1L)
  startfrac <- 0
  if (vis_x1 > x1) startfrac <- startfrac + iscale * (vis_x1 - x1)
  texturemid <- BASEYCENTER * FRACUNIT + FRACUNIT / 2 - (sy - top * FRACUNIT)
  mo <- new.env(parent = emptyenv())
  mo$flags <- 0
  draw_sprite(r, list(
    mo = mo, patch = patch, w = w,
    scale = scale * bitwShiftL(1L, r$detailshift),
    texturemid = texturemid,
    x1 = vis_x1, x2 = vis_x2,
    xiscale = iscale, startfrac = startfrac
  ), FALSE)
  invisible()
}

draw_weapon <- function(r, player, leveltime) {
  if (is.null(player) || player$playerstate == PST_DEAD) return(invisible())
  xy <- weapon_psprite_xy(player, leveltime)
  body <- player$psprite_body
  if (!nzchar(body)) body <- "PISGA0"
  draw_psprite(r, body, xy[1], xy[2])
  if (nzchar(player$psprite_flash)) draw_psprite(r, player$psprite_flash, xy[1], xy[2])
  invisible()
}

draw_sprites <- function(r, world) {
  vis <- list()
  for (mo in world$mobjs) {
    if (!nzchar(mo$sprite)) next
    item <- project_sprite(r, mo)
    if (!is.null(item)) vis[[length(vis) + 1L]] <- item
  }
  if (!length(vis)) return(invisible(r$fb))
  ord <- order(vapply(vis, function(s) s$scale, numeric(1)))
  for (spr in vis[ord]) draw_sprite(r, spr, TRUE)
  invisible(r$fb)
}

project_sprite <- function(r, mo) {
  tr_x <- as_i32(mo$x - r$viewx)
  tr_y <- as_i32(mo$y - r$viewy)
  gxt <- fixed_mul(tr_x, r$viewcos)
  gyt <- -fixed_mul(tr_y, r$viewsin)
  tz <- gxt - gyt
  if (tz < MINZ) return(NULL)
  xscale <- fixed_div(r$projection, tz)
  gxt <- -fixed_mul(tr_x, r$viewsin)
  gyt <- fixed_mul(tr_y, r$viewcos)
  tx <- -(gyt + gxt)
  if (abs(tx) > tz * 4) return(NULL)
  found <- lookup_sprite(r$res, mo$sprite, point_to_angle(r, mo$x, mo$y), mo$angle, mo$frame)
  if (is.null(found)) return(NULL)
  patch <- wad_cache_num(r$res$wad, found$lump)
  w <- i16_at(patch, 0)
  left <- i16_at(patch, 4)
  top <- i16_at(patch, 6)
  tx <- tx - left * FRACUNIT
  x1 <- shar(r$centerxfrac + fixed_mul(tx, xscale), FRACBITS)
  if (x1 > r$viewwidth) return(NULL)
  tx <- tx + w * FRACUNIT
  x2 <- shar(r$centerxfrac + fixed_mul(tx, xscale), FRACBITS) - 1
  if (x2 < 0) return(NULL)
  iscale <- if (xscale) fixed_div(FRACUNIT, xscale) else FRACUNIT
  xiscale <- if (found$flip) -iscale else iscale
  startfrac <- if (found$flip) (w * FRACUNIT - 1) else 0
  vis_x1 <- max(x1, 0)
  vis_x2 <- min(x2, r$viewwidth - 1L)
  if (vis_x1 > vis_x2) return(NULL)
  if (vis_x1 > x1) startfrac <- startfrac + xiscale * (vis_x1 - x1)
  list(
    mo = mo, patch = patch, w = w,
    scale = xscale * bitwShiftL(1L, r$detailshift),
    gx = mo$x, gy = mo$y, gz = mo$z,
    gzt = mo$z + top * FRACUNIT,
    texturemid = mo$z + top * FRACUNIT - r$viewz,
    x1 = vis_x1, x2 = vis_x2,
    xiscale = xiscale, startfrac = startfrac
  )
}

draw_sprite <- function(r, spr, clip_walls) {
  patch <- spr$patch
  patch_w <- spr$w
  iscale <- spr$xiscale
  spryscale <- spr$scale
  y_iscale <- abs(iscale)
  if (r$detailshift) y_iscale <- shar(y_iscale, r$detailshift)
  if (y_iscale < 1) y_iscale <- 1
  sprtopscreen <- r$centeryfrac - fixed_mul(spr$texturemid, spryscale)
  if (clip_walls) {
    clips <- clip_sprite_walls(r, spr)
    cliptop <- clips$top
    clipbot <- clips$bot
  } else {
    cliptop <- rep(-1L, SCREENWIDTH)
    clipbot <- rep(r$viewheight, SCREENWIDTH)
  }
  cm <- if (bitwAnd(spr$mo$flags, MF_SHADOW) != 0) colormap_bytes(r$res, 6L) else {
    if (!is.null(r$fixedcolormap)) r$fixedcolormap else colormap_bytes(r$res, 0L)
  }
  frac <- spr$startfrac
  n <- length(patch)
  for (x in spr$x1:spr$x2) {
    col <- shar(frac, FRACBITS)
    if (col >= 0 && col < patch_w && 8L + col * 4L + 3L < n) {
      column <- u32_at(patch, 8L + col * 4L)
      while (column + 3 < n) {
        topdelta <- as.integer(patch[column + 1])
        if (is.na(topdelta) || topdelta == 255L) break
        length_run <- as.integer(patch[column + 2L])
        if (is.na(length_run) || length_run <= 0L || column + 3 + length_run > n) break
        source <- column + 3
        topscreen <- sprtopscreen + spryscale * topdelta
        bottomscreen <- topscreen + spryscale * length_run
        yl <- shar(topscreen + FRACUNIT - 1, FRACBITS)
        yh <- shar(bottomscreen - 1, FRACBITS)
        if (yl <= cliptop[x + 1L]) yl <- cliptop[x + 1L] + 1L
        if (yh >= clipbot[x + 1L]) yh <- clipbot[x + 1L] - 1L
        if (yl < 0L) yl <- 0L
        if (yh >= r$viewheight) yh <- r$viewheight - 1L
        if (yl <= yh) {
          ys <- seq.int(yl, yh)
          texfrac <- fixed_mul((ys * FRACUNIT) - topscreen, y_iscale)
          texfrac[texfrac < 0] <- 0
          idx <- shar(texfrac, FRACBITS)
          srcs <- source + idx + 1L
          ok <- idx >= 0 & idx < length_run & srcs >= 1L & srcs <= n
          ys <- ys[ok]
          if (length(ys)) {
            pix <- pmin(length(cm) - 1L, pmax(0L, as.integer(patch[srcs[ok]])))
            val <- as.raw(as.integer(cm[pix + 1L]))
            paint_view(r, rep.int(x, length(ys)), ys, val)
          }
        }
        column <- column + length_run + 4
      }
    }
    frac <- frac + iscale
  }
  invisible(NULL)
}

clip_sprite_walls <- function(r, spr) {
  x1 <- spr$x1
  x2 <- spr$x2
  clipbot <- rep(-2L, SCREENWIDTH)
  cliptop <- rep(-2L, SCREENWIDTH)
  if (length(r$drawsegs)) {
    for (di in length(r$drawsegs):1) {
      ds <- r$drawsegs[[di]]
      if (ds$x1 > x2 || ds$x2 < x1) next
      if (ds$silhouette == 0L && is.null(ds$maskedtexturecol)) next
      r1 <- max(ds$x1, x1)
      r2 <- min(ds$x2, x2)
      scale <- max(ds$scale1, ds$scale2)
      lowscale <- min(ds$scale1, ds$scale2)
      in_front <- scale < spr$scale || (
        lowscale < spr$scale && !is.null(ds$curline) &&
          point_on_seg_side(spr$gx, spr$gy, ds$curline) == 0L
      )
      if (in_front) {
        if (!is.null(ds$maskedtexturecol)) render_masked_seg_range(r, ds, r1, r2)
        next
      }
      silhouette <- ds$silhouette
      if (spr$gz >= ds$bsilheight) silhouette <- bitwAnd(silhouette, bitwNot(SIL_BOTTOM))
      if (spr$gzt <= ds$tsilheight) silhouette <- bitwAnd(silhouette, bitwNot(SIL_TOP))
      for (x in r1:r2) {
        i <- x - ds$x1 + 1L
        if (i < 1L || i > length(ds$sprtopclip)) next
        if (bitwAnd(silhouette, SIL_BOTTOM) != 0L && clipbot[x + 1L] == -2L) {
          clipbot[x + 1L] <- ds$sprbottomclip[i]
        }
        if (bitwAnd(silhouette, SIL_TOP) != 0L && cliptop[x + 1L] == -2L) {
          cliptop[x + 1L] <- ds$sprtopclip[i]
        }
      }
    }
  }
  for (x in x1:x2) {
    if (clipbot[x + 1L] == -2L) clipbot[x + 1L] <- r$viewheight
    if (cliptop[x + 1L] == -2L) cliptop[x + 1L] <- -1L
  }
  list(top = cliptop, bot = clipbot)
}

point_on_seg_side <- function(x, y, line) {
  lx <- line$v1$x
  ly <- line$v1$y
  ldx <- line$v2$x - lx
  ldy <- line$v2$y - ly
  if (ldx == 0) {
    if (x <= lx) return(if (ldy > 0) 1L else 0L)
    return(if (ldy < 0) 1L else 0L)
  }
  if (ldy == 0) {
    if (y <= ly) return(if (ldx < 0) 1L else 0L)
    return(if (ldx > 0) 1L else 0L)
  }
  dx <- x - lx
  dy <- y - ly
  left <- fixed_mul(shar(ldy, FRACBITS), dx)
  right <- fixed_mul(dy, shar(ldx, FRACBITS))
  if (right < left) 0L else 1L
}
