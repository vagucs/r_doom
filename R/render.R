# Vista: BSP, paredes, chao, teto e ceu. Sprites entram depois, em sprites.R.

renderer_new <- function(res) {
  init_tables()
  r <- new.env(parent = emptyenv())
  r$res <- res
  r$detailshift <- 0L
  r$screenblocks <- 10L
  r$fb <- raw(SCREENPIXELS)
  r$solid <- list()
  r$newend <- 0L
  r$visplanes <- list()
  r$drawsegs <- list()
  r$colcache <- new.env(parent = emptyenv())
  r$extralight <- 0L
  r$fixedcolormap <- NULL
  r$viewx <- r$viewy <- r$viewz <- r$viewangle <- 0
  r$viewsin <- r$viewcos <- 0
  renderer_set_view_size(r, 10L, 0L)
  r
}

renderer_set_view_size <- function(r, blocks, detail) {
  blocks <- max(3L, min(11L, as.integer(blocks)))
  detail <- if (detail) 1L else 0L
  if (identical(r$screenblocks, blocks) && identical(r$detailshift, detail) && !is.null(r$xtoviewangle)) {
    return(invisible(r))
  }
  r$screenblocks <- blocks
  r$detailshift <- detail
  if (blocks == 11L) {
    scaled <- SCREENWIDTH
    viewheight <- SCREENHEIGHT
  } else {
    scaled <- blocks * 32L
    viewheight <- (blocks * 168L) %/% 10L
    viewheight <- viewheight - viewheight %% 8L
  }
  r$scaledviewwidth <- scaled
  r$viewwidth <- bitwShiftR(scaled, detail)
  r$viewheight <- viewheight
  r$centerx <- r$viewwidth %/% 2L
  r$centery <- r$viewheight %/% 2L
  r$centerxfrac <- r$centerx * FRACUNIT
  r$centeryfrac <- r$centery * FRACUNIT
  r$projection <- r$centerxfrac
  r$viewwindowx <- bitwShiftR(SCREENWIDTH - scaled, 1L)
  r$viewwindowy <- if (scaled == SCREENWIDTH) 0L else bitwShiftR(SCREENHEIGHT - SBARHEIGHT - viewheight, 1L)
  r$ylookup <- (seq_len(SCREENHEIGHT) - 1L + r$viewwindowy) * SCREENWIDTH
  r$columnofs <- r$viewwindowx + (seq_len(SCREENWIDTH) - 1L)
  r$ceilingclip <- integer(max(1L, r$viewwidth))
  r$floorclip <- integer(max(1L, r$viewwidth))
  r$pspritescale <- (FRACUNIT * r$viewwidth) %/% SCREENWIDTH
  r$pspriteiscale <- (FRACUNIT * SCREENWIDTH) %/% max(1L, r$viewwidth)
  init_mapping(r)
  init_slopes(r)
  init_lights(r)
  invisible(r)
}

init_mapping <- function(r) {
  half <- FINEANGLES %/% 2L
  focallength <- fixed_div(r$centerxfrac, finetangent[half %/% 2L + FIELDOFVIEW %/% 2L + 1L])
  tox <- integer(half)
  for (i in seq_len(half) - 1L) {
    ft <- finetangent[i + 1L]
    if (ft > FRACUNIT * 2) {
      t <- -1L
    } else if (ft < -FRACUNIT * 2) {
      t <- r$viewwidth + 1L
    } else {
      t <- shar(r$centerxfrac - fixed_mul(ft, focallength) + FRACUNIT - 1, FRACBITS)
      t <- max(-1L, min(r$viewwidth + 1L, t))
    }
    tox[i + 1L] <- t
  }
  xtova <- numeric(r$viewwidth + 1L)
  for (x in seq_len(r$viewwidth + 1L) - 1L) {
    i <- 0L
    while (i < half && tox[i + 1L] > x) i <- i + 1L
    xtova[x + 1L] <- as_u32(i * bitwShiftL(1L, ANGLETOFINESHIFT) - ANG90)
  }
  tox[tox == -1L] <- 0L
  tox[tox == r$viewwidth + 1L] <- r$viewwidth
  r$viewangletox <- tox
  r$xtoviewangle <- xtova
  r$clipangle <- xtova[1]
  invisible(r)
}

init_lights <- function(r) {
  scalelight <- matrix(0L, nrow = MAXLIGHTSCALE, ncol = LIGHTLEVELS)
  zlight <- matrix(0L, nrow = MAXLIGHTZ, ncol = LIGHTLEVELS)
  vw <- max(1L, bitwShiftL(r$viewwidth, r$detailshift))
  for (i in seq_len(LIGHTLEVELS) - 1L) {
    startmap <- ((LIGHTLEVELS - 1L - i) * 2L) * NUMCOLORMAPS %/% LIGHTLEVELS
    for (j in seq_len(MAXLIGHTZ) - 1L) {
      scale <- shar(fixed_div((SCREENWIDTH %/% 2L * FRACUNIT), bitwShiftL(j + 1L, LIGHTZSHIFT)), LIGHTSCALESHIFT)
      level <- startmap - scale %/% 2L
      zlight[j + 1L, i + 1L] <- max(0L, min(NUMCOLORMAPS - 1L, level))
    }
    for (j in seq_len(MAXLIGHTSCALE) - 1L) {
      level <- startmap - as.integer(j * SCREENWIDTH / vw / 2)
      scalelight[j + 1L, i + 1L] <- max(0L, min(NUMCOLORMAPS - 1L, level))
    }
  }
  r$scalelight <- scalelight
  r$zlight <- zlight
  invisible(r)
}

init_slopes <- function(r) {
  yslope <- numeric(r$viewheight)
  for (i in seq_len(r$viewheight) - 1L) {
    dy <- abs(((i - r$centery) * FRACUNIT) + FRACUNIT %/% 2L)
    dy <- max(dy, 1)
    yslope[i + 1L] <- fixed_div((bitwShiftL(r$viewwidth, r$detailshift) %/% 2L) * FRACUNIT, dy)
  }
  distscale <- numeric(r$viewwidth)
  for (i in seq_len(r$viewwidth) - 1L) {
    cosadj <- abs_fixed(fine_cos(r$xtoviewangle[i + 1L]))
    distscale[i + 1L] <- fixed_div(FRACUNIT, max(cosadj, 1))
  }
  r$yslope <- yslope
  r$distscale <- distscale
  invisible(r)
}

renderer_setup_frame <- function(r, x, y, z, angle, extra_light = 0L, fixedcolormap = 0L) {
  r$viewx <- x
  r$viewy <- y
  r$viewz <- z
  r$viewangle <- as_u32(angle)
  r$viewsin <- fine_sin(r$viewangle)
  r$viewcos <- fine_cos(r$viewangle)
  r$extralight <- extra_light
  r$fixedcolormap <- if (fixedcolormap) colormap_bytes(r$res, fixedcolormap) else NULL
  ang <- bitwAnd(as.integer(u32_shr(as_u32(r$viewangle - ANG90), ANGLETOFINESHIFT)), FINEMASK)
  denom <- if (r$centerxfrac) r$centerxfrac else 1
  r$basexscale <- fixed_div(finesine[bitwAnd(ang + FINEANGLES %/% 4L, FINEMASK) + 1L], denom)
  r$baseyscale <- -fixed_div(finesine[ang + 1L], denom)
  invisible(r)
}

renderer_render <- function(r, world, fb) {
  r$fb <- fb
  r$visplanes <- list()
  r$drawsegs <- list()
  r$colcache <- new.env(parent = emptyenv())
  clear_clip(r)
  if (length(world$nodes)) render_bsp_node(r, world, length(world$nodes) - 1L) else render_subsector(r, world, 0L)
  draw_planes(r)
  invisible(r$fb)
}

fetch_column <- function(r, tex, col) {
  key <- paste(tex, col, sep = ":")
  hit <- r$colcache[[key]]
  if (!is.null(hit)) return(hit)
  hit <- get_column(r$res, tex, col)
  r$colcache[[key]] <- hit
  hit
}

point_to_angle <- function(r, x, y) {
  x <- as_i32(x - r$viewx)
  y <- as_i32(y - r$viewy)
  if (x == 0 && y == 0) return(0)
  if (x >= 0) {
    if (y >= 0) {
      if (x > y) return(tantoangle[slope_div(y, x) + 1L])
      return(as_u32(ANG90 - 1 - tantoangle[slope_div(x, y) + 1L]))
    }
    y <- -y
    if (x > y) return(as_u32(-tantoangle[slope_div(y, x) + 1L]))
    return(as_u32(ANG270 + tantoangle[slope_div(x, y) + 1L]))
  }
  x <- -x
  if (y >= 0) {
    if (x > y) return(as_u32(ANG180 - 1 - tantoangle[slope_div(y, x) + 1L]))
    return(as_u32(ANG90 + tantoangle[slope_div(x, y) + 1L]))
  }
  y <- -y
  if (x > y) return(as_u32(ANG180 + tantoangle[slope_div(y, x) + 1L]))
  as_u32(ANG270 - 1 - tantoangle[slope_div(x, y) + 1L])
}

point_on_side <- function(r, x, y, node) {
  dx <- as_i32(x - node$x)
  dy <- as_i32(y - node$y)
  left <- as_i32(trunc(node$dy / FRACUNIT)) * dx
  right <- dy * as_i32(trunc(node$dx / FRACUNIT))
  if (right >= left) 1L else 0L
}

scale_from_global_angle <- function(r, visangle) {
  anglea <- as_u32(ANG90 + as_u32(visangle - r$viewangle))
  angleb <- as_u32(ANG90 + as_u32(visangle - r$rw_normalangle))
  sinea <- finesine[bitwAnd(as.integer(u32_shr(anglea, ANGLETOFINESHIFT)), FINEMASK) + 1L]
  sineb <- finesine[bitwAnd(as.integer(u32_shr(angleb, ANGLETOFINESHIFT)), FINEMASK) + 1L]
  num <- fixed_mul(r$projection, sineb) * bitwShiftL(1L, r$detailshift)
  den <- fixed_mul(r$rw_distance, sinea)
  if (den > shar(num, 16L) && den != 0) {
    scale <- fixed_div(num, den)
    if (scale > 64 * FRACUNIT) 64 * FRACUNIT else if (scale < 256) 256 else scale
  } else {
    64 * FRACUNIT
  }
}

clear_clip <- function(r) {
  r$solid <- list(
    list(first = -2147483647, last = -1),
    list(first = r$viewwidth, last = 2147483647)
  )
  r$newend <- 2L
  r$floorclip[] <- r$viewheight
  r$ceilingclip[] <- -1L
}

render_bsp_node <- function(r, world, bspnum) {
  bspnum <- as.integer(bspnum)
  if (is.na(bspnum)) return(invisible())
  if (bspnum < 0L || bitwAnd(bspnum, NF_SUBSECTOR) != 0L) {
    idx <- if (bspnum < 0L) 0L else bitwAnd(bspnum, 32767L)
    render_subsector(r, world, idx)
    return(invisible())
  }
  node <- world$nodes[[bspnum + 1L]]
  side <- point_on_side(r, r$viewx, r$viewy, node)
  render_bsp_node(r, world, node$children[side + 1L])
  render_bsp_node(r, world, node$children[bitwXor(side, 1L) + 1L])
}

render_subsector <- function(r, world, num) {
  sub <- world$subsectors[[num + 1L]]
  r$frontsector <- sub$sector
  if (is.null(r$frontsector)) return(invisible())
  light <- (r$frontsector$lightlevel %/% 16L) + r$extralight
  light <- max(0L, min(LIGHTLEVELS - 1L, light))
  r$walllights <- r$scalelight[, light + 1L]
  r$floorplane <- find_plane(r, r$frontsector$floorheight, r$frontsector$floorpic, r$frontsector$lightlevel)
  r$ceilingplane <- find_plane(r, r$frontsector$ceilingheight, r$frontsector$ceilingpic, r$frontsector$lightlevel)
  if (!sub$numlines) return(invisible())
  for (i in seq_len(sub$numlines) - 1L) add_line(r, world$segs[[sub$firstline + i + 1L]])
}

find_plane <- function(r, height, picnum, lightlevel) {
  if (identical(picnum, r$res$skyflatnum)) {
    height <- 0
    lightlevel <- 0L
  }
  for (p in r$visplanes) {
    if (p$height == height && p$picnum == picnum && p$lightlevel == lightlevel) return(p)
  }
  p <- new.env(parent = emptyenv())
  p$height <- height
  p$picnum <- picnum
  p$lightlevel <- lightlevel
  p$minx <- r$viewwidth
  p$maxx <- -1L
  p$top <- rep(255L, SCREENWIDTH)
  p$bottom <- integer(SCREENWIDTH)
  r$visplanes[[length(r$visplanes) + 1L]] <- p
  p
}

check_plane <- function(r, pl, start, stop) {
  if (is.null(pl)) return(find_plane(r, 0, 0L, 0L))
  if (start < pl$minx) {
    intrl <- pl$minx
    unionl <- start
  } else {
    unionl <- pl$minx
    intrl <- start
  }
  if (stop > pl$maxx) {
    intrh <- pl$maxx
    unionh <- stop
  } else {
    unionh <- pl$maxx
    intrh <- stop
  }
  x <- intrl
  while (x <= intrh) {
    if (x >= 0L && x < SCREENWIDTH && pl$top[x + 1L] != 255L) break
    x <- x + 1L
  }
  if (x > intrh) {
    pl$minx <- unionl
    pl$maxx <- unionh
    return(pl)
  }
  dup_plane(r, pl, start, stop)
}

dup_plane <- function(r, src, start, stop) {
  p <- new.env(parent = emptyenv())
  p$height <- src$height
  p$picnum <- src$picnum
  p$lightlevel <- src$lightlevel
  p$minx <- start
  p$maxx <- stop
  p$top <- rep(255L, SCREENWIDTH)
  p$bottom <- integer(SCREENWIDTH)
  r$visplanes[[length(r$visplanes) + 1L]] <- p
  p
}

add_line <- function(r, line) {
  r$curline <- line
  angle1 <- point_to_angle(r, line$v1$x, line$v1$y)
  angle2 <- point_to_angle(r, line$v2$x, line$v2$y)
  span <- as_u32(angle1 - angle2)
  if (span >= ANG180) return(invisible())
  r$rw_angle1 <- angle1
  angle1 <- as_u32(angle1 - r$viewangle)
  angle2 <- as_u32(angle2 - r$viewangle)
  tspan <- as_u32(angle1 + r$clipangle)
  if (tspan > as_u32(2 * r$clipangle)) {
    tspan <- as_u32(tspan - 2 * r$clipangle)
    if (tspan >= span) return(invisible())
    angle1 <- r$clipangle
  }
  tspan <- as_u32(r$clipangle - angle2)
  if (tspan > as_u32(2 * r$clipangle)) {
    tspan <- as_u32(tspan - 2 * r$clipangle)
    if (tspan >= span) return(invisible())
    angle2 <- as_u32(-r$clipangle)
  }
  mask <- FINEANGLES %/% 2L - 1L
  x1 <- r$viewangletox[bitwAnd(as.integer(u32_shr(as_u32(angle1 + ANG90), ANGLETOFINESHIFT)), mask) + 1L]
  x2 <- r$viewangletox[bitwAnd(as.integer(u32_shr(as_u32(angle2 + ANG90), ANGLETOFINESHIFT)), mask) + 1L]
  if (x1 == x2) return(invisible())
  r$backsector <- line$backsector
  if (is.null(r$backsector)) {
    clip_solid(r, x1, x2 - 1L)
    return(invisible())
  }
  if (r$backsector$ceilingheight <= r$frontsector$floorheight ||
      r$backsector$floorheight >= r$frontsector$ceilingheight) {
    clip_solid(r, x1, x2 - 1L)
    return(invisible())
  }
  clip_pass(r, x1, x2 - 1L)
}

clip_solid <- function(r, first, last) {
  if (first > last) return(invisible())
  start <- 0L
  while (start < r$newend && r$solid[[start + 1L]]$last < first - 1L) start <- start + 1L
  if (start >= r$newend) {
    store_wall_range(r, first, last)
    return(invisible())
  }
  if (first < r$solid[[start + 1L]]$first) {
    if (last < r$solid[[start + 1L]]$first - 1L) {
      store_wall_range(r, first, last)
      r$solid <- append(r$solid, list(list(first = first, last = last)), after = start)
      r$newend <- r$newend + 1L
      return(invisible())
    }
    store_wall_range(r, first, r$solid[[start + 1L]]$first - 1L)
    r$solid[[start + 1L]]$first <- first
  }
  if (last <= r$solid[[start + 1L]]$last) return(invisible())
  nexti <- start
  while (nexti + 1L < r$newend && last >= r$solid[[nexti + 2L]]$first - 1L) {
    store_wall_range(r, r$solid[[nexti + 1L]]$last + 1L, r$solid[[nexti + 2L]]$first - 1L)
    nexti <- nexti + 1L
    if (last <= r$solid[[nexti + 1L]]$last) {
      r$solid[[start + 1L]]$last <- r$solid[[nexti + 1L]]$last
      crunch_solid(r, start, nexti)
      return(invisible())
    }
  }
  store_wall_range(r, r$solid[[nexti + 1L]]$last + 1L, last)
  r$solid[[start + 1L]]$last <- last
  crunch_solid(r, start, nexti)
}

crunch_solid <- function(r, start, nexti) {
  if (nexti == start) return(invisible())
  tail <- if (nexti + 2L <= r$newend) r$solid[(nexti + 2L):r$newend] else list()
  head <- r$solid[seq_len(start + 1L)]
  r$solid <- c(head, tail)
  r$newend <- length(r$solid)
}

clip_pass <- function(r, first, last) {
  if (first > last) return(invisible())
  start <- 0L
  while (start < r$newend && r$solid[[start + 1L]]$last < first - 1L) start <- start + 1L
  if (start >= r$newend) {
    store_wall_range(r, first, last)
    return(invisible())
  }
  if (first < r$solid[[start + 1L]]$first) {
    if (last < r$solid[[start + 1L]]$first - 1L) {
      store_wall_range(r, first, last)
      return(invisible())
    }
    store_wall_range(r, first, r$solid[[start + 1L]]$first - 1L)
  }
  if (last <= r$solid[[start + 1L]]$last) return(invisible())
  nexti <- start
  while (nexti + 1L < r$newend && last >= r$solid[[nexti + 2L]]$first - 1L) {
    store_wall_range(r, r$solid[[nexti + 1L]]$last + 1L, r$solid[[nexti + 2L]]$first - 1L)
    nexti <- nexti + 1L
    if (last <= r$solid[[nexti + 1L]]$last) return(invisible())
  }
  store_wall_range(r, r$solid[[nexti + 1L]]$last + 1L, last)
}

store_wall_range <- function(r, start, stop) {
  if (start < 0L) start <- 0L
  if (stop >= r$viewwidth) stop <- r$viewwidth - 1L
  if (start > stop) return(invisible())
  line <- r$curline
  linedef <- line$linedef
  sidedef <- line$sidedef
  if (is.null(sidedef)) return(invisible())
  linedef$flags <- bitwOr(linedef$flags, ML_MAPPED)
  r$rw_normalangle <- as_u32(line$angle + ANG90)
  offsetangle <- as_u32(r$rw_normalangle - r$rw_angle1)
  if (offsetangle > ANG180) offsetangle <- as_u32(-offsetangle)
  if (offsetangle > ANG90) offsetangle <- ANG90
  distangle <- as_u32(ANG90 - offsetangle)
  hyp <- point_to_dist(r, line$v1$x, line$v1$y)
  r$rw_distance <- fixed_mul(hyp, finesine[bitwAnd(as.integer(u32_shr(distangle, ANGLETOFINESHIFT)), FINEMASK) + 1L])
  r$rw_x <- start
  r$rw_start <- start
  r$rw_stopx <- stop + 1L
  r$rw_scale <- scale_from_global_angle(r, as_u32(r$viewangle + r$xtoviewangle[start + 1L]))
  if (stop > start) {
    scale2 <- scale_from_global_angle(r, as_u32(r$viewangle + r$xtoviewangle[stop + 1L]))
    r$rw_scalestep <- (scale2 - r$rw_scale) / (stop - start)
  } else {
    r$rw_scalestep <- 0
  }
  r$worldtop <- r$frontsector$ceilingheight - r$viewz
  r$worldbottom <- r$frontsector$floorheight - r$viewz
  r$midtexture <- 0L
  r$toptexture <- 0L
  r$bottomtexture <- 0L
  r$maskedtexture <- FALSE
  r$maskedtexturecol <- NULL
  if (is.null(r$backsector)) {
    r$midtexture <- sidedef$midtexture
    r$markfloor <- TRUE
    r$markceiling <- TRUE
    if (bitwAnd(linedef$flags, ML_DONTPEGBOTTOM) != 0L) {
      vtop <- r$frontsector$floorheight + texture_height(r$res, r$midtexture)
      r$rw_midtexturemid <- vtop - r$viewz
    } else {
      r$rw_midtexturemid <- r$worldtop
    }
    r$rw_midtexturemid <- r$rw_midtexturemid + sidedef$rowoffset
  } else {
    r$worldhigh <- r$backsector$ceilingheight - r$viewz
    r$worldlow <- r$backsector$floorheight - r$viewz
    if (r$frontsector$ceilingpic == r$res$skyflatnum && r$backsector$ceilingpic == r$res$skyflatnum) {
      r$worldtop <- r$worldhigh
    }
    r$markfloor <- r$worldlow != r$worldbottom ||
      r$backsector$floorpic != r$frontsector$floorpic ||
      r$backsector$lightlevel != r$frontsector$lightlevel
    r$markceiling <- r$worldhigh != r$worldtop ||
      r$backsector$ceilingpic != r$frontsector$ceilingpic ||
      r$backsector$lightlevel != r$frontsector$lightlevel
    if (r$backsector$ceilingheight <= r$frontsector$floorheight ||
        r$backsector$floorheight >= r$frontsector$ceilingheight) {
      r$markceiling <- TRUE
      r$markfloor <- TRUE
    }
    r$rw_toptexturemid <- 0
    r$rw_bottomtexturemid <- 0
    if (r$worldhigh < r$worldtop) {
      r$toptexture <- sidedef$toptexture
      if (bitwAnd(linedef$flags, ML_DONTPEGTOP) != 0L) {
        r$rw_toptexturemid <- r$worldtop
      } else {
        vtop <- r$backsector$ceilingheight + texture_height(r$res, if (r$toptexture) r$toptexture else 0L)
        r$rw_toptexturemid <- vtop - r$viewz
      }
    }
    if (r$worldlow > r$worldbottom) {
      r$bottomtexture <- sidedef$bottomtexture
      if (bitwAnd(linedef$flags, ML_DONTPEGBOTTOM) != 0L) {
        r$rw_bottomtexturemid <- r$worldtop
      } else {
        r$rw_bottomtexturemid <- r$worldlow
      }
    }
    r$rw_toptexturemid <- r$rw_toptexturemid + sidedef$rowoffset
    r$rw_bottomtexturemid <- r$rw_bottomtexturemid + sidedef$rowoffset
    if (sidedef$midtexture) {
      r$maskedtexture <- TRUE
      r$maskedtexturecol <- rep(SHRT_MAX, stop - start + 1L)
    }
  }
  r$segtextured <- r$midtexture != 0L || r$toptexture != 0L || r$bottomtexture != 0L || r$maskedtexture
  if (r$segtextured) {
    offsetangle <- as_u32(r$rw_normalangle - r$rw_angle1)
    if (offsetangle > ANG180) offsetangle <- as_u32(-offsetangle)
    r$rw_offset <- fixed_mul(hyp, finesine[bitwAnd(as.integer(u32_shr(offsetangle, ANGLETOFINESHIFT)), FINEMASK) + 1L])
    if (as_u32(r$rw_normalangle - r$rw_angle1) < ANG180) r$rw_offset <- -r$rw_offset
    r$rw_offset <- r$rw_offset + sidedef$textureoffset + line$offset
    r$rw_centerangle <- as_u32(ANG90 + r$viewangle - r$rw_normalangle)
  }
  if (r$frontsector$floorheight >= r$viewz) r$markfloor <- FALSE
  if (r$frontsector$ceilingheight <= r$viewz && r$frontsector$ceilingpic != r$res$skyflatnum) {
    r$markceiling <- FALSE
  }
  if (r$markceiling) r$ceilingplane <- check_plane(r, r$ceilingplane, start, stop)
  if (r$markfloor) r$floorplane <- check_plane(r, r$floorplane, start, stop)
  r$worldtop <- shar(r$worldtop, 4L)
  r$worldbottom <- shar(r$worldbottom, 4L)
  r$topstep <- -fixed_mul(r$rw_scalestep, r$worldtop)
  r$topfrac <- shar(r$centeryfrac, 4L) - fixed_mul(r$worldtop, r$rw_scale)
  r$bottomstep <- -fixed_mul(r$rw_scalestep, r$worldbottom)
  r$bottomfrac <- shar(r$centeryfrac, 4L) - fixed_mul(r$worldbottom, r$rw_scale)
  if (!is.null(r$backsector)) {
    r$worldhigh <- shar(r$worldhigh, 4L)
    r$worldlow <- shar(r$worldlow, 4L)
    if (r$worldhigh < r$worldtop) {
      r$pixhigh <- shar(r$centeryfrac, 4L) - fixed_mul(r$worldhigh, r$rw_scale)
      r$pixhighstep <- -fixed_mul(r$rw_scalestep, r$worldhigh)
    }
    if (r$worldlow > r$worldbottom) {
      r$pixlow <- shar(r$centeryfrac, 4L) - fixed_mul(r$worldlow, r$rw_scale)
      r$pixlowstep <- -fixed_mul(r$rw_scalestep, r$worldlow)
    }
  }
  scale1 <- r$rw_scale
  render_seg_loop(r)
  push_drawseg(r, start, stop, scale1)
}

push_drawseg <- function(r, start, stop, scale1) {
  ds <- new.env(parent = emptyenv())
  ds$x1 <- start
  ds$x2 <- stop
  ds$scale1 <- scale1
  ds$scalestep <- r$rw_scalestep
  ds$scale2 <- scale1 + r$rw_scalestep * max(0, stop - start)
  ds$curline <- r$curline
  ds$maskedtexturecol <- r$maskedtexturecol
  width <- stop - start + 1L
  if (is.null(r$backsector)) {
    ds$silhouette <- SIL_BOTH
    ds$bsilheight <- INT_MAX
    ds$tsilheight <- INT_MIN
    ds$sprtopclip <- rep(r$viewheight, width)
    ds$sprbottomclip <- rep(-1L, width)
  } else {
    ds$silhouette <- SIL_NONE
    ds$bsilheight <- 0
    ds$tsilheight <- 0
    if (r$frontsector$floorheight > r$backsector$floorheight) {
      ds$silhouette <- SIL_BOTTOM
      ds$bsilheight <- r$frontsector$floorheight
    } else if (r$backsector$floorheight > r$viewz) {
      ds$silhouette <- SIL_BOTTOM
      ds$bsilheight <- INT_MAX
    }
    if (r$frontsector$ceilingheight < r$backsector$ceilingheight) {
      ds$silhouette <- bitwOr(ds$silhouette, SIL_TOP)
      ds$tsilheight <- r$frontsector$ceilingheight
    } else if (r$backsector$ceilingheight < r$viewz) {
      ds$silhouette <- bitwOr(ds$silhouette, SIL_TOP)
      ds$tsilheight <- INT_MIN
    }
    if (r$backsector$ceilingheight <= r$frontsector$floorheight) {
      ds$silhouette <- bitwOr(ds$silhouette, SIL_BOTTOM)
      ds$bsilheight <- INT_MAX
    }
    if (r$backsector$floorheight >= r$frontsector$ceilingheight) {
      ds$silhouette <- bitwOr(ds$silhouette, SIL_TOP)
      ds$tsilheight <- INT_MIN
    }
    ds$sprtopclip <- r$ceilingclip[(start + 1L):(stop + 1L)]
    ds$sprbottomclip <- r$floorclip[(start + 1L):(stop + 1L)]
    if (r$maskedtexture) {
      if (bitwAnd(ds$silhouette, SIL_TOP) == 0L) {
        ds$silhouette <- bitwOr(ds$silhouette, SIL_TOP)
        ds$tsilheight <- INT_MIN
      }
      if (bitwAnd(ds$silhouette, SIL_BOTTOM) == 0L) {
        ds$silhouette <- bitwOr(ds$silhouette, SIL_BOTTOM)
        ds$bsilheight <- INT_MAX
      }
    }
  }
  r$drawsegs[[length(r$drawsegs) + 1L]] <- ds
}

point_to_dist <- function(r, x, y) {
  dx <- abs_fixed(x - r$viewx)
  dy <- abs_fixed(y - r$viewy)
  if (dy > dx) {
    tmp <- dx
    dx <- dy
    dy <- tmp
  }
  if (dx == 0) return(0)
  frac <- fixed_div(dy, dx)
  ang <- u32_shr(tantoangle[min(shar(frac, DBITS), 2048) + 1L] + ANG90, ANGLETOFINESHIFT)
  fixed_div(dx, finesine[bitwAnd(as.integer(ang), FINEMASK) + 1L])
}

render_seg_loop <- function(r) {
  texturecolumn <- 0L
  while (r$rw_x < r$rw_stopx) {
    yl <- shar(r$topfrac + HEIGHTUNIT - 1, HEIGHTBITS)
    if (yl < r$ceilingclip[r$rw_x + 1L] + 1L) yl <- r$ceilingclip[r$rw_x + 1L] + 1L
    if (r$markceiling && !is.null(r$ceilingplane)) {
      top <- r$ceilingclip[r$rw_x + 1L] + 1L
      bottom <- yl - 1L
      if (bottom >= r$floorclip[r$rw_x + 1L]) bottom <- r$floorclip[r$rw_x + 1L] - 1L
      if (top <= bottom) {
        r$ceilingplane$top[r$rw_x + 1L] <- top
        r$ceilingplane$bottom[r$rw_x + 1L] <- bottom
      }
    }
    yh <- shar(r$bottomfrac, HEIGHTBITS)
    if (yh >= r$floorclip[r$rw_x + 1L]) yh <- r$floorclip[r$rw_x + 1L] - 1L
    if (r$markfloor && !is.null(r$floorplane)) {
      top <- yh + 1L
      bottom <- r$floorclip[r$rw_x + 1L] - 1L
      if (top <= r$ceilingclip[r$rw_x + 1L]) top <- r$ceilingclip[r$rw_x + 1L] + 1L
      if (top <= bottom) {
        r$floorplane$top[r$rw_x + 1L] <- top
        r$floorplane$bottom[r$rw_x + 1L] <- bottom
      }
    }
    if (r$segtextured) {
      angle <- u32_shr(as_u32(r$rw_centerangle + r$xtoviewangle[r$rw_x + 1L]), ANGLETOFINESHIFT)
      tanv <- finetangent[bitwAnd(as.integer(angle), FINEANGLES %/% 2L - 1L) + 1L]
      texturecolumn <- shar(r$rw_offset - fixed_mul(tanv, r$rw_distance), FRACBITS)
      index <- u32_shr(r$rw_scale, LIGHTSCALESHIFT)
      if (index >= MAXLIGHTSCALE) index <- MAXLIGHTSCALE - 1L
      r$dc_colormap <- if (!is.null(r$fixedcolormap)) r$fixedcolormap else colormap_bytes(r$res, r$walllights[index + 1L])
      r$dc_x <- r$rw_x
      r$dc_iscale <- if (r$rw_scale) trunc(4294967295 / r$rw_scale) else 0
    }
    if (r$midtexture) {
      r$dc_yl <- yl
      r$dc_yh <- yh
      r$dc_texturemid <- r$rw_midtexturemid
      r$dc_source <- fetch_column(r, r$midtexture, texturecolumn)
      draw_column(r)
      r$ceilingclip[r$rw_x + 1L] <- r$viewheight
      r$floorclip[r$rw_x + 1L] <- -1L
    } else {
      if (r$toptexture) {
        mid <- shar(r$pixhigh, HEIGHTBITS)
        r$pixhigh <- r$pixhigh + r$pixhighstep
        if (mid >= r$floorclip[r$rw_x + 1L]) mid <- r$floorclip[r$rw_x + 1L] - 1L
        if (mid >= yl) {
          r$dc_yl <- yl
          r$dc_yh <- mid
          r$dc_texturemid <- r$rw_toptexturemid
          r$dc_source <- fetch_column(r, r$toptexture, texturecolumn)
          draw_column(r)
          r$ceilingclip[r$rw_x + 1L] <- mid
        } else {
          r$ceilingclip[r$rw_x + 1L] <- yl - 1L
        }
      } else if (r$markceiling) {
        r$ceilingclip[r$rw_x + 1L] <- yl - 1L
      }
      if (r$bottomtexture) {
        mid <- shar(r$pixlow + HEIGHTUNIT - 1, HEIGHTBITS)
        r$pixlow <- r$pixlow + r$pixlowstep
        if (mid <= r$ceilingclip[r$rw_x + 1L]) mid <- r$ceilingclip[r$rw_x + 1L] + 1L
        if (mid <= yh) {
          r$dc_yl <- mid
          r$dc_yh <- yh
          r$dc_texturemid <- r$rw_bottomtexturemid
          r$dc_source <- fetch_column(r, r$bottomtexture, texturecolumn)
          draw_column(r)
          r$floorclip[r$rw_x + 1L] <- mid
        } else {
          r$floorclip[r$rw_x + 1L] <- yh + 1L
        }
      } else if (r$markfloor) {
        r$floorclip[r$rw_x + 1L] <- yh + 1L
      }
      if (r$maskedtexture && !is.null(r$maskedtexturecol)) {
        r$maskedtexturecol[r$rw_x - r$rw_start + 1L] <- texturecolumn
      }
    }
    r$rw_scale <- r$rw_scale + r$rw_scalestep
    r$topfrac <- r$topfrac + r$topstep
    r$bottomfrac <- r$bottomfrac + r$bottomstep
    r$rw_x <- r$rw_x + 1L
  }
}

paint_view <- function(r, xs, ys, val) {
  if (!length(ys)) return(invisible())
  sx <- bitwShiftL(as.integer(xs), r$detailshift)
  ok <- sx >= 0L & sx < length(r$columnofs)
  if (!any(ok)) return(invisible())
  dest <- r$ylookup[ys[ok] + 1L] + r$columnofs[sx[ok] + 1L] + 1L
  fit <- dest >= 1L & dest <= length(r$fb)
  if (any(fit)) r$fb[dest[fit]] <- val[ok][fit]
  if (r$detailshift) {
    ok2 <- ok & (sx + 1L < length(r$columnofs))
    if (any(ok2)) {
      dest2 <- r$ylookup[ys[ok2] + 1L] + r$columnofs[sx[ok2] + 2L] + 1L
      fit2 <- dest2 >= 1L & dest2 <= length(r$fb)
      if (any(fit2)) r$fb[dest2[fit2]] <- val[ok2][fit2]
    }
  }
  invisible()
}

draw_column <- function(r) {
  if (r$dc_yh < r$dc_yl || r$dc_x < 0L || r$dc_x >= r$viewwidth) return(invisible())
  yl <- max(0L, r$dc_yl)
  yh <- min(r$viewheight - 1L, r$dc_yh)
  if (yh < yl) return(invisible())
  ys <- seq.int(yl, yh)
  frac <- as_i32(r$dc_texturemid + (ys - r$centery) * r$dc_iscale)
  idx <- bitwAnd(as.integer(shar(frac, FRACBITS)), 127L)
  if (!length(r$dc_source)) return(invisible())
  idx <- pmin(length(r$dc_source) - 1L, pmax(0L, idx))
  pix <- as.integer(r$dc_source[idx + 1L])
  cm <- r$dc_colormap
  val <- as.raw(as.integer(cm[pmin(pix, length(cm) - 1L) + 1L]))
  paint_view(r, rep.int(r$dc_x, length(ys)), ys, val)
  invisible(NULL)
}

renderer_draw_masked <- function(r) {
  if (!length(r$drawsegs)) return(invisible())
  for (i in length(r$drawsegs):1) {
    ds <- r$drawsegs[[i]]
    if (!is.null(ds$maskedtexturecol)) render_masked_seg_range(r, ds, ds$x1, ds$x2)
  }
}

render_masked_seg_range <- function(r, ds, x1, x2) {
  if (is.null(ds$maskedtexturecol) || is.null(ds$curline)) return(invisible())
  line <- ds$curline
  front <- line$frontsector
  back <- line$backsector
  sidedef <- line$sidedef
  texnum <- if (is.null(sidedef)) 0L else sidedef$midtexture
  if (!texnum || is.null(back) || is.null(front)) return(invisible())
  lightnum <- (front$lightlevel %/% 16L) + r$extralight
  if (line$v1$y == line$v2$y) lightnum <- lightnum - 1L
  else if (line$v1$x == line$v2$x) lightnum <- lightnum + 1L
  lightnum <- max(0L, min(LIGHTLEVELS - 1L, lightnum))
  walllights <- r$scalelight[, lightnum + 1L]
  if (bitwAnd(line$linedef$flags, ML_DONTPEGBOTTOM) != 0L) {
    base <- if (front$floorheight > back$floorheight) front$floorheight else back$floorheight
    dc_texturemid <- base + texture_height(r$res, texnum) - r$viewz
  } else {
    base <- if (front$ceilingheight < back$ceilingheight) front$ceilingheight else back$ceilingheight
    dc_texturemid <- base - r$viewz
  }
  dc_texturemid <- dc_texturemid + sidedef$rowoffset
  spryscale <- ds$scale1 + (x1 - ds$x1) * ds$scalestep
  for (dc_x in x1:x2) {
    i <- dc_x - ds$x1 + 1L
    if (i < 1L || i > length(ds$maskedtexturecol)) {
      spryscale <- spryscale + ds$scalestep
      next
    }
    tcol <- ds$maskedtexturecol[i]
    if (tcol != SHRT_MAX) {
      index <- if (spryscale > 0) u32_shr(spryscale, LIGHTSCALESHIFT) else 0
      if (index >= MAXLIGHTSCALE) index <- MAXLIGHTSCALE - 1L
      r$dc_colormap <- if (!is.null(r$fixedcolormap)) r$fixedcolormap else colormap_bytes(r$res, walllights[index + 1L])
      r$dc_x <- dc_x
      r$dc_iscale <- if (spryscale) trunc(4294967295 / spryscale) else 0
      r$dc_texturemid <- dc_texturemid
      sprtopscreen <- r$centeryfrac - fixed_mul(dc_texturemid, spryscale)
      mceil <- if (i <= length(ds$sprtopclip)) ds$sprtopclip[i] else -1L
      mfloor <- if (i <= length(ds$sprbottomclip)) ds$sprbottomclip[i] else r$viewheight
      posts <- column_posts(r$res, texnum, tcol)
      for (post in posts) {
        if (!length(post$pixels)) next
        topscreen <- sprtopscreen + spryscale * post$top
        bottomscreen <- topscreen + spryscale * length(post$pixels)
        yl <- shar(topscreen + FRACUNIT - 1, FRACBITS)
        yh <- shar(bottomscreen - 1, FRACBITS)
        if (yh >= mfloor) yh <- mfloor - 1L
        if (yl <= mceil) yl <- mceil + 1L
        if (yl < 0L) yl <- 0L
        if (yh >= r$viewheight) yh <- r$viewheight - 1L
        if (yl <= yh) {
          src <- post$pixels
          if (length(src) < 128L) src <- c(src, raw(128L - length(src)))
          r$dc_yl <- yl
          r$dc_yh <- yh
          r$dc_source <- src[seq_len(128L)]
          r$dc_texturemid <- dc_texturemid - post$top * FRACUNIT
          draw_column(r)
        }
      }
      ds$maskedtexturecol[i] <- SHRT_MAX
    }
    spryscale <- spryscale + ds$scalestep
  }
}

# Mesma conta de fixed_mul, sem a lista do meio. a e b ja estao em 32 bits.
.span_mul <- function(a, b) {
  ah <- trunc(a / 65536)
  al <- a - ah * 65536
  bh <- trunc(b / 65536)
  bl <- b - bh * 65536
  n <- ah * bh * 65536 + ah * bl + al * bh + trunc(al * bl / 65536)
  n <- n %% MASK32
  n - (n >= 2147483648) * MASK32
}

draw_flat_plane <- function(r, pl) {
  light <- max(0L, min(LIGHTLEVELS - 1L, (pl$lightlevel %/% 16L) + r$extralight))
  planezlight <- r$zlight[, light + 1L]
  flat <- flat_pixels(r$res, pl$picnum)
  planeheight <- abs_fixed(pl$height - r$viewz)
  if (planeheight == 0) return(invisible())
  x0 <- max(0L, pl$minx)
  x1 <- min(r$viewwidth - 1L, pl$maxx)
  if (x0 > x1) return(invisible())
  xs <- x0:x1
  top <- pl$top[xs + 1L]
  bot <- pmin(pl$bottom[xs + 1L], r$viewheight - 1L)
  keep <- top <= bot & top != 255L
  if (!any(keep)) return(invisible())
  xs <- xs[keep]
  top <- top[keep]
  bot <- bot[keep]
  lens <- bot - top + 1L
  pix_x <- rep.int(xs, lens)
  pix_y <- sequence(lens, from = top)
  ang <- bitwAnd(
    as.integer(u32_shr(as_u32(r$viewangle + r$xtoviewangle[xs + 1L]), ANGLETOFINESHIFT)),
    FINEMASK
  )
  sinx <- finesine[bitwAnd(ang + FINEANGLES %/% 4L, FINEMASK) + 1L]
  siny <- finesine[ang + 1L]
  distance <- .span_mul(planeheight, r$yslope[pix_y + 1L])
  lengthv <- .span_mul(distance, r$distscale[pix_x + 1L])
  ds_x <- as_i32(r$viewx + .span_mul(rep.int(sinx, lens), lengthv))
  ds_y <- as_i32(-r$viewy - .span_mul(rep.int(siny, lens), lengthv))
  spot <- bitwOr(
    bitwAnd(as.integer(floor(ds_x / 65536)), 63L),
    bitwAnd(as.integer(floor(ds_y / 1024)), 4032L)
  )
  spot <- pmin(4095L, pmax(0L, spot))
  zi <- pmin(MAXLIGHTZ - 1L, as.integer(pmax(0, floor(abs(distance) / 2^LIGHTZSHIFT))))
  pix <- as.integer(flat[spot + 1L])
  if (!is.null(r$fixedcolormap)) {
    val <- as.raw(as.integer(r$fixedcolormap[pix + 1L]))
  } else {
    lev <- planezlight[zi + 1L]
    val <- as.raw(as.integer(r$res$colormaps[lev * 256L + pix + 1L]))
  }
  paint_view(r, pix_x, pix_y, val)
  invisible(NULL)
}

draw_planes <- function(r) {
  for (pl in r$visplanes) {
    if (pl$minx > pl$maxx) next
    if (pl$picnum == r$res$skyflatnum) {
      r$dc_iscale <- trunc(9 * FRACUNIT / 10)
      r$dc_colormap <- colormap_bytes(r$res, 0L)
      r$dc_texturemid <- 100 * FRACUNIT
      x0 <- max(0L, pl$minx)
      x1 <- min(r$viewwidth - 1L, pl$maxx)
      if (x0 <= x1) {
        for (x in x0:x1) {
          yl <- pl$top[x + 1L]
          yh <- pl$bottom[x + 1L]
          if (yl <= yh && yl < 255L) {
            ang <- u32_shr(as_u32(r$viewangle + r$xtoviewangle[x + 1L]), ANGLETOSKYSHIFT)
            r$dc_x <- x
            r$dc_yl <- yl
            r$dc_yh <- yh
            r$dc_source <- fetch_column(r, r$res$skytexture, ang)
            draw_column(r)
          }
        }
      }
      next
    }
    draw_flat_plane(r, pl)
  }
}

render_level_view <- function(game) {
  if (is.null(game$renderer)) game$renderer <- renderer_new(game$res)
  r <- game$renderer
  renderer_set_view_size(r, game$screen_size + 3L, game$detail_level)
  player <- game$player
  if (!is.null(player) && !is.null(player$mo)) {
    mo <- player$mo
    x <- mo$x
    y <- mo$y
    z <- player$viewz
    ang <- mo$angle
    light <- player$extralight
    cmap <- player$fixedcolormap
  } else {
    st <- player_start(game$world)
    if (is.null(st)) return(invisible())
    x <- st$x * FRACUNIT
    y <- st$y * FRACUNIT
    sec <- world_sector_at(game$world, x, y)
    z <- if (is.null(sec)) VIEWHEIGHT else sec$floorheight + VIEWHEIGHT
    ang <- as_u32(st$angle * ANG90 / 90)
    light <- 0L
    cmap <- 0L
  }
  fb <- raw(SCREENPIXELS)
  if (!isTRUE(game$view_ready)) {
    message("desenhando a vista...")
    game$view_ready <- TRUE
  }
  renderer_setup_frame(r, x, y, z, ang, light, cmap)
  renderer_render(r, game$world, fb)
  draw_sprites(r, game$world)
  renderer_draw_masked(r)
  if (!is.null(player)) draw_weapon(r, player, game$leveltime)
  game$base_fb <- r$fb
  game$view_blocks <- game$screen_size
  game$view_detail <- game$detail_level
  game$need_view <- FALSE
  game$menu$dirty <- TRUE
  invisible(game)
}
