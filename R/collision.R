# Colisao, uso e tiro (p_map / p_maputl).

has_flag <- function(flags, bit) {
  if (is.null(flags)) return(FALSE)
  bitwAnd(as.integer(flags), as.integer(bit)) != 0L
}

point_on_line_side <- function(x, y, line) {
  if (line$dx == 0) {
    if (x <= line$v1$x) return(if (line$dy > 0) 1L else 0L)
    return(if (line$dy < 0) 1L else 0L)
  }
  if (line$dy == 0) {
    if (y <= line$v1$y) return(if (line$dx < 0) 1L else 0L)
    return(if (line$dx > 0) 1L else 0L)
  }
  dx <- as_i32(x - line$v1$x)
  dy <- as_i32(y - line$v1$y)
  left <- fixed_mul(shar(line$dy, FRACBITS), dx)
  right <- fixed_mul(dy, shar(line$dx, FRACBITS))
  if (right < left) 0L else 1L
}

box_on_line_side <- function(bbox, line) {
  if (line$dx == 0) {
    p1 <- if (bbox[BOXRIGHT + 1L] < line$v1$x) 0L else 1L
    p2 <- if (bbox[BOXLEFT + 1L] < line$v1$x) 0L else 1L
    if (line$dy > 0) {
      p1 <- bitwXor(p1, 1L)
      p2 <- bitwXor(p2, 1L)
    }
  } else if (line$dy == 0) {
    p1 <- if (bbox[BOXTOP + 1L] > line$v1$y) 0L else 1L
    p2 <- if (bbox[BOXBOTTOM + 1L] > line$v1$y) 0L else 1L
    if (line$dx < 0) {
      p1 <- bitwXor(p1, 1L)
      p2 <- bitwXor(p2, 1L)
    }
  } else {
    same <- (line$dy > 0) == (line$dx > 0)
    if (same) {
      p1 <- point_on_line_side(bbox[BOXLEFT + 1L], bbox[BOXTOP + 1L], line)
      p2 <- point_on_line_side(bbox[BOXRIGHT + 1L], bbox[BOXBOTTOM + 1L], line)
    } else {
      p1 <- point_on_line_side(bbox[BOXRIGHT + 1L], bbox[BOXTOP + 1L], line)
      p2 <- point_on_line_side(bbox[BOXLEFT + 1L], bbox[BOXBOTTOM + 1L], line)
    }
  }
  if (p1 == p2) p1 else -1L
}

intercept_frac <- function(x1, y1, x2, y2, line) {
  ax <- x1 / FRACUNIT
  ay <- y1 / FRACUNIT
  bx <- x2 / FRACUNIT
  by <- y2 / FRACUNIT
  cx <- line$v1$x / FRACUNIT
  cy <- line$v1$y / FRACUNIT
  dx <- line$v2$x / FRACUNIT
  dy <- line$v2$y / FRACUNIT
  den <- (bx - ax) * (dy - cy) - (by - ay) * (dx - cx)
  if (abs(den) < 1e-8) return(NULL)
  t <- ((cx - ax) * (dy - cy) - (cy - ay) * (dx - cx)) / den
  u <- ((cx - ax) * (by - ay) - (cy - ay) * (bx - ax)) / den
  if (t < 0 || t > 1 || u < 0 || u > 1) return(NULL)
  t * FRACUNIT
}

check_sight <- function(world, t1, t2) {
  s1 <- point_in_subsector(world, t1$x, t1$y)$sector
  s2 <- point_in_subsector(world, t2$x, t2$y)$sector
  nsec <- length(world$sectors)
  rej <- world$rejectmatrix
  if (nsec && length(rej)) {
    pnum <- s1$i_sector * nsec + s2$i_sector
    bytenum <- bitwShiftR(as.integer(pnum), 3L)
    bitnum <- bitwShiftL(1L, bitwAnd(as.integer(pnum), 7L))
    if (bytenum >= 0L && bytenum < length(rej) && bitwAnd(as.integer(rej[bytenum + 1L]), bitnum) != 0L) return(FALSE)
  }
  if (identical(s1, s2)) return(TRUE)
  x1 <- t1$x
  y1 <- t1$y
  x2 <- t2$x
  y2 <- t2$y
  minx <- min(x1, x2)
  maxx <- max(x1, x2)
  miny <- min(y1, y2)
  maxy <- max(y1, y2)
  lim <- FRACUNIT / 64
  for (ln in world$lines) {
    bb <- ln$bbox
    if (bb[BOXRIGHT + 1L] < minx || bb[BOXLEFT + 1L] > maxx || bb[BOXTOP + 1L] < miny || bb[BOXBOTTOM + 1L] > maxy) next
    if (!is.null(ln$backsector)) {
      open <- line_opening(ln)
      if (open[1] - open[2] > 0) next
    }
    frac <- intercept_frac(x1, y1, x2, y2, ln)
    if (!is.null(frac) && frac > lim && frac < FRACUNIT - lim) return(FALSE)
  }
  TRUE
}

line_opening <- function(line) {
  if (is.null(line$backsector)) return(c(0, 0, 0))
  front <- line$frontsector
  back <- line$backsector
  opentop <- min(front$ceilingheight, back$ceilingheight)
  if (front$floorheight > back$floorheight) {
    c(opentop, front$floorheight, back$floorheight)
  } else {
    c(opentop, back$floorheight, front$floorheight)
  }
}

point_in_subsector <- function(world, x, y) {
  if (!length(world$nodes)) return(world$subsectors[[1]])
  nodenum <- length(world$nodes) - 1L
  repeat {
    if (nodenum < 0 || bitwAnd(as.integer(nodenum), NF_SUBSECTOR) != 0L) {
      idx <- if (nodenum < 0) 0L else bitwAnd(as.integer(nodenum), 32767L)
      return(world$subsectors[[idx + 1L]])
    }
    node <- world$nodes[[nodenum + 1L]]
    dx <- x - node$x
    dy <- y - node$y
    left <- trunc(node$dy / FRACUNIT) * dx
    right <- dy * trunc(node$dx / FRACUNIT)
    side <- if (right >= left) 1L else 0L
    nodenum <- node$children[side + 1L]
  }
}

unset_thing_position <- function(world, thing) {
  if (!isTRUE(thing$blocklinked)) return(invisible())
  nxt <- thing$bnext
  prev <- thing$bprev
  if (!is.null(nxt)) nxt$bprev <- prev
  if (!is.null(prev)) {
    prev$bnext <- nxt
  } else {
    i <- thing$bindex
    if (!is.null(i) && i >= 1L && i <= length(world$blocklinks) && identical(world$blocklinks[[i]], thing)) {
      world$blocklinks[i] <- list(nxt)
    }
  }
  thing$bnext <- NULL
  thing$bprev <- NULL
  thing$blocklinked <- FALSE
  invisible()
}

set_thing_position <- function(world, thing) {
  thing$bnext <- NULL
  thing$bprev <- NULL
  thing$blocklinked <- FALSE
  links <- world$blocklinks
  if (!length(links) || has_flag(thing$flags, 16L)) return(invisible())
  bx <- shar(thing$x - world$bmaporgx, MAPBLOCKSHIFT)
  by <- shar(thing$y - world$bmaporgy, MAPBLOCKSHIFT)
  if (bx < 0 || by < 0 || bx >= world$bmapwidth || by >= world$bmapheight) return(invisible())
  i <- by * world$bmapwidth + bx + 1
  if (length(i) != 1L || is.na(i) || i < 1 || i > length(links)) return(invisible())
  head <- links[[i]]
  thing$bnext <- head
  if (!is.null(head)) head$bprev <- thing
  world$blocklinks[[i]] <- thing
  thing$bindex <- i
  thing$blocklinked <- TRUE
  invisible()
}

block_things <- function(world, x, y, func) {
  x <- as.integer(x)[1]
  y <- as.integer(y)[1]
  if (is.na(x) || is.na(y) || x < 0L || y < 0L || x >= world$bmapwidth || y >= world$bmapheight) return(TRUE)
  i <- y * world$bmapwidth + x + 1L
  if (i < 1L || i > length(world$blocklinks)) return(TRUE)
  mo <- world$blocklinks[[i]]
  guard <- 0L
  while (!is.null(mo)) {
    guard <- guard + 1L
    if (guard > 4096L) return(TRUE)
    nxt <- mo$bnext
    if (!isTRUE(func(mo))) return(FALSE)
    mo <- nxt
  }
  TRUE
}

block_lines <- function(world, x, y, func) {
  x <- as.integer(x)[1]
  y <- as.integer(y)[1]
  if (is.na(x) || is.na(y) || x < 0L || y < 0L || x >= world$bmapwidth || y >= world$bmapheight) return(TRUE)
  i <- y * world$bmapwidth + x + 1L
  if (i < 1L || i > length(world$blockmap)) return(TRUE)
  lump <- world$blockmaplump
  offset <- world$blockmap[i]
  while (!is.na(offset) && offset >= 0 && offset < length(lump)) {
    n <- lump[offset + 1L]
    offset <- offset + 1
    if (n == 65535L) return(TRUE)
    if (n < 0L || n >= length(world$lines)) next
    ld <- world$lines[[n + 1L]]
    if (identical(ld$validcount, world$validcount)) next
    ld$validcount <- world$validcount
    if (!isTRUE(func(ld))) return(FALSE)
  }
  TRUE
}

pit_thing <- function(world, tm, other, game) {
  if (!has_flag(other$flags, MF_SOLID + MF_SPECIAL + MF_SHOOTABLE)) return(TRUE)
  blockdist <- other$radius + tm$radius
  if (abs(other$x - tm$tmx) >= blockdist || abs(other$y - tm$tmy) >= blockdist) return(TRUE)
  if (identical(other, tm)) return(TRUE)
  if (has_flag(tm$flags, MF_SKULLFLY)) {
    dmg <- ((p_random() %% 8) + 1) * (if (is.null(tm$damage)) 0 else tm$damage)
    if (!is.null(game)) game$damage_mobj(other, tm, dmg, tm)
    tm$flags <- bitwAnd(as.integer(tm$flags), bitwNot(MF_SKULLFLY))
    tm$momx <- 0
    tm$momy <- 0
    tm$momz <- 0
    if (!is.null(tm$type)) set_mobj_state(tm, MOBJINFO[[tm$type + 1L]]$spawnstate, world, game)
    return(FALSE)
  }
  if (has_flag(tm$flags, MF_MISSILE)) {
    if (tm$z > other$z + other$height || tm$z + tm$height < other$z) return(TRUE)
    if (!is.null(tm$target) && same_species(tm$target, other)) {
      if (identical(other, tm$target)) return(TRUE)
      if (is.null(other$type) || other$type != MT_PLAYER) return(FALSE)
    }
    if (!has_flag(other$flags, MF_SHOOTABLE)) return(!has_flag(other$flags, MF_SOLID))
    dmg <- ((p_random() %% 8) + 1) * (if (is.null(tm$damage)) 0 else tm$damage)
    src <- if (!is.null(tm$target)) tm$target else tm
    if (!is.null(game)) game$damage_mobj(other, src, dmg, tm)
    return(FALSE)
  }
  if (has_flag(other$flags, MF_SPECIAL)) {
    solid <- has_flag(other$flags, MF_SOLID)
    if (has_flag(tm$flags, MF_PICKUP) && !is.null(game)) game$touch_special(other, tm)
    return(!solid)
  }
  !has_flag(other$flags, MF_SOLID)
}

pit_line <- function(tm, chk, ld) {
  bbox <- chk$bbox
  if (bbox[BOXRIGHT + 1L] <= ld$bbox[BOXLEFT + 1L] ||
      bbox[BOXLEFT + 1L] >= ld$bbox[BOXRIGHT + 1L] ||
      bbox[BOXTOP + 1L] <= ld$bbox[BOXBOTTOM + 1L] ||
      bbox[BOXBOTTOM + 1L] >= ld$bbox[BOXTOP + 1L]) return(TRUE)
  if (box_on_line_side(bbox, ld) != -1L) return(TRUE)
  if (is.null(ld$backsector)) return(FALSE)
  if (!has_flag(tm$flags, MF_MISSILE)) {
    if (has_flag(ld$flags, ML_BLOCKING)) return(FALSE)
    if (is.null(tm$player) && has_flag(ld$flags, 2L)) return(FALSE)
  }
  open <- line_opening(ld)
  if (open[1] < chk$ceilingz) {
    chk$ceilingz <- open[1]
    chk$ceilingline <- ld
  }
  if (open[2] > chk$floorz) chk$floorz <- open[2]
  if (open[3] < chk$dropoffz) chk$dropoffz <- open[3]
  if (ld$special) chk$spechit[[length(chk$spechit) + 1L]] <- ld
  TRUE
}

check_position <- function(world, thing, x, y, game = NULL) {
  chk <- new.env(parent = emptyenv())
  thing$tmx <- x
  thing$tmy <- y
  radius <- thing$radius
  chk$bbox <- c(x - radius, x + radius, y - radius, y + radius)
  sub <- point_in_subsector(world, x, y)
  chk$floorz <- sub$sector$floorheight
  chk$dropoffz <- chk$floorz
  chk$ceilingz <- sub$sector$ceilingheight
  chk$spechit <- list()
  chk$blocked <- FALSE
  chk$ceilingline <- NULL
  world$validcount <- world$validcount + 1
  if (has_flag(thing$flags, MF_NOCLIP) || !length(world$blocklinks)) return(chk)
  orgx <- world$bmaporgx
  orgy <- world$bmaporgy
  xl <- shar(chk$bbox[BOXLEFT + 1L] - orgx - MAXRADIUS, MAPBLOCKSHIFT)
  xh <- shar(chk$bbox[BOXRIGHT + 1L] - orgx + MAXRADIUS, MAPBLOCKSHIFT)
  yl <- shar(chk$bbox[BOXBOTTOM + 1L] - orgy - MAXRADIUS, MAPBLOCKSHIFT)
  yh <- shar(chk$bbox[BOXTOP + 1L] - orgy + MAXRADIUS, MAPBLOCKSHIFT)
  if (xl <= xh && yl <= yh) {
    for (bx in xl:xh) {
      for (by in yl:yh) {
        if (!block_things(world, bx, by, function(th) pit_thing(world, thing, th, game))) {
          chk$blocked <- TRUE
          return(chk)
        }
      }
    }
  }
  xl <- shar(chk$bbox[BOXLEFT + 1L] - orgx, MAPBLOCKSHIFT)
  xh <- shar(chk$bbox[BOXRIGHT + 1L] - orgx, MAPBLOCKSHIFT)
  yl <- shar(chk$bbox[BOXBOTTOM + 1L] - orgy, MAPBLOCKSHIFT)
  yh <- shar(chk$bbox[BOXTOP + 1L] - orgy, MAPBLOCKSHIFT)
  if (xl <= xh && yl <= yh) {
    for (bx in xl:xh) {
      for (by in yl:yh) {
        if (!block_lines(world, bx, by, function(ld) pit_line(thing, chk, ld))) {
          chk$blocked <- TRUE
          return(chk)
        }
      }
    }
  }
  chk
}

try_move <- function(world, thing, x, y, game = NULL) {
  world$floatok <- FALSE
  chk <- check_position(world, thing, x, y, game)
  world$last_spechit <- chk$spechit
  world$tmfloorz <- chk$floorz
  world$ceilingline <- chk$ceilingline
  if (chk$blocked) return(FALSE)
  if (!has_flag(thing$flags, MF_NOCLIP)) {
    if (chk$ceilingz - chk$floorz < thing$height) return(FALSE)
    world$floatok <- TRUE
    if (chk$ceilingz - thing$z < thing$height) return(FALSE)
    if (chk$floorz - thing$z > MAXSTEP) return(FALSE)
    if (!has_flag(thing$flags, MF_DROPOFF + MF_FLOAT) && chk$floorz - chk$dropoffz > MAXSTEP) return(FALSE)
  }
  unset_thing_position(world, thing)
  oldx <- thing$x
  oldy <- thing$y
  thing$floorz <- chk$floorz
  thing$ceilingz <- chk$ceilingz
  thing$x <- x
  thing$y <- y
  set_thing_position(world, thing)
  if (!is.null(game) && !has_flag(thing$flags, MF_NOCLIP)) {
    hits <- chk$spechit
    if (length(hits)) {
      for (i in length(hits):1) {
        ln <- hits[[i]]
        if (!ln$special) next
        side <- point_on_line_side(thing$x, thing$y, ln)
        oldside <- point_on_line_side(oldx, oldy, ln)
        if (side != oldside) game$cross_special(ln, oldside, thing)
      }
    }
  }
  TRUE
}

thing_height_clip <- function(world, thing) {
  on_floor <- thing$z == thing$floorz
  chk <- check_position(world, thing, thing$x, thing$y)
  thing$floorz <- chk$floorz
  thing$ceilingz <- chk$ceilingz
  if (on_floor) thing$z <- thing$floorz
  else if (thing$z + thing$height > thing$ceilingz) thing$z <- thing$ceilingz - thing$height
  if (!is.null(thing$player)) thing$player$viewz <- thing$z + thing$player$viewheight
  thing$ceilingz - thing$floorz >= thing$height
}

change_sector <- function(world, sector, crush) {
  nofit <- FALSE
  for (thing in world$mobjs) {
    sub <- point_in_subsector(world, thing$x, thing$y)
    if (is.null(sub$sector) || !identical(sub$sector, sector)) next
    if (thing_height_clip(world, thing)) next
    if (!is.null(thing$health) && thing$health <= 0) {
      thing$flags <- bitwAnd(as.integer(thing$flags), bitwNot(MF_SOLID))
      thing$height <- 0
      next
    }
    if (!has_flag(thing$flags, MF_SHOOTABLE)) next
    nofit <- TRUE
    if (crush) {
      thing$health <- thing$health - 10
      if (!is.null(thing$player)) thing$player$health <- thing$health
      if (thing$health <= 0) {
        thing$flags <- bitwAnd(as.integer(thing$flags), bitwNot(MF_SOLID))
        thing$height <- 0
      }
    }
  }
  nofit
}

approx_distance <- function(dx, dy) {
  dx <- abs(dx)
  dy <- abs(dy)
  if (dx < dy) {
    big <- dy
    small <- dx
  } else {
    big <- dx
    small <- dy
  }
  big + shar(small, 1L)
}

angle_to <- function(x1, y1, x2, y2) {
  x <- x2 - x1
  y <- y2 - y1
  if (x == 0 && y == 0) return(0)
  if (x >= 0) {
    if (y >= 0) {
      if (x > y) return(as_u32(tantoangle[slope_div(y, x) + 1L]))
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

bit31 <- function(n) bitwAnd(as.integer(floor(as_u32(n) / 65536)), 32768L) != 0L

point_on_divline_side <- function(x, y, line) {
  if (line$dx == 0) {
    if (x <= line$x) return(if (line$dy > 0) 1L else 0L)
    return(if (line$dy < 0) 1L else 0L)
  }
  if (line$dy == 0) {
    if (y <= line$y) return(if (line$dx < 0) 1L else 0L)
    return(if (line$dx > 0) 1L else 0L)
  }
  dx <- x - line$x
  dy <- y - line$y
  signxor <- xor(xor(bit31(line$dy), bit31(line$dx)), xor(bit31(dx), bit31(dy)))
  if (signxor) return(if (xor(bit31(line$dy), bit31(dx))) 1L else 0L)
  left <- fixed_mul(shar(line$dy, 8L), shar(dx, 8L))
  right <- fixed_mul(shar(dy, 8L), shar(line$dx, 8L))
  if (right < left) 0L else 1L
}

intercept_vector <- function(v2, v1) {
  den <- as_i32(fixed_mul(shar(v1$dy, 8L), v2$dx) - fixed_mul(shar(v1$dx, 8L), v2$dy))
  if (den == 0) return(0)
  num <- as_i32(fixed_mul(shar(v1$x - v2$x, 8L), v1$dy) + fixed_mul(shar(v2$y - v1$y, 8L), v1$dx))
  fixed_div(num, den)
}

path_traverse <- function(world, x1, y1, x2, y2, flags, trav) {
  st <- new.env(parent = emptyenv())
  st$early <- bitwAnd(flags, PT_EARLYOUT) != 0L
  st$intercepts <- list()
  st$x <- x1
  st$y <- y1
  st$dx <- as_i32(x2 - x1)
  st$dy <- as_i32(y2 - y1)
  world$validcount <- world$validcount + 1
  add_line <- function(ld) {
    big <- 16 * FRACUNIT
    dx <- st$dx
    dy <- st$dy
    if (dx > big || dy > big || dx < -big || dy < -big) {
      tr <- list(x = st$x, y = st$y, dx = dx, dy = dy)
      s1 <- point_on_divline_side(ld$v1$x, ld$v1$y, tr)
      s2 <- point_on_divline_side(ld$v2$x, ld$v2$y, tr)
    } else {
      s1 <- point_on_line_side(st$x, st$y, ld)
      s2 <- point_on_line_side(st$x + dx, st$y + dy, ld)
    }
    if (s1 == s2) return(TRUE)
    frac <- intercept_vector(list(x = st$x, y = st$y, dx = dx, dy = dy), list(x = ld$v1$x, y = ld$v1$y, dx = ld$dx, dy = ld$dy))
    if (frac < 0) return(TRUE)
    if (st$early && frac < FRACUNIT && is.null(ld$backsector)) return(FALSE)
    st$intercepts[[length(st$intercepts) + 1L]] <- list(frac = frac, isaline = TRUE, line = ld, thing = NULL)
    TRUE
  }
  add_thing <- function(thing) {
    tr <- list(x = st$x, y = st$y, dx = st$dx, dy = st$dy)
    xh <- bitwXor(as.integer(floor(as_u32(tr$dx) / 65536) %% 65536), as.integer(floor(as_u32(tr$dy) / 65536) %% 65536))
    xl <- bitwXor(as.integer(as_u32(tr$dx) %% 65536), as.integer(as_u32(tr$dy) %% 65536))
    positive <- bitwAnd(xh, 32768L) == 0L && (xh != 0L || xl != 0L)
    if (positive) {
      x1t <- thing$x - thing$radius
      y1t <- thing$y + thing$radius
      x2t <- thing$x + thing$radius
      y2t <- thing$y - thing$radius
    } else {
      x1t <- thing$x - thing$radius
      y1t <- thing$y - thing$radius
      x2t <- thing$x + thing$radius
      y2t <- thing$y + thing$radius
    }
    if (point_on_divline_side(x1t, y1t, tr) == point_on_divline_side(x2t, y2t, tr)) return(TRUE)
    frac <- intercept_vector(tr, list(x = x1t, y = y1t, dx = x2t - x1t, dy = y2t - y1t))
    if (frac < 0) return(TRUE)
    st$intercepts[[length(st$intercepts) + 1L]] <- list(frac = frac, isaline = FALSE, line = NULL, thing = thing)
    TRUE
  }
  orgx <- world$bmaporgx
  orgy <- world$bmaporgy
  if (as_i32(x1 - orgx) %% MAPBLOCKSIZE == 0) x1 <- x1 + FRACUNIT
  if (as_i32(y1 - orgy) %% MAPBLOCKSIZE == 0) y1 <- y1 + FRACUNIT
  st$x <- x1
  st$y <- y1
  st$dx <- as_i32(x2 - x1)
  st$dy <- as_i32(y2 - y1)
  x1m <- as_i32(x1 - orgx)
  y1m <- as_i32(y1 - orgy)
  xt1 <- shar(x1m, MAPBLOCKSHIFT)
  yt1 <- shar(y1m, MAPBLOCKSHIFT)
  x2m <- as_i32(x2 - orgx)
  y2m <- as_i32(y2 - orgy)
  xt2 <- shar(x2m, MAPBLOCKSHIFT)
  yt2 <- shar(y2m, MAPBLOCKSHIFT)
  if (xt2 > xt1) {
    mapxstep <- 1
    partial <- FRACUNIT - bitwAnd(as.integer(shar(x1m, MAPBTOFRAC)), FRACUNIT - 1)
    ystep <- fixed_div(as_i32(y2m - y1m), abs(as_i32(x2m - x1m)))
  } else if (xt2 < xt1) {
    mapxstep <- -1
    partial <- bitwAnd(as.integer(shar(x1m, MAPBTOFRAC)), FRACUNIT - 1)
    ystep <- fixed_div(as_i32(y2m - y1m), abs(as_i32(x2m - x1m)))
  } else {
    mapxstep <- 0
    partial <- FRACUNIT
    ystep <- 256 * FRACUNIT
  }
  yintercept <- as_i32(shar(y1m, MAPBTOFRAC) + fixed_mul(partial, ystep))
  if (yt2 > yt1) {
    mapystep <- 1
    partial <- FRACUNIT - bitwAnd(as.integer(shar(y1m, MAPBTOFRAC)), FRACUNIT - 1)
    xstep <- fixed_div(as_i32(x2m - x1m), abs(as_i32(y2m - y1m)))
  } else if (yt2 < yt1) {
    mapystep <- -1
    partial <- bitwAnd(as.integer(shar(y1m, MAPBTOFRAC)), FRACUNIT - 1)
    xstep <- fixed_div(as_i32(x2m - x1m), abs(as_i32(y2m - y1m)))
  } else {
    mapystep <- 0
    partial <- FRACUNIT
    xstep <- 256 * FRACUNIT
  }
  xintercept <- as_i32(shar(x1m, MAPBTOFRAC) + fixed_mul(partial, xstep))
  mapx <- xt1
  mapy <- yt1
  for (count in seq_len(64L)) {
    if (bitwAnd(flags, PT_ADDLINES) != 0L && !block_lines(world, mapx, mapy, add_line)) return(FALSE)
    if (bitwAnd(flags, PT_ADDTHINGS) != 0L && !block_things(world, mapx, mapy, add_thing)) return(FALSE)
    if (mapx == xt2 && mapy == yt2) break
    if (shar(yintercept, FRACBITS) == mapy) {
      yintercept <- as_i32(yintercept + ystep)
      mapx <- mapx + mapxstep
    } else if (shar(xintercept, FRACBITS) == mapx) {
      xintercept <- as_i32(xintercept + xstep)
      mapy <- mapy + mapystep
    } else {
      break
    }
  }
  count <- length(st$intercepts)
  while (count > 0) {
    count <- count - 1L
    dist <- INT_MAX
    chosen <- 0L
    for (i in seq_along(st$intercepts)) {
      if (st$intercepts[[i]]$frac < dist) {
        dist <- st$intercepts[[i]]$frac
        chosen <- i
      }
    }
    if (dist > FRACUNIT) return(TRUE)
    if (!chosen || !isTRUE(trav(st$intercepts[[chosen]], st))) return(FALSE)
    st$intercepts[[chosen]]$frac <- INT_MAX
  }
  TRUE
}

slide_move <- function(world, thing, momx, momy, game = NULL) {
  if (abs(momx) > MAXMOVE) momx <- if (momx > 0) MAXMOVE else -MAXMOVE
  if (abs(momy) > MAXMOVE) momy <- if (momy > 0) MAXMOVE else -MAXMOVE
  thing$momx <- momx
  thing$momy <- momy
  hitcount <- 0L
  repeat {
    hitcount <- hitcount + 1L
    if (hitcount == 3L) {
      if (!try_move(world, thing, thing$x, thing$y + thing$momy, game)) {
        try_move(world, thing, thing$x + thing$momx, thing$y, game)
      }
      return(invisible())
    }
    if (thing$momx > 0) {
      leadx <- thing$x + thing$radius
      trailx <- thing$x - thing$radius
    } else {
      leadx <- thing$x - thing$radius
      trailx <- thing$x + thing$radius
    }
    if (thing$momy > 0) {
      leady <- thing$y + thing$radius
      traily <- thing$y - thing$radius
    } else {
      leady <- thing$y - thing$radius
      traily <- thing$y + thing$radius
    }
    best <- new.env(parent = emptyenv())
    best$frac <- FRACUNIT + 1
    best$line <- NULL
    slide_trav <- function(inn, st) {
      li <- inn$line
      blocking <- FALSE
      if (!has_flag(li$flags, ML_TWOSIDED)) {
        if (point_on_line_side(thing$x, thing$y, li) != 0L) return(TRUE)
        blocking <- TRUE
      } else {
        open <- line_opening(li)
        if (open[1] - open[2] < thing$height) blocking <- TRUE
        else if (open[1] - thing$z < thing$height) blocking <- TRUE
        else if (open[2] - thing$z > 24 * FRACUNIT) blocking <- TRUE
      }
      if (!blocking) return(TRUE)
      if (inn$frac < best$frac) {
        best$frac <- inn$frac
        best$line <- li
      }
      FALSE
    }
    mx <- thing$momx
    my <- thing$momy
    path_traverse(world, leadx, leady, leadx + mx, leady + my, PT_ADDLINES, slide_trav)
    path_traverse(world, trailx, leady, trailx + mx, leady + my, PT_ADDLINES, slide_trav)
    path_traverse(world, leadx, traily, leadx + mx, traily + my, PT_ADDLINES, slide_trav)
    if (best$frac == FRACUNIT + 1 || is.null(best$line)) {
      if (!try_move(world, thing, thing$x, thing$y + thing$momy, game)) {
        try_move(world, thing, thing$x + thing$momx, thing$y, game)
      }
      return(invisible())
    }
    best$frac <- best$frac - 2048
    if (best$frac > 0) {
      newx <- fixed_mul(thing$momx, best$frac)
      newy <- fixed_mul(thing$momy, best$frac)
      if (!try_move(world, thing, thing$x + newx, thing$y + newy, game)) {
        if (!try_move(world, thing, thing$x, thing$y + thing$momy, game)) {
          try_move(world, thing, thing$x + thing$momx, thing$y, game)
        }
        return(invisible())
      }
    }
    best$frac <- FRACUNIT - (best$frac + 2048)
    if (best$frac > FRACUNIT) best$frac <- FRACUNIT
    if (best$frac <= 0) return(invisible())
    tmx <- fixed_mul(thing$momx, best$frac)
    tmy <- fixed_mul(thing$momy, best$frac)
    line <- best$line
    if (line$dy == 0) {
      tmy <- 0
    } else if (line$dx == 0) {
      tmx <- 0
    } else {
      side <- point_on_line_side(thing$x, thing$y, line)
      lineangle <- angle_to(0, 0, line$dx, line$dy)
      if (side == 1L) lineangle <- as_u32(lineangle + ANG180)
      moveangle <- angle_to(0, 0, tmx, tmy)
      delta <- as_u32(moveangle - lineangle)
      if (delta > ANG180) delta <- as_u32(delta + ANG180)
      newlen <- fixed_mul(approx_distance(tmx, tmy), fine_cos(delta))
      tmx <- fixed_mul(newlen, fine_cos(lineangle))
      tmy <- fixed_mul(newlen, fine_sin(lineangle))
    }
    thing$momx <- tmx
    thing$momy <- tmy
    if (try_move(world, thing, thing$x + tmx, thing$y + tmy, game)) return(invisible())
  }
}

use_lines <- function(world, player, game) {
  mo <- player$mo
  x2 <- mo$x + shar(USERANGE, FRACBITS) * fine_cos(mo$angle)
  y2 <- mo$y + shar(USERANGE, FRACBITS) * fine_sin(mo$angle)
  path_traverse(world, mo$x, mo$y, x2, y2, PT_ADDLINES, function(inn, st) {
    ln <- inn$line
    if (!ln$special) {
      open <- line_opening(ln)
      if (open[1] - open[2] <= 0) {
        if (!is.null(game)) game$start_sound("noway")
        return(FALSE)
      }
      return(TRUE)
    }
    side <- if (point_on_line_side(mo$x, mo$y, ln) == 1L) 1L else 0L
    if (!is.null(game)) game$use_special(ln, mo, side)
    FALSE
  })
  invisible()
}

shot_ends <- function(source, angle, attackrange) {
  c(
    source$x + shar(attackrange, FRACBITS) * fine_cos(angle),
    source$y + shar(attackrange, FRACBITS) * fine_sin(angle),
    source$z + shar(source$height, 1L) + 8 * FRACUNIT
  )
}

aim_shot <- function(world, source, angle, attackrange) {
  ends <- shot_ends(source, angle, attackrange)
  shootz <- ends[3]
  window <- (100 * FRACUNIT) %/% 160
  state <- new.env(parent = emptyenv())
  state$top <- window
  state$bottom <- -window
  state$slope <- 0
  state$target <- NULL
  path_traverse(world, source$x, source$y, ends[1], ends[2], PT_ADDLINES + PT_ADDTHINGS, function(inn, st) {
    if (inn$isaline) {
      li <- inn$line
      if (!has_flag(li$flags, ML_TWOSIDED)) return(FALSE)
      open <- line_opening(li)
      if (open[2] >= open[1]) return(FALSE)
      dist <- fixed_mul(attackrange, inn$frac)
      front <- li$frontsector
      back <- li$backsector
      if (is.null(back) || front$floorheight != back$floorheight) {
        slope <- fixed_div(open[2] - shootz, dist)
        if (slope > state$bottom) state$bottom <- slope
      }
      if (is.null(back) || front$ceilingheight != back$ceilingheight) {
        slope <- fixed_div(open[1] - shootz, dist)
        if (slope < state$top) state$top <- slope
      }
      return(state$top > state$bottom)
    }
    th <- inn$thing
    if (identical(th, source) || !has_flag(th$flags, MF_SHOOTABLE)) return(TRUE)
    dist <- fixed_mul(attackrange, inn$frac)
    thingtop <- fixed_div(th$z + th$height - shootz, dist)
    if (thingtop < state$bottom) return(TRUE)
    thingbot <- fixed_div(th$z - shootz, dist)
    if (thingbot > state$top) return(TRUE)
    if (thingtop > state$top) thingtop <- state$top
    if (thingbot < state$bottom) thingbot <- state$bottom
    state$slope <- (thingtop + thingbot) / 2
    state$target <- th
    FALSE
  })
  list(slope = state$slope, target = state$target)
}

aim_slope <- function(world, source, angle, attackrange) {
  aim_shot(world, source, angle, attackrange)$slope
}

bullet_aim <- function(world, source, span = 16 * 64 * FRACUNIT) {
  base <- source$angle
  for (ang in c(base, as_u32(base + 67108864), as_u32(base - 67108864))) {
    hit <- aim_shot(world, source, ang, span)
    if (!is.null(hit$target)) return(list(slope = hit$slope, angle = ang, target = hit$target))
  }
  list(slope = 0, angle = base, target = NULL)
}

bullet_slope <- function(world, source) bullet_aim(world, source)$slope

spawn_fx <- function(world, x, y, z, sprite) {
  mo <- new.env(parent = emptyenv())
  mo$x <- x
  mo$y <- y
  mo$z <- z
  mo$angle <- 0
  mo$sprite <- sprite
  mo$frame <- 0L
  mo$flags <- 0
  mo$radius <- FRACUNIT
  mo$height <- FRACUNIT
  mo$momx <- 0
  mo$momy <- 0
  mo$momz <- FRACUNIT
  mo$floorz <- z
  mo$ceilingz <- z + 128 * FRACUNIT
  mo$player <- NULL
  mo$health <- 1
  mo$tics <- 12L
  mo$fx <- TRUE
  mo$blocklinked <- FALSE
  world$mobjs[[length(world$mobjs) + 1L]] <- mo
  mo
}

line_attack <- function(world, source, damage, game, attackrange, angle = NULL, slope = NULL) {
  ang <- if (is.null(angle)) source$angle else angle
  aimslope <- if (is.null(slope)) aim_shot(world, source, ang, attackrange)$slope else slope
  ends <- shot_ends(source, ang, attackrange)
  shootz <- ends[3]
  hit <- FALSE
  path_traverse(world, source$x, source$y, ends[1], ends[2], PT_ADDLINES + PT_ADDTHINGS, function(inn, st) {
    if (inn$isaline) {
      li <- inn$line
      if (li$special && !is.null(game)) game$shoot_special(li, source)
      hit_line <- FALSE
      if (!has_flag(li$flags, ML_TWOSIDED)) {
        hit_line <- TRUE
      } else {
        open <- line_opening(li)
        dist <- fixed_mul(attackrange, inn$frac)
        front <- li$frontsector
        back <- li$backsector
        if (is.null(back)) {
          if (fixed_div(open[2] - shootz, dist) > aimslope) hit_line <- TRUE
          else if (fixed_div(open[1] - shootz, dist) < aimslope) hit_line <- TRUE
        } else {
          if (front$floorheight != back$floorheight && fixed_div(open[2] - shootz, dist) > aimslope) hit_line <- TRUE
          if (!hit_line && front$ceilingheight != back$ceilingheight && fixed_div(open[1] - shootz, dist) < aimslope) {
            hit_line <- TRUE
          }
        }
      }
      if (!hit_line) return(TRUE)
      frac <- inn$frac - fixed_div(4 * FRACUNIT, attackrange)
      x <- st$x + fixed_mul(st$dx, frac)
      y <- st$y + fixed_mul(st$dy, frac)
      z <- shootz + fixed_mul(aimslope, fixed_mul(frac, attackrange))
      spawn_fx(world, x, y, z, "PUFF")
      FALSE
    } else {
      th <- inn$thing
      if (identical(th, source) || !has_flag(th$flags, MF_SHOOTABLE)) return(TRUE)
      dist <- fixed_mul(attackrange, inn$frac)
      if (fixed_div(th$z + th$height - shootz, dist) < aimslope) return(TRUE)
      if (fixed_div(th$z - shootz, dist) > aimslope) return(TRUE)
      frac <- inn$frac - fixed_div(10 * FRACUNIT, attackrange)
      x <- st$x + fixed_mul(st$dx, frac)
      y <- st$y + fixed_mul(st$dy, frac)
      z <- shootz + fixed_mul(aimslope, fixed_mul(frac, attackrange))
      spawn_fx(world, x, y, z, "BLUD")
      if (!is.null(game) && damage) game$damage_mobj(th, source, damage)
      hit <<- TRUE
      FALSE
    }
  })
  hit
}
