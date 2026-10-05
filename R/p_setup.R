# Carga do mapa (p_setup): geometria, BSP, things, blockmap e reject.
# Nomes de textura ficam em texto ate o passo das texturas.

u16_vec <- function(buf) {
  n <- length(buf) %/% 2L
  if (!n) return(integer())
  lo <- as.integer(buf[seq.int(1L, by = 2L, length.out = n)])
  hi <- as.integer(buf[seq.int(2L, by = 2L, length.out = n)])
  lo + hi * 256L
}

i16_vec <- function(buf) {
  v <- u16_vec(buf)
  v[v >= 32768L] <- v[v >= 32768L] - 65536L
  v
}

lump_name_at <- function(buf, off) name8(buf[off + seq_len(8L)])

world_map_name <- function(wad, episode, mapn) {
  doom2 <- sprintf("MAP%02d", as.integer(mapn))
  if (wad_check_num(wad, doom2) >= 0L) doom2 else sprintf("E%dM%d", as.integer(episode), as.integer(mapn))
}

world_new <- function() {
  e <- new.env(parent = emptyenv())
  e$name <- ""
  e$vertexes <- list()
  e$sectors <- list()
  e$sides <- list()
  e$lines <- list()
  e$segs <- list()
  e$subsectors <- list()
  e$nodes <- list()
  e$things <- list()
  e$numnodes <- 0L
  e$blockmap <- integer()
  e$blockmaplump <- integer()
  e$bmaporgx <- 0
  e$bmaporgy <- 0
  e$bmapwidth <- 0L
  e$bmapheight <- 0L
  e$rejectmatrix <- raw()
  e
}

world_setup <- function(wad, episode, mapn) {
  world <- world_new()
  world$name <- world_map_name(wad, episode, mapn)
  base <- wad_num_for_name(wad, world$name)
  world$vertexes <- load_vertexes(wad_cache_num(wad, base + 4L))
  world$sectors <- load_sectors(wad_cache_num(wad, base + 8L))
  world$sides <- load_sides(wad_cache_num(wad, base + 3L), world$sectors)
  world$lines <- load_lines(wad_cache_num(wad, base + 2L), world$vertexes, world$sides)
  world$segs <- load_segs(wad_cache_num(wad, base + 5L), world$vertexes, world$lines)
  world$subsectors <- load_subsectors(wad_cache_num(wad, base + 6L))
  world$nodes <- load_nodes(wad_cache_num(wad, base + 7L))
  world$things <- load_things(wad_cache_num(wad, base + 1L))
  load_blockmap(world, wad_cache_num(wad, base + 10L))
  world$rejectmatrix <- wad_cache_num(wad, base + 9L)
  world$numnodes <- length(world$nodes)
  for (ss in world$subsectors) {
    ss$sector <- world$segs[[ss$firstline + 1L]]$frontsector
  }
  world
}

load_vertexes <- function(data) {
  n <- length(data) %/% MAPVERTEX_SIZE
  xy <- i16_vec(data)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    e <- new.env(parent = emptyenv())
    e$x <- as.numeric(xy[i * 2L - 1L]) * FRACUNIT
    e$y <- as.numeric(xy[i * 2L]) * FRACUNIT
    out[[i]] <- e
  }
  out
}

load_sectors <- function(data) {
  n <- length(data) %/% MAPSECTOR_SIZE
  out <- vector("list", n)
  for (i in seq_len(n)) {
    o <- (i - 1L) * MAPSECTOR_SIZE
    e <- new.env(parent = emptyenv())
    e$floorheight <- as.numeric(i16_at(data, o)) * FRACUNIT
    e$ceilingheight <- as.numeric(i16_at(data, o + 2L)) * FRACUNIT
    e$floorflat <- lump_name_at(data, o + 4L)
    e$ceilingflat <- lump_name_at(data, o + 12L)
    e$lightlevel <- i16_at(data, o + 20L)
    e$special <- i16_at(data, o + 22L)
    e$tag <- i16_at(data, o + 24L)
    e$i_sector <- i - 1L
    e$lines <- list()
    e$specialdata <- NULL
    out[[i]] <- e
  }
  out
}

load_sides <- function(data, sectors) {
  n <- length(data) %/% MAPSIDEDEF_SIZE
  fallback <- if (length(sectors)) sectors[[1]] else NULL
  out <- vector("list", n)
  for (i in seq_len(n)) {
    o <- (i - 1L) * MAPSIDEDEF_SIZE
    e <- new.env(parent = emptyenv())
    e$textureoffset <- as.numeric(i16_at(data, o)) * FRACUNIT
    e$rowoffset <- as.numeric(i16_at(data, o + 2L)) * FRACUNIT
    e$topname <- lump_name_at(data, o + 4L)
    e$bottomname <- lump_name_at(data, o + 12L)
    e$midname <- lump_name_at(data, o + 20L)
    sec <- i16_at(data, o + 28L)
    e$sector <- if (sec >= 0L && sec < length(sectors)) sectors[[sec + 1L]] else fallback
    out[[i]] <- e
  }
  out
}

load_lines <- function(data, vertexes, sides) {
  n <- length(data) %/% MAPLINEDEF_SIZE
  out <- vector("list", n)
  for (i in seq_len(n)) {
    o <- (i - 1L) * MAPLINEDEF_SIZE
    vals <- i16_vec(data[o + seq_len(MAPLINEDEF_SIZE)])
    e <- new.env(parent = emptyenv())
    e$v1 <- vertexes[[vals[1] + 1L]]
    e$v2 <- vertexes[[vals[2] + 1L]]
    e$dx <- e$v2$x - e$v1$x
    e$dy <- e$v2$y - e$v1$y
    e$flags <- vals[3]
    e$special <- vals[4]
    e$tag <- vals[5]
    e$sidenum <- c(vals[6], vals[7])
    s0 <- vals[6]
    s1 <- vals[7]
    e$sides <- list(
      if (s0 >= 0L) sides[[s0 + 1L]] else NULL,
      if (s1 >= 0L) sides[[s1 + 1L]] else NULL
    )
    e$frontsector <- if (is.null(e$sides[[1]])) NULL else e$sides[[1]]$sector
    e$backsector <- if (is.null(e$sides[[2]])) NULL else e$sides[[2]]$sector
    if (e$v1$x < e$v2$x) {
      left <- e$v1$x
      right <- e$v2$x
    } else {
      left <- e$v2$x
      right <- e$v1$x
    }
    if (e$v1$y < e$v2$y) {
      bottom <- e$v1$y
      top <- e$v2$y
    } else {
      bottom <- e$v2$y
      top <- e$v1$y
    }
    e$bbox <- c(left, right, bottom, top)
    e$i_line <- i - 1L
    e$validcount <- 0L
    if (!is.null(e$frontsector)) e$frontsector$lines <- c(e$frontsector$lines, list(e))
    if (!is.null(e$backsector) && !identical(e$backsector, e$frontsector)) {
      e$backsector$lines <- c(e$backsector$lines, list(e))
    }
    out[[i]] <- e
  }
  out
}

load_segs <- function(data, vertexes, lines) {
  n <- length(data) %/% MAPSEG_SIZE
  out <- vector("list", n)
  for (i in seq_len(n)) {
    o <- (i - 1L) * MAPSEG_SIZE
    vals <- i16_vec(data[o + seq_len(MAPSEG_SIZE)])
    e <- new.env(parent = emptyenv())
    e$v1 <- vertexes[[vals[1] + 1L]]
    e$v2 <- vertexes[[vals[2] + 1L]]
    e$angle <- as_u32(as.numeric(vals[3]) * 65536)
    ln <- lines[[vals[4] + 1L]]
    e$linedef <- ln
    side <- vals[5]
    e$offset <- as.numeric(vals[6]) * FRACUNIT
    pick <- if (side %in% c(0L, 1L) && !is.null(ln$sides[[side + 1L]])) side + 1L else 1L
    e$sidedef <- ln$sides[[pick]]
    e$frontsector <- if (is.null(e$sidedef)) NULL else e$sidedef$sector
    other <- ln$sides[[bitwXor(side, 1L) + 1L]]
    if (bitwAnd(ln$flags, ML_TWOSIDED) != 0L && !is.null(other)) {
      e$backsector <- other$sector
    } else {
      e$backsector <- NULL
    }
    out[[i]] <- e
  }
  out
}

load_subsectors <- function(data) {
  n <- length(data) %/% MAPSUBSECTOR_SIZE
  vals <- u16_vec(data)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    e <- new.env(parent = emptyenv())
    e$numlines <- vals[i * 2L - 1L]
    e$firstline <- vals[i * 2L]
    e$sector <- NULL
    out[[i]] <- e
  }
  out
}

load_nodes <- function(data) {
  n <- length(data) %/% MAPNODE_SIZE
  out <- vector("list", n)
  for (i in seq_len(n)) {
    o <- (i - 1L) * MAPNODE_SIZE
    coords <- i16_vec(data[o + seq_len(24L)])
    kids <- u16_vec(data[o + 24L + seq_len(4L)])
    e <- new.env(parent = emptyenv())
    e$x <- as.numeric(coords[1]) * FRACUNIT
    e$y <- as.numeric(coords[2]) * FRACUNIT
    e$dx <- as.numeric(coords[3]) * FRACUNIT
    e$dy <- as.numeric(coords[4]) * FRACUNIT
    e$bbox <- list(
      as.numeric(coords[5:8]) * FRACUNIT,
      as.numeric(coords[9:12]) * FRACUNIT
    )
    e$children <- kids
    out[[i]] <- e
  }
  out
}

load_things <- function(data) {
  n <- length(data) %/% MAPTHING_SIZE
  vals <- i16_vec(data)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    b <- (i - 1L) * 5L
    e <- new.env(parent = emptyenv())
    e$x <- vals[b + 1L]
    e$y <- vals[b + 2L]
    e$angle <- vals[b + 3L]
    e$type <- vals[b + 4L]
    e$options <- vals[b + 5L]
    out[[i]] <- e
  }
  out
}

load_blockmap <- function(world, data) {
  lump <- u16_vec(data)
  world$blockmaplump <- lump
  world$blockmap <- integer()
  if (length(lump) < 4L) return(invisible(world))
  org <- i16_vec(data[1:4])
  wh <- i16_vec(data[5:8])
  world$bmaporgx <- as.numeric(org[1]) * FRACUNIT
  world$bmaporgy <- as.numeric(org[2]) * FRACUNIT
  world$bmapwidth <- wh[1]
  world$bmapheight <- wh[2]
  count <- world$bmapwidth * world$bmapheight
  if (count > 0L && length(lump) >= 4L + count) {
    world$blockmap <- lump[5:(4L + count)]
  }
  world$blocklinks <- vector("list", max(0L, count))
  world$validcount <- 0
  invisible(world)
}

world_bind_textures <- function(world, res) {
  for (s in world$sectors) {
    s$floorpic <- flat_num_for_name(res, s$floorflat)
    s$ceilingpic <- flat_num_for_name(res, s$ceilingflat)
  }
  for (sd in world$sides) {
    sd$toptexture <- texture_num_for_name(res, sd$topname)
    sd$bottomtexture <- texture_num_for_name(res, sd$bottomname)
    sd$midtexture <- texture_num_for_name(res, sd$midname)
  }
  invisible(world)
}

side_texnum <- function(side, field, namefield) {
  if (is.null(side)) return(NA_integer_)
  nm <- side[[namefield]]
  if (is.null(nm) || !nzchar(nm) || nm == "-") return(NA_integer_)
  tex <- side[[field]]
  if (is.null(tex)) NA_integer_ else as.integer(tex)
}

line_texnum <- function(ln) {
  side <- ln$sides[[1]]
  if (is.null(side)) return(NA_integer_)
  if (bitwAnd(ln$flags, ML_TWOSIDED) != 0L) {
    pick <- c(
      side_texnum(side, "toptexture", "topname"),
      side_texnum(side, "bottomtexture", "bottomname"),
      side_texnum(side, "midtexture", "midname")
    )
  } else {
    pick <- c(
      side_texnum(side, "midtexture", "midname"),
      side_texnum(side, "toptexture", "topname"),
      side_texnum(side, "bottomtexture", "bottomname")
    )
  }
  pick <- pick[!is.na(pick)]
  if (length(pick)) pick[[1]] else NA_integer_
}

world_sector_at <- function(world, x, y) {
  if (!length(world$nodes)) {
    if (length(world$subsectors)) return(world$subsectors[[1]]$sector)
    return(NULL)
  }
  nodenum <- length(world$nodes) - 1L
  repeat {
    if (nodenum < 0L || bitwAnd(as.integer(nodenum), NF_SUBSECTOR) != 0L) {
      idx <- if (nodenum < 0L) 0L else bitwAnd(as.integer(nodenum), 32767L)
      return(world$subsectors[[idx + 1L]]$sector)
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

spawn_health <- function(doomed) {
  tab <- c(
    "3004" = 20, "9" = 30, "3001" = 60, "3002" = 150, "58" = 150,
    "3005" = 400, "3006" = 100, "3003" = 1000, "7" = 3000, "16" = 4000,
    "84" = 50, "65" = 70, "68" = 500, "64" = 700, "66" = 300,
    "69" = 500, "67" = 600, "71" = 400, "2035" = 20, "72" = 100
  )
  v <- tab[as.character(doomed)]
  if (!length(v) || is.na(v)) 100 else as.numeric(v)
}

world_spawn_things <- function(world, skill) {
  world$mobjs <- list()
  by_num <- new.env(parent = emptyenv())
  for (info in SPAWNINFO) {
    if (is.null(info)) next
    by_num[[as.character(info$doomed)]] <- info
  }
  skill <- as.integer(skill)
  bit <- if (skill <= 0L) 1L else if (skill >= 4L) 4L else bitwShiftL(1L, skill - 1L)
  for (th in world$things) {
    if (th$type %in% c(1L, 2L, 3L, 4L, 11L)) next
    if (bitwAnd(th$options, bit) == 0L) next
    info <- by_num[[as.character(th$type)]]
    if (is.null(info)) next
    x <- th$x * FRACUNIT
    y <- th$y * FRACUNIT
    sec <- world_sector_at(world, x, y)
    if (is.null(sec)) next
    z <- if (bitwAnd(as.integer(info$flags), as.integer(MF_SPAWNCEILING)) != 0L) {
      sec$ceilingheight - info$height
    } else {
      sec$floorheight
    }
    mo <- new.env(parent = emptyenv())
    mo$x <- x
    mo$y <- y
    mo$z <- z
    mo$angle <- as_u32(th$angle * ANG90 / 90)
    mo$sprite <- info$sprite
    mo$frame <- info$frame
    mo$flags <- info$flags
    mo$radius <- info$radius
    mo$height <- info$height
    mo$health <- spawn_health(th$type)
    mo$doomed <- th$type
    mo$momx <- 0
    mo$momy <- 0
    mo$momz <- 0
    mo$floorz <- z
    mo$ceilingz <- sec$ceilingheight
    mo$player <- NULL
    mo$tics <- 0L
    mo$spawn_x <- th$x
    mo$spawn_y <- th$y
    mo$spawn_angle <- th$angle
    mo$spawn_options <- th$options
    if (bitwAnd(as.integer(th$options), 8L) != 0L) mo$flags <- bitwOr(as.integer(mo$flags), MF_AMBUSH)
    mo$bnext <- NULL
    mo$bprev <- NULL
    mo$blocklinked <- FALSE
    world$mobjs[[length(world$mobjs) + 1L]] <- mo
  }
  invisible(world)
}

player_start <- function(world) {
  for (th in world$things) if (th$type == 1L) return(th)
  if (length(world$things)) world$things[[1]] else NULL
}

segment_pixels <- function(x0, y0, x1, y1) {
  x0 <- as.integer(round(x0))
  y0 <- as.integer(round(y0))
  x1 <- as.integer(round(x1))
  y1 <- as.integer(round(y1))
  dx <- abs(x1 - x0)
  dy <- abs(y1 - y0)
  sx <- if (x0 < x1) 1L else -1L
  sy <- if (y0 < y1) 1L else -1L
  err <- dx - dy
  n <- dx + dy + 1L
  xs <- integer(n)
  ys <- integer(n)
  i <- 1L
  repeat {
    xs[i] <- x0
    ys[i] <- y0
    if (x0 == x1 && y0 == y1) break
    e2 <- err * 2L
    if (e2 > -dy) {
      err <- err - dy
      x0 <- x0 + sx
    }
    if (e2 < dx) {
      err <- err + dx
      y0 <- y0 + sy
    }
    i <- i + 1L
    if (i > n) break
  }
  xs <- xs[seq_len(i)]
  ys <- ys[seq_len(i)]
  ok <- xs >= 0L & xs < SCREENWIDTH & ys >= 0L & ys < SCREENHEIGHT
  ys[ok] * SCREENWIDTH + xs[ok]
}

world_draw <- function(world, video, res = NULL) {
  fb <- raw(SCREENPIXELS)
  if (!length(world$vertexes)) return(fb)
  xs <- vapply(world$vertexes, function(v) v$x / FRACUNIT, numeric(1))
  ys <- vapply(world$vertexes, function(v) v$y / FRACUNIT, numeric(1))
  minx <- min(xs)
  maxx <- max(xs)
  miny <- min(ys)
  maxy <- max(ys)
  spanx <- max(maxx - minx, 1)
  spany <- max(maxy - miny, 1)
  scale <- min(300 / spanx, 180 / spany)
  ox <- (SCREENWIDTH - spanx * scale) / 2
  oy <- (SCREENHEIGHT - spany * scale) / 2
  to_screen <- function(x, y) {
    c(ox + (x - minx) * scale, oy + (maxy - y) * scale)
  }
  lum <- video$pal$r + video$pal$g + video$pal$b
  bright <- as.raw(which.max(lum) - 1L)
  dimcol <- as.raw(which.min(abs(lum - 90)) - 1L)
  nlines <- length(world$lines)
  chunks <- vector("list", nlines)
  colors <- integer(nlines)
  for (i in seq_len(nlines)) {
    ln <- world$lines[[i]]
    a <- to_screen(ln$v1$x / FRACUNIT, ln$v1$y / FRACUNIT)
    b <- to_screen(ln$v2$x / FRACUNIT, ln$v2$y / FRACUNIT)
    chunks[[i]] <- segment_pixels(a[1], a[2], b[1], b[2])
    if (is.null(res)) {
      colors[i] <- as.integer(bright)
    } else {
      sample <- as.integer(texture_sample(res, line_texnum(ln)))
      colors[i] <- if (sample == 0L) as.integer(dimcol) else sample
    }
  }
  for (col in unique(colors)) {
    px <- unlist(chunks[colors == col], use.names = FALSE)
    if (length(px)) fb[px + 1L] <- as.raw(col)
  }
  start <- player_start(world)
  if (!is.null(start)) {
    p <- to_screen(start$x, start$y)
    rad <- start$angle * pi / 180
    tip <- c(p[1] + cos(rad) * 8, p[2] - sin(rad) * 8)
    px <- c(
      segment_pixels(p[1] - 2, p[2], p[1] + 2, p[2]),
      segment_pixels(p[1], p[2] - 2, p[1], p[2] + 2),
      segment_pixels(p[1], p[2], tip[1], tip[2])
    )
    if (length(px)) fb[px + 1L] <- bright
  }
  fb
}
