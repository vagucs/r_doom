# Portas, elevadores, chao, teto, interruptores e saida (p_spec).

VDOORSPEED <- 2 * FRACUNIT
VDOORWAIT <- 150
PLATSPEED <- FRACUNIT
PLATWAIT <- 3
FLOORSPEED <- FRACUNIT
CEILSPEED <- FRACUNIT
GLOWSPEED <- 8
STROBEBRIGHT <- 5
FASTDARK <- 15
SLOWDARK <- 35
BUTTONTIME <- 35
RESULT_CRUSHED <- 1L
RESULT_PASTDEST <- 2L
VLD_NORMAL <- 0L
VLD_CLOSE30 <- 1L
VLD_CLOSE <- 2L
VLD_OPEN <- 3L
VLD_RAISEIN5 <- 4L
VLD_BLAZERAISE <- 5L
VLD_BLAZEOPEN <- 6L
VLD_BLAZECLOSE <- 7L
PLAT_DOWN <- 0L
PLAT_UP <- 1L
PLAT_WAITING <- 2L
PLAT_DWUS <- 0L
PLAT_PERPETUAL <- 1L
PLAT_BLAZEDWUS <- 2L
CEIL_LOWERTOFLOOR <- 0L
CEIL_RAISETOHIGHEST <- 1L
CEIL_LOWERANDCRUSH <- 2L
CEIL_CRUSHANDRAISE <- 3L
CEIL_FASTCRUSH <- 4L
CEIL_SILENTCRUSH <- 5L
MF_MISSILE <- 65536

SWITCH_PAIRS <- rbind(
  c("SW1BRCOM", "SW2BRCOM"), c("SW1BRN1", "SW2BRN1"), c("SW1BRN2", "SW2BRN2"),
  c("SW1BRNGN", "SW2BRNGN"), c("SW1BROWN", "SW2BROWN"), c("SW1COMM", "SW2COMM"),
  c("SW1COMP", "SW2COMP"), c("SW1DIRT", "SW2DIRT"), c("SW1EXIT", "SW2EXIT"),
  c("SW1GRAY", "SW2GRAY"), c("SW1GRAY1", "SW2GRAY1"), c("SW1METAL", "SW2METAL"),
  c("SW1PIPE", "SW2PIPE"), c("SW1SLAD", "SW2SLAD"), c("SW1STARG", "SW2STARG"),
  c("SW1STON1", "SW2STON1"), c("SW1STON2", "SW2STON2"), c("SW1STONE", "SW2STONE"),
  c("SW1STRTN", "SW2STRTN"), c("SW1BLUE", "SW2BLUE"), c("SW1CMT", "SW2CMT"),
  c("SW1GARG", "SW2GARG"), c("SW1GSTON", "SW2GSTON"), c("SW1HOT", "SW2HOT"),
  c("SW1LION", "SW2LION"), c("SW1SATYR", "SW2SATYR"), c("SW1SKIN", "SW2SKIN"),
  c("SW1VINE", "SW2VINE"), c("SW1WOOD", "SW2WOOD"), c("SW1PANEL", "SW2PANEL"),
  c("SW1ROCK", "SW2ROCK"), c("SW1MET2", "SW2MET2"), c("SW1WDMET", "SW2WDMET"),
  c("SW1BRIK", "SW2BRIK"), c("SW1MOD1", "SW2MOD1"), c("SW1ZIM", "SW2ZIM"),
  c("SW1STON6", "SW2STON6"), c("SW1TEK", "SW2TEK"), c("SW1MARB", "SW2MARB"),
  c("SW1SKULL", "SW2SKULL")
)

spec_sectors <- function(sector) {
  out <- list()
  for (ln in sector$lines) {
    other <- if (identical(ln$frontsector, sector)) ln$backsector else ln$frontsector
    if (!is.null(other) && !identical(other, sector)) {
      if (!any(vapply(out, function(s) identical(s, other), logical(1)))) out[[length(out) + 1L]] <- other
    }
  }
  out
}

spec_lowest_ceiling <- function(sector) {
  h <- INT_MAX
  for (other in spec_sectors(sector)) if (other$ceilingheight < h) h <- other$ceilingheight
  if (h == INT_MAX) sector$ceilingheight else h
}

spec_lowest_floor <- function(sector) {
  h <- sector$floorheight
  for (other in spec_sectors(sector)) if (other$floorheight < h) h <- other$floorheight
  h
}

spec_highest_floor <- function(sector) {
  h <- -500 * FRACUNIT
  for (other in spec_sectors(sector)) if (other$floorheight > h) h <- other$floorheight
  h
}

spec_highest_ceiling <- function(sector) {
  h <- sector$ceilingheight
  for (other in spec_sectors(sector)) if (other$ceilingheight > h) h <- other$ceilingheight
  h
}

spec_next_floor <- function(sector, current) {
  min_h <- INT_MAX
  found <- FALSE
  for (other in spec_sectors(sector)) {
    if (other$floorheight > current && other$floorheight < min_h) {
      min_h <- other$floorheight
      found <- TRUE
    }
  }
  if (found) min_h else current
}

spec_raise_dest <- function(sector) {
  dest <- spec_lowest_ceiling(sector)
  if (dest <= sector$ceilingheight) dest else sector$ceilingheight
}

spec_min_light <- function(sector, maxlight) {
  low <- maxlight
  for (other in spec_sectors(sector)) if (other$lightlevel < low) low <- other$lightlevel
  low
}

spec_max_light <- function(sector) {
  high <- sector$lightlevel
  for (other in spec_sectors(sector)) if (other$lightlevel > high) high <- other$lightlevel
  high
}

sectors_from_tag <- function(world, tag) {
  if (!tag) return(list())
  Filter(function(s) s$tag == tag, world$sectors)
}

move_plane <- function(world, sector, speed, dest, floor_or_ceiling, direction, crush = FALSE) {
  last <- if (floor_or_ceiling == 0L) sector$floorheight else sector$ceilingheight
  past <- FALSE
  if (direction == -1) {
    if (last - speed < dest) {
      next_h <- dest
      past <- TRUE
    } else next_h <- last - speed
  } else if (last + speed > dest) {
    next_h <- dest
    past <- TRUE
  } else next_h <- last + speed
  if (floor_or_ceiling == 0L) sector$floorheight <- next_h else sector$ceilingheight <- next_h
  if (next_h != last) world$dirty <- TRUE
  nofit <- change_sector(world, sector, crush)
  if (nofit) {
    if (!crush || past) {
      if (floor_or_ceiling == 0L) sector$floorheight <- last else sector$ceilingheight <- last
      change_sector(world, sector, crush)
    }
    return(if (past) RESULT_PASTDEST else RESULT_CRUSHED)
  }
  if (past) RESULT_PASTDEST else 0L
}

specials_new <- function(world, res, game) {
  s <- new.env(parent = emptyenv())
  s$world <- world
  s$res <- res
  s$game <- game
  s$thinkers <- list()
  s$lights <- list()
  s$scroll <- list()
  s$buttons <- list()
  s$exit_requested <- FALSE
  s$secret_exit <- FALSE
  s$switch_map <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(SWITCH_PAIRS))) {
    a <- SWITCH_PAIRS[i, 1]
    b <- SWITCH_PAIRS[i, 2]
    if (!exists(a, res$tex_index, inherits = FALSE) && !exists(b, res$tex_index, inherits = FALSE)) next
    ia <- texture_num_for_name(res, a)
    ib <- texture_num_for_name(res, b)
    s$switch_map[[as.character(ia)]] <- ib
    s$switch_map[[as.character(ib)]] <- ia
  }
  specials_spawn(s)
  s
}

specials_spawn <- function(s) {
  world <- s$world
  for (sector in world$sectors) {
    spec <- sector$special
    if (spec == 1L) spawn_light(s, sector, "flash")
    else if (spec == 2L) spawn_light(s, sector, "strobe", FASTDARK, FALSE)
    else if (spec == 3L) spawn_light(s, sector, "strobe", SLOWDARK, FALSE)
    else if (spec == 4L) spawn_light(s, sector, "strobe", FASTDARK, FALSE)
    else if (spec == 8L) spawn_light(s, sector, "glow")
    else if (spec == 10L) spawn_delayed_door(s, sector, VLD_CLOSE, 30L * TICRATE)
    else if (spec == 12L) spawn_light(s, sector, "strobe", SLOWDARK, TRUE)
    else if (spec == 13L) spawn_light(s, sector, "strobe", FASTDARK, TRUE)
    else if (spec == 14L) spawn_delayed_door(s, sector, VLD_RAISEIN5, 5L * 60L * TICRATE)
    else if (spec == 17L) spawn_light(s, sector, "fire")
  }
  for (ln in world$lines) if (ln$special == 48L) s$scroll[[length(s$scroll) + 1L]] <- ln
  for (th in world$things) {
    if (th$type != 14L) next
    mo <- new.env(parent = emptyenv())
    mo$x <- th$x * FRACUNIT
    mo$y <- th$y * FRACUNIT
    mo$z <- 0
    mo$angle <- as_u32((th$angle %/% 45) * 536870912)
    mo$doomed <- 14L
    mo$sprite <- ""
    mo$frame <- 0L
    mo$flags <- 0
    mo$radius <- 20 * FRACUNIT
    mo$height <- 16 * FRACUNIT
    mo$health <- 1
    mo$player <- NULL
    mo$momx <- 0
    mo$momy <- 0
    mo$momz <- 0
    mo$floorz <- 0
    mo$ceilingz <- 0
    mo$blocklinked <- FALSE
    sec <- world_sector_at(world, mo$x, mo$y)
    if (!is.null(sec)) {
      mo$z <- sec$floorheight
      mo$floorz <- sec$floorheight
      mo$ceilingz <- sec$ceilingheight
    }
    world$mobjs[[length(world$mobjs) + 1L]] <- mo
    set_thing_position(world, mo)
  }
  invisible(s)
}

spawn_light <- function(s, sector, kind, darktime = 0, synced = FALSE) {
  if (kind != "strobe" || sector$special != 4L) sector$special <- 0L
  light <- new.env(parent = emptyenv())
  light$kind <- kind
  light$sector <- sector
  light$maxlight <- sector$lightlevel
  light$minlight <- spec_min_light(sector, sector$lightlevel)
  light$direction <- -1
  light$count <- 1L
  if (kind == "flash") {
    light$maxtime <- 64L
    light$mintime <- 7L
    light$count <- bitwAnd(p_random(), light$maxtime) + 1L
  } else if (kind == "strobe") {
    if (light$minlight == sector$lightlevel) light$minlight <- 0
    light$darktime <- darktime
    light$brighttime <- STROBEBRIGHT
    light$count <- if (synced) 1L else bitwAnd(p_random(), 7L) + 1L
  } else if (kind == "fire") {
    light$minlight <- light$minlight + 16
    light$count <- 4L
  }
  s$lights[[length(s$lights) + 1L]] <- light
}

spawn_delayed_door <- function(s, sector, dtype, countdown) {
  if (!is.null(sector$specialdata)) return(invisible())
  sector$special <- 0L
  door <- new.env(parent = emptyenv())
  door$kind <- "door"
  door$sector <- sector
  door$type <- dtype
  door$direction <- 0L
  door$topheight <- if (dtype == VLD_RAISEIN5) spec_lowest_ceiling(sector) - 4 * FRACUNIT else sector$ceilingheight
  door$speed <- VDOORSPEED
  door$topwait <- VDOORWAIT
  door$topcountdown <- countdown
  door$dead <- FALSE
  sector$specialdata <- door
  s$thinkers[[length(s$thinkers) + 1L]] <- door
}

specials_tick <- function(s) {
  for (light in s$lights) tick_light(s, light)
  for (ln in s$scroll) {
    side <- ln$sides[[1]]
    if (!is.null(side)) {
      side$textureoffset <- side$textureoffset + FRACUNIT
      s$world$dirty <- TRUE
    }
  }
  alive <- list()
  for (th in s$thinkers) {
    if (isTRUE(th$dead)) next
    if (th$kind == "door") tick_door(s, th)
    else if (th$kind == "plat") tick_plat(s, th)
    else if (th$kind == "floor") tick_floor(s, th)
    else if (th$kind == "ceil") tick_ceiling(s, th)
    if (!isTRUE(th$dead)) alive[[length(alive) + 1L]] <- th
  }
  s$thinkers <- alive
  keep <- list()
  for (btn in s$buttons) {
    btn$timer <- btn$timer - 1L
    if (btn$timer > 0L) {
      keep[[length(keep) + 1L]] <- btn
      next
    }
    side <- btn$line$sides[[1]]
    if (!is.null(side)) {
      if (btn$where == "top") side$toptexture <- btn$texture
      else if (btn$where == "mid") side$midtexture <- btn$texture
      else side$bottomtexture <- btn$texture
      s$world$dirty <- TRUE
    }
  }
  s$buttons <- keep
  invisible(s)
}

tick_light <- function(s, light) {
  sec <- light$sector
  before <- sec$lightlevel
  if (light$kind == "glow") {
    if (light$direction == -1) {
      sec$lightlevel <- sec$lightlevel - GLOWSPEED
      if (sec$lightlevel <= light$minlight) {
        sec$lightlevel <- sec$lightlevel + GLOWSPEED
        light$direction <- 1
      }
    } else {
      sec$lightlevel <- sec$lightlevel + GLOWSPEED
      if (sec$lightlevel >= light$maxlight) {
        sec$lightlevel <- sec$lightlevel - GLOWSPEED
        light$direction <- -1
      }
    }
    if (sec$lightlevel != before) s$world$dirty <- TRUE
    return(invisible())
  }
  light$count <- light$count - 1L
  if (light$count != 0L) return(invisible())
  if (light$kind == "flash") {
    if (sec$lightlevel == light$maxlight) {
      sec$lightlevel <- light$minlight
      light$count <- bitwAnd(p_random(), light$mintime) + 1L
    } else {
      sec$lightlevel <- light$maxlight
      light$count <- bitwAnd(p_random(), light$maxtime) + 1L
    }
  } else if (light$kind == "strobe") {
    if (sec$lightlevel == light$minlight) {
      sec$lightlevel <- light$maxlight
      light$count <- light$brighttime
    } else {
      sec$lightlevel <- light$minlight
      light$count <- light$darktime
    }
  } else if (light$kind == "fire") {
    amount <- bitwAnd(p_random(), 3L) * 16
    if (sec$lightlevel - amount < light$minlight) sec$lightlevel <- light$minlight
    else sec$lightlevel <- light$maxlight - amount
    light$count <- 4L
  }
  if (sec$lightlevel != before) s$world$dirty <- TRUE
  invisible()
}

tick_door <- function(s, door) {
  if (door$direction == 0L) {
    door$topcountdown <- door$topcountdown - 1L
    if (door$topcountdown <= 0L) {
      if (door$type %in% c(VLD_NORMAL, VLD_BLAZERAISE, VLD_CLOSE)) {
        door$direction <- -1L
        s$game$start_sound(if (door$type == VLD_BLAZERAISE) "bdcls" else "dorcls")
      } else if (door$type %in% c(VLD_CLOSE30, VLD_RAISEIN5)) {
        door$direction <- 1L
        s$game$start_sound("doropn")
      }
    }
    return(invisible())
  }
  dest <- if (door$direction == 1L) door$topheight else door$sector$floorheight
  res <- move_plane(s$world, door$sector, door$speed, dest, 1L, door$direction)
  if (res != RESULT_PASTDEST) return(invisible())
  if (door$direction == 1L) {
    if (door$type %in% c(VLD_NORMAL, VLD_BLAZERAISE)) {
      door$direction <- 0L
      door$topcountdown <- door$topwait
    } else {
      door$sector$specialdata <- NULL
      door$dead <- TRUE
    }
  } else if (door$type == VLD_CLOSE30) {
    door$direction <- 0L
    door$topcountdown <- TICRATE * 30
  } else {
    door$sector$specialdata <- NULL
    door$dead <- TRUE
  }
  invisible()
}

tick_plat <- function(s, plat) {
  if (plat$status == PLAT_WAITING) {
    plat$count <- plat$count - 1L
    if (plat$count <= 0L) {
      plat$status <- if (plat$sector$floorheight <= plat$low) PLAT_UP else PLAT_DOWN
      s$game$start_sound("pstart")
    }
    return(invisible())
  }
  dest <- if (plat$status == PLAT_UP) plat$high else plat$low
  direction <- if (plat$status == PLAT_UP) 1L else -1L
  res <- move_plane(s$world, plat$sector, plat$speed, dest, 0L, direction)
  if (res != RESULT_PASTDEST) return(invisible())
  if (plat$status == PLAT_DOWN || plat$type == PLAT_PERPETUAL) {
    plat$status <- PLAT_WAITING
    plat$count <- plat$wait
  } else {
    plat$sector$specialdata <- NULL
    plat$dead <- TRUE
  }
  s$game$start_sound("pstop")
  invisible()
}

tick_floor <- function(s, floor) {
  res <- move_plane(s$world, floor$sector, floor$speed, floor$dest, 0L, floor$direction, floor$crush)
  if (res != RESULT_PASTDEST) return(invisible())
  if (!is.null(floor$floorpic)) floor$sector$floorpic <- floor$floorpic
  floor$sector$specialdata <- NULL
  floor$dead <- TRUE
  invisible()
}

tick_ceiling <- function(s, ceil) {
  dest <- if (ceil$ctype) {
    if (ceil$direction == 1L) ceil$topheight else ceil$bottomheight
  } else ceil$dest
  res <- move_plane(s$world, ceil$sector, ceil$speed, dest, 1L, ceil$direction, ceil$crush)
  bounce <- ceil$ctype %in% c(CEIL_CRUSHANDRAISE, CEIL_FASTCRUSH, CEIL_SILENTCRUSH)
  if (res == RESULT_PASTDEST) {
    if (bounce) {
      if (ceil$direction == -1L) {
        ceil$direction <- 1L
        ceil$speed <- CEILSPEED * (if (ceil$ctype == CEIL_FASTCRUSH) 2 else 1)
      } else ceil$direction <- -1L
      if (ceil$ctype == CEIL_SILENTCRUSH) s$game$start_sound("pstop")
    } else {
      ceil$sector$specialdata <- NULL
      ceil$dead <- TRUE
    }
  } else if (res == RESULT_CRUSHED && bounce) {
    ceil$speed <- max(1, CEILSPEED / 8)
  }
  invisible()
}

spawn_door <- function(s, sector, dtype, reverse = FALSE) {
  if (!is.null(sector$specialdata)) {
    door <- sector$specialdata
    if (identical(door$kind, "door") && dtype %in% c(VLD_NORMAL, VLD_BLAZERAISE)) {
      door$direction <- if (door$direction == -1L) 1L else -1L
      return(TRUE)
    }
    return(FALSE)
  }
  door <- new.env(parent = emptyenv())
  door$kind <- "door"
  door$sector <- sector
  door$type <- dtype
  door$direction <- if (reverse || dtype %in% c(VLD_CLOSE, VLD_BLAZECLOSE, VLD_CLOSE30)) -1L else 1L
  door$topheight <- spec_lowest_ceiling(sector) - 4 * FRACUNIT
  door$speed <- VDOORSPEED * (if (dtype >= VLD_BLAZERAISE) 4 else 1)
  door$topwait <- VDOORWAIT
  door$topcountdown <- 0L
  door$dead <- FALSE
  if (dtype == VLD_CLOSE30) door$topheight <- sector$ceilingheight
  sector$specialdata <- door
  s$thinkers[[length(s$thinkers) + 1L]] <- door
  if (door$direction == 1L) s$game$start_sound(if (dtype < VLD_BLAZERAISE) "doropn" else "bdopn")
  else s$game$start_sound(if (dtype < VLD_BLAZERAISE) "dorcls" else "bdcls")
  TRUE
}

do_door <- function(s, line, dtype, reverse = FALSE) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) if (spawn_door(s, sec, dtype, reverse)) ok <- TRUE
  ok
}

need_key <- function(s, player, spec) {
  if (spec %in% c(26L, 32L, 99L, 133L) && !has_card(player, IT_BLUECARD, IT_BLUESKULL)) {
    set_player_message(player, "precisa da chave azul")
    s$game$start_sound("oof")
    return(TRUE)
  }
  if (spec %in% c(27L, 34L, 136L, 137L) && !has_card(player, IT_YELLOWCARD, IT_YELLOWSKULL)) {
    set_player_message(player, "precisa da chave amarela")
    s$game$start_sound("oof")
    return(TRUE)
  }
  if (spec %in% c(28L, 33L, 134L, 135L) && !has_card(player, IT_REDCARD, IT_REDSKULL)) {
    set_player_message(player, "precisa da chave vermelha")
    s$game$start_sound("oof")
    return(TRUE)
  }
  FALSE
}

vertical_door <- function(s, line, thing) {
  player <- thing$player
  spec <- line$special
  if (!is.null(player) && need_key(s, player, spec)) return(invisible())
  if (is.null(line$sides[[2]]) || is.null(line$sides[[2]]$sector)) return(invisible())
  sec <- line$sides[[2]]$sector
  if (spec %in% c(1L, 26L, 27L, 28L)) dtype <- VLD_NORMAL
  else if (spec %in% c(31L, 32L, 33L, 34L)) {
    dtype <- VLD_OPEN
    line$special <- 0L
  } else if (spec == 117L) dtype <- VLD_BLAZERAISE
  else if (spec %in% c(118L, 99L, 133L, 134L, 135L, 136L, 137L)) {
    dtype <- VLD_BLAZEOPEN
    line$special <- 0L
  } else dtype <- VLD_NORMAL
  spawn_door(s, sec, dtype)
}

start_floor <- function(s, sec, dest, direction, speed, crush = FALSE, floorpic = NULL) {
  if (!is.null(sec$specialdata)) return(FALSE)
  floor <- new.env(parent = emptyenv())
  floor$kind <- "floor"
  floor$sector <- sec
  floor$direction <- direction
  floor$dest <- dest
  floor$speed <- speed
  floor$crush <- crush
  floor$floorpic <- floorpic
  floor$dead <- FALSE
  sec$specialdata <- floor
  s$thinkers[[length(s$thinkers) + 1L]] <- floor
  TRUE
}

do_floor <- function(s, line, dest_fn, direction, speed = FLOORSPEED, crush = FALSE) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (start_floor(s, sec, dest_fn(sec), direction, speed, crush)) ok <- TRUE
  }
  ok
}

do_plat <- function(s, line, blaze = FALSE) {
  ok <- FALSE
  speed <- PLATSPEED * (if (blaze) 8 else 1)
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    plat <- new.env(parent = emptyenv())
    plat$kind <- "plat"
    plat$sector <- sec
    plat$type <- if (blaze) PLAT_BLAZEDWUS else PLAT_DWUS
    plat$status <- PLAT_DOWN
    plat$speed <- speed
    plat$low <- spec_lowest_floor(sec)
    plat$high <- sec$floorheight
    if (plat$low == plat$high) plat$low <- plat$high - 8 * FRACUNIT
    plat$wait <- PLATWAIT * TICRATE
    plat$count <- 0L
    plat$dead <- FALSE
    sec$specialdata <- plat
    s$thinkers[[length(s$thinkers) + 1L]] <- plat
    s$game$start_sound("pstart")
    ok <- TRUE
  }
  ok
}

do_plat_raise <- function(s, line, amount = 0) {
  ok <- FALSE
  pic <- NULL
  if (!is.null(line$sides[[1]]) && !is.null(line$sides[[1]]$sector)) pic <- line$sides[[1]]$sector$floorpic
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    high <- if (amount) sec$floorheight + amount else spec_next_floor(sec, sec$floorheight)
    if (!is.null(pic)) sec$floorpic <- pic
    plat <- new.env(parent = emptyenv())
    plat$kind <- "plat"
    plat$sector <- sec
    plat$type <- PLAT_DWUS
    plat$status <- PLAT_UP
    plat$speed <- PLATSPEED / 2
    plat$low <- sec$floorheight
    plat$high <- high
    plat$wait <- 0L
    plat$count <- 0L
    plat$dead <- FALSE
    sec$specialdata <- plat
    s$thinkers[[length(s$thinkers) + 1L]] <- plat
    s$game$start_sound("pstart")
    ok <- TRUE
  }
  ok
}

do_plat_perpetual <- function(s, line) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    low <- spec_lowest_floor(sec)
    high <- spec_highest_floor(sec)
    if (low > sec$floorheight) low <- sec$floorheight
    if (high < sec$floorheight) high <- sec$floorheight
    plat <- new.env(parent = emptyenv())
    plat$kind <- "plat"
    plat$sector <- sec
    plat$type <- PLAT_PERPETUAL
    plat$status <- bitwAnd(p_random(), 1L)
    plat$speed <- PLATSPEED
    plat$low <- low
    plat$high <- high
    plat$wait <- PLATWAIT * TICRATE
    plat$count <- 0L
    plat$dead <- FALSE
    sec$specialdata <- plat
    s$thinkers[[length(s$thinkers) + 1L]] <- plat
    s$game$start_sound("pstart")
    ok <- TRUE
  }
  ok
}

stop_plat <- function(s, line) {
  ok <- FALSE
  for (th in s$thinkers) {
    if (th$kind == "plat" && !isTRUE(th$dead) && th$sector$tag == line$tag) {
      th$status <- PLAT_WAITING
      th$count <- INT_MAX
      ok <- TRUE
    }
  }
  ok
}

do_stairs <- function(s, line, step, speed) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    height <- sec$floorheight + step
    if (!start_floor(s, sec, height, 1L, speed)) next
    ok <- TRUE
    texture <- sec$floorpic
    cur <- sec
    repeat {
      nxt <- NULL
      for (ln in cur$lines) {
        if (!has_flag(ln$flags, ML_TWOSIDED)) next
        other <- if (identical(ln$frontsector, cur)) ln$backsector else ln$frontsector
        if (is.null(other) || identical(other, cur)) next
        if (!identical(other$floorpic, texture) || !is.null(other$specialdata)) next
        nxt <- other
        break
      }
      if (is.null(nxt)) break
      height <- height + step
      start_floor(s, nxt, height, 1L, speed)
      cur <- nxt
    }
  }
  ok
}

do_crusher <- function(s, line, ctype) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    top <- sec$ceilingheight
    bottom <- sec$floorheight
    crush <- ctype != CEIL_RAISETOHIGHEST
    speed <- CEILSPEED * (if (ctype == CEIL_FASTCRUSH) 2 else 1)
    direction <- -1L
    dest <- bottom
    if (ctype == CEIL_RAISETOHIGHEST) {
      dest <- spec_highest_ceiling(sec)
      direction <- 1L
      crush <- FALSE
    } else if (ctype != CEIL_LOWERTOFLOOR) {
      bottom <- bottom + 8 * FRACUNIT
      dest <- bottom
    }
    ceil <- new.env(parent = emptyenv())
    ceil$kind <- "ceil"
    ceil$sector <- sec
    ceil$direction <- direction
    ceil$dest <- dest
    ceil$speed <- speed
    ceil$crush <- crush
    ceil$ctype <- ctype
    ceil$topheight <- top
    ceil$bottomheight <- bottom
    ceil$dead <- FALSE
    sec$specialdata <- ceil
    s$thinkers[[length(s$thinkers) + 1L]] <- ceil
    ok <- TRUE
  }
  ok
}

do_donut <- function(s, line) {
  ok <- FALSE
  for (s1 in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(s1$specialdata) || !length(s1$lines)) next
    ln0 <- s1$lines[[1]]
    s2 <- if (identical(ln0$frontsector, s1)) ln0$backsector else ln0$frontsector
    if (is.null(s2)) next
    s3 <- NULL
    for (ln in s2$lines) {
      other <- ln$backsector
      if (is.null(other) || identical(other, s1)) next
      s3 <- other
      break
    }
    if (is.null(s3)) next
    if (start_floor(s, s2, s3$floorheight, 1L, FLOORSPEED / 2, floorpic = s3$floorpic)) ok <- TRUE
    if (start_floor(s, s1, s3$floorheight, -1L, FLOORSPEED / 2)) ok <- TRUE
  }
  ok
}

raise_to_texture <- function(s, line) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    minsize <- INT_MAX
    for (ln in sec$lines) {
      if (!has_flag(ln$flags, ML_TWOSIDED)) next
      for (side in ln$sides) {
        if (is.null(side) || is.null(side$bottomtexture) || side$bottomtexture <= 0) next
        h <- texture_height(s$res, side$bottomtexture)
        if (h > 0 && h < minsize) minsize <- h
      }
    }
    if (minsize == INT_MAX) minsize <- 64 * FRACUNIT
    if (start_floor(s, sec, sec$floorheight + minsize, 1L, FLOORSPEED)) ok <- TRUE
  }
  ok
}

lower_and_change <- function(s, line) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    dest <- spec_lowest_floor(sec)
    pic <- sec$floorpic
    for (other in spec_sectors(sec)) {
      if (other$floorheight == dest) {
        pic <- other$floorpic
        break
      }
    }
    if (start_floor(s, sec, dest, -1L, FLOORSPEED, floorpic = pic)) ok <- TRUE
  }
  ok
}

light_on <- function(s, line, bright) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    sec$lightlevel <- if (bright) bright else spec_max_light(sec)
    s$world$dirty <- TRUE
    ok <- TRUE
  }
  ok
}

lights_off <- function(s, line) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    sec$lightlevel <- spec_min_light(sec, sec$lightlevel)
    s$world$dirty <- TRUE
    ok <- TRUE
  }
  ok
}

start_strobing <- function(s, line) {
  ok <- FALSE
  for (sec in sectors_from_tag(s$world, line$tag)) {
    if (!is.null(sec$specialdata)) next
    spawn_light(s, sec, "strobe", SLOWDARK, FALSE)
    ok <- TRUE
  }
  ok
}

change_switch <- function(s, line, use_again) {
  side <- line$sides[[1]]
  if (is.null(side)) return(invisible())
  if (!use_again) line$special <- 0L
  sound <- if (line$special == 11L) "swtchx" else "swtchn"
  for (where in c("top", "mid", "bottom")) {
    field <- paste0(where, "texture")
    tex <- side[[field]]
    key <- as.character(tex)
    if (is.null(tex) || !exists(key, s$switch_map, inherits = FALSE)) next
    new <- s$switch_map[[key]]
    if (use_again) {
      btn <- new.env(parent = emptyenv())
      btn$line <- line
      btn$where <- where
      btn$texture <- tex
      btn$timer <- BUTTONTIME
      s$buttons[[length(s$buttons) + 1L]] <- btn
    }
    side[[field]] <- new
    s$world$dirty <- TRUE
    s$game$start_sound(sound)
    return(invisible())
  }
  s$game$start_sound(sound)
}

specials_use <- function(s, line, thing, side) {
  if (side != 0L) return(FALSE)
  spec <- line$special
  if (spec %in% c(1L, 26L, 27L, 28L, 31L, 32L, 33L, 34L, 99L, 117L, 118L, 133L, 134L, 135L, 136L, 137L)) {
    vertical_door(s, line, thing)
    return(TRUE)
  }
  if (spec == 11L) {
    change_switch(s, line, 0L)
    s$exit_requested <- TRUE
    return(TRUE)
  }
  if (spec == 51L) {
    change_switch(s, line, 0L)
    s$exit_requested <- TRUE
    s$secret_exit <- TRUE
    return(TRUE)
  }
  ok <- FALSE
  again <- FALSE
  if (spec == 29L) ok <- do_door(s, line, VLD_NORMAL)
  else if (spec == 50L) ok <- do_door(s, line, VLD_CLOSE)
  else if (spec == 103L) ok <- do_door(s, line, VLD_OPEN)
  else if (spec == 111L) ok <- do_door(s, line, VLD_BLAZERAISE)
  else if (spec == 112L) ok <- do_door(s, line, VLD_BLAZEOPEN)
  else if (spec == 113L) ok <- do_door(s, line, VLD_BLAZECLOSE)
  else if (spec == 21L) ok <- do_plat(s, line, FALSE)
  else if (spec == 122L) ok <- do_plat(s, line, TRUE)
  else if (spec == 18L) ok <- do_floor(s, line, function(sec) spec_next_floor(sec, sec$floorheight), 1L)
  else if (spec == 23L) ok <- do_floor(s, line, spec_lowest_floor, -1L)
  else if (spec == 71L) ok <- do_floor(s, line, spec_highest_floor, -1L)
  else if (spec == 101L) ok <- do_floor(s, line, spec_raise_dest, 1L)
  else if (spec == 102L) ok <- do_floor(s, line, function(sec) sec$floorheight - 8 * FRACUNIT, -1L)
  else if (spec == 7L) ok <- do_stairs(s, line, 8 * FRACUNIT, FLOORSPEED / 4)
  else if (spec == 127L) ok <- do_stairs(s, line, 16 * FRACUNIT, FLOORSPEED * 4)
  else if (spec == 41L) ok <- do_crusher(s, line, CEIL_LOWERTOFLOOR)
  else if (spec == 49L) ok <- do_crusher(s, line, CEIL_CRUSHANDRAISE)
  else if (spec == 9L) ok <- do_donut(s, line)
  else if (spec == 14L) ok <- do_plat_raise(s, line, 32 * FRACUNIT)
  else if (spec == 15L) ok <- do_plat_raise(s, line, 24 * FRACUNIT)
  else if (spec == 20L) ok <- do_plat_raise(s, line, 0)
  else if (spec == 55L) ok <- do_floor(s, line, function(sec) spec_raise_dest(sec) - 8 * FRACUNIT, 1L, crush = TRUE)
  else if (spec == 131L) ok <- do_floor(s, line, function(sec) spec_next_floor(sec, sec$floorheight), 1L, FLOORSPEED * 4)
  else if (spec == 140L) ok <- do_floor(s, line, function(sec) sec$floorheight + 512 * FRACUNIT, 1L)
  else if (spec == 42L) { ok <- do_door(s, line, VLD_CLOSE); again <- TRUE }
  else if (spec == 61L) { ok <- do_door(s, line, VLD_OPEN); again <- TRUE }
  else if (spec == 63L) { ok <- do_door(s, line, VLD_NORMAL); again <- TRUE }
  else if (spec == 62L) { ok <- do_plat(s, line, FALSE); again <- TRUE }
  else if (spec == 114L) { ok <- do_door(s, line, VLD_BLAZERAISE); again <- TRUE }
  else if (spec == 115L) { ok <- do_door(s, line, VLD_BLAZEOPEN); again <- TRUE }
  else if (spec == 116L) { ok <- do_door(s, line, VLD_BLAZECLOSE); again <- TRUE }
  else if (spec %in% c(120L, 123L)) { ok <- do_plat(s, line, TRUE); again <- TRUE }
  else if (spec == 45L) { ok <- do_floor(s, line, function(sec) sec$floorheight - 8 * FRACUNIT, -1L); again <- TRUE }
  else if (spec == 60L) { ok <- do_floor(s, line, spec_lowest_floor, -1L); again <- TRUE }
  else if (spec == 64L) { ok <- do_floor(s, line, spec_raise_dest, 1L); again <- TRUE }
  else if (spec == 70L) { ok <- do_floor(s, line, spec_highest_floor, -1L, FLOORSPEED * 4); again <- TRUE }
  else if (spec == 43L) { ok <- do_crusher(s, line, CEIL_LOWERTOFLOOR); again <- TRUE }
  else if (spec == 65L) { ok <- do_floor(s, line, function(sec) spec_raise_dest(sec) - 8 * FRACUNIT, 1L, crush = TRUE); again <- TRUE }
  else if (spec == 66L) { ok <- do_plat_raise(s, line, 24 * FRACUNIT); again <- TRUE }
  else if (spec == 67L) { ok <- do_plat_raise(s, line, 32 * FRACUNIT); again <- TRUE }
  else if (spec == 68L) { ok <- do_plat_raise(s, line, 0); again <- TRUE }
  else if (spec == 69L) { ok <- do_floor(s, line, function(sec) spec_next_floor(sec, sec$floorheight), 1L); again <- TRUE }
  else if (spec == 132L) { ok <- do_floor(s, line, function(sec) spec_next_floor(sec, sec$floorheight), 1L, FLOORSPEED * 4); again <- TRUE }
  else if (spec == 138L) { ok <- light_on(s, line, 255); again <- TRUE }
  else if (spec == 139L) { ok <- light_on(s, line, 35); again <- TRUE }
  else return(FALSE)
  if (ok) change_switch(s, line, if (again) 1L else 0L)
  ok
}

specials_shoot <- function(s, line, thing) {
  spec <- line$special
  if (spec == 24L) {
    if (do_floor(s, line, spec_raise_dest, 1L)) change_switch(s, line, 0L)
  } else if (spec == 46L) {
    do_door(s, line, VLD_OPEN)
    change_switch(s, line, 1L)
  } else if (spec == 47L) {
    if (do_plat_raise(s, line, 0)) change_switch(s, line, 0L)
  }
  invisible()
}

specials_cross <- function(s, line, side, thing) {
  spec <- line$special
  if (spec == 52L) {
    s$exit_requested <- TRUE
    return(invisible())
  }
  if (spec == 124L) {
    s$exit_requested <- TRUE
    s$secret_exit <- TRUE
    return(invisible())
  }
  once <- spec %in% c(
    2L, 3L, 4L, 5L, 6L, 8L, 10L, 12L, 13L, 16L, 17L, 19L, 22L, 25L, 30L, 35L, 36L,
    37L, 38L, 39L, 40L, 44L, 53L, 54L, 56L, 57L, 58L, 59L, 100L, 104L, 108L, 109L,
    110L, 119L, 121L, 125L, 130L, 141L
  )
  again <- spec %in% c(
    72L, 73L, 74L, 75L, 76L, 77L, 79L, 80L, 81L, 82L, 83L, 84L, 86L, 87L, 88L, 89L,
    90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 105L, 106L, 107L, 120L, 126L, 128L, 129L
  )
  if (!once && !again) return(invisible())
  if (spec %in% c(2L, 86L)) do_door(s, line, VLD_OPEN)
  else if (spec %in% c(3L, 75L)) do_door(s, line, VLD_CLOSE)
  else if (spec %in% c(4L, 90L)) do_door(s, line, VLD_NORMAL)
  else if (spec %in% c(16L, 76L)) do_door(s, line, VLD_CLOSE30, TRUE)
  else if (spec %in% c(108L, 105L)) do_door(s, line, VLD_BLAZERAISE)
  else if (spec %in% c(109L, 106L)) do_door(s, line, VLD_BLAZEOPEN)
  else if (spec %in% c(110L, 107L)) do_door(s, line, VLD_BLAZECLOSE)
  else if (spec %in% c(5L, 91L)) do_floor(s, line, spec_raise_dest, 1L)
  else if (spec == 19L) do_floor(s, line, spec_highest_floor, -1L)
  else if (spec %in% c(36L, 98L)) do_floor(s, line, spec_highest_floor, -1L, FLOORSPEED * 4)
  else if (spec %in% c(38L, 82L)) do_floor(s, line, spec_lowest_floor, -1L)
  else if (spec == 83L) do_floor(s, line, spec_highest_floor, -1L)
  else if (spec %in% c(56L, 94L)) do_floor(s, line, function(sec) spec_raise_dest(sec) - 8 * FRACUNIT, 1L, crush = TRUE)
  else if (spec %in% c(58L, 59L, 92L, 93L)) do_floor(s, line, function(sec) sec$floorheight + 24 * FRACUNIT, 1L)
  else if (spec %in% c(119L, 128L)) do_floor(s, line, function(sec) spec_next_floor(sec, sec$floorheight), 1L)
  else if (spec %in% c(130L, 129L)) do_floor(s, line, function(sec) spec_next_floor(sec, sec$floorheight), 1L, FLOORSPEED * 4)
  else if (spec == 8L) do_stairs(s, line, 8 * FRACUNIT, FLOORSPEED / 4)
  else if (spec == 100L) do_stairs(s, line, 16 * FRACUNIT, FLOORSPEED * 4)
  else if (spec == 6L || spec == 77L) do_crusher(s, line, CEIL_FASTCRUSH)
  else if (spec == 25L || spec == 73L) do_crusher(s, line, CEIL_CRUSHANDRAISE)
  else if (spec == 44L || spec == 72L) do_crusher(s, line, CEIL_LOWERANDCRUSH)
  else if (spec == 141L) do_crusher(s, line, CEIL_SILENTCRUSH)
  else if (spec == 40L) {
    do_crusher(s, line, CEIL_RAISETOHIGHEST)
    do_floor(s, line, spec_lowest_floor, -1L)
  }
  else if (spec %in% c(10L, 88L)) do_plat(s, line, FALSE)
  else if (spec %in% c(121L, 120L)) do_plat(s, line, TRUE)
  else if (spec %in% c(22L, 95L)) do_plat_raise(s, line, 0)
  else if (spec == 53L || spec == 87L) do_plat_perpetual(s, line)
  else if (spec %in% c(54L, 57L, 74L, 89L)) stop_plat(s, line)
  else if (spec == 12L) light_on(s, line, 0)
  else if (spec == 13L || spec == 81L) light_on(s, line, 255)
  else if (spec == 35L || spec == 79L) light_on(s, line, 35)
  else if (spec == 80L) light_on(s, line, 0)
  else if (spec == 17L) start_strobing(s, line)
  else if (spec == 104L) lights_off(s, line)
  else if (spec == 30L || spec == 96L) raise_to_texture(s, line)
  else if (spec == 37L || spec == 84L) lower_and_change(s, line)
  else if (spec == 39L || spec == 97L) specials_teleport(s, line, side, thing)
  else if (spec == 125L || spec == 126L) {
    if (is.null(thing$player)) specials_teleport(s, line, side, thing)
  }
  if (once) line$special <- 0L
  invisible()
}

specials_teleport <- function(s, line, side, thing) {
  if (side == 1L || has_flag(thing$flags, MF_MISSILE)) return(invisible())
  for (sector in s$world$sectors) {
    if (sector$tag != line$tag) next
    for (dest in s$world$mobjs) {
      if (is.null(dest$doomed) || dest$doomed != 14L) next
      there <- point_in_subsector(s$world, dest$x, dest$y)$sector
      if (!identical(there, sector)) next
      thing$momx <- 0
      thing$momy <- 0
      thing$momz <- 0
      unset_thing_position(s$world, thing)
      thing$x <- dest$x
      thing$y <- dest$y
      thing$angle <- dest$angle
      ss <- point_in_subsector(s$world, thing$x, thing$y)
      thing$floorz <- ss$sector$floorheight
      thing$ceilingz <- ss$sector$ceilingheight
      thing$z <- thing$floorz
      set_thing_position(s$world, thing)
      if (!is.null(thing$player)) {
        thing$player$viewz <- thing$z + thing$player$viewheight
        thing$player$reactiontime <- 18L
      }
      s$game$start_sound("telept")
      s$world$dirty <- TRUE
      return(invisible())
    }
  }
  invisible()
}
