# Inimigos: olhar, perseguir, atacar e morrer (p_enemy / p_mobj).

MT_PLAYER <- 0L
MT_POSSESSED <- 1L
MT_SHOTGUY <- 2L
MT_VILE <- 3L
MT_FIRE <- 4L
MT_UNDEAD <- 5L
MT_TRACER <- 6L
MT_FATSO <- 8L
MT_FATSHOT <- 9L
MT_CHAINGUY <- 10L
MT_TROOP <- 11L
MT_SERGEANT <- 12L
MT_SHADOWS <- 13L
MT_HEAD <- 14L
MT_BRUISER <- 15L
MT_BRUISERSHOT <- 16L
MT_KNIGHT <- 17L
MT_SKULL <- 18L
MT_SPIDER <- 19L
MT_BABY <- 20L
MT_CYBORG <- 21L
MT_PAIN <- 22L
MT_KEEN <- 24L
MT_BOSSBRAIN <- 25L
MT_BOSSTARGET <- 27L
MT_SPAWNSHOT <- 28L
MT_BARREL <- 30L
MT_TROOPSHOT <- 31L
MT_HEADSHOT <- 32L
MT_ROCKET <- 33L
MT_PLASMA <- 34L
MT_BFG <- 35L
MT_CLIP <- 63L
MT_CHAINGUN <- 73L
MT_SHOTGUN <- 77L

S_NULL <- 0L
S_PLAY <- 149L
S_PLAY_RUN1 <- 150L
S_VILE_HEAL1 <- 266L
S_SARG_RUN1 <- 477L
S_SARG_PAIN2 <- 489L

DI_EAST <- 0L
DI_NE <- 1L
DI_NORTH <- 2L
DI_NW <- 3L
DI_WEST <- 4L
DI_SW <- 5L
DI_SOUTH <- 6L
DI_SE <- 7L
DI_NODIR <- 8L
XSPEED <- c(FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000, 0, 47000)
YSPEED <- c(0, 47000, FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000)
OPPOSITE <- c(4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L, 8L)
DIAGS <- c(DI_NW, DI_NE, DI_SW, DI_SE)
FLOATSPEED <- 4 * FRACUNIT
SKULLSPEED <- 20 * FRACUNIT
FATSPREAD <- ANG90 / 8
TRACEANGLE <- 201326592
MAX_SKULLS <- 21L

.fast_on <- NULL
.fast_base <- NULL
.shot_base <- NULL
.doomed_type <- NULL
.brain_targets <- list()
.brain_on <- 0L

mi <- function(mo) MOBJINFO[[mo$type + 1L]]

mark_dirty <- function(world) {
  if (!is.null(world)) world$dirty <- TRUE
}

same_species <- function(target, other) {
  if (is.null(target$type) || is.null(other$type)) return(FALSE)
  if (target$type == other$type) return(TRUE)
  if (target$type == MT_KNIGHT && other$type == MT_BRUISER) return(TRUE)
  target$type == MT_BRUISER && other$type == MT_KNIGHT
}

type_for_doomed <- function(n) {
  if (is.null(.doomed_type)) {
    env <- new.env(parent = emptyenv())
    for (i in seq_along(MOBJINFO)) {
      key <- as.character(MOBJINFO[[i]]$doomed)
      if (MOBJINFO[[i]]$doomed >= 0 && !exists(key, env, inherits = FALSE)) env[[key]] <- i - 1L
    }
    .doomed_type <<- env
  }
  key <- as.character(n)
  if (!exists(key, .doomed_type, inherits = FALSE)) return(NULL)
  .doomed_type[[key]]
}

apply_fast <- function(game) {
  want <- isTRUE(game$skill == SK_NIGHTMARE)
  idx <- (S_SARG_RUN1:S_SARG_PAIN2) + 1L
  if (is.null(.fast_base)) {
    .fast_base <<- STATES[idx, 3]
    .shot_base <<- c(
      MOBJINFO[[MT_BRUISERSHOT + 1L]]$speed,
      MOBJINFO[[MT_HEADSHOT + 1L]]$speed,
      MOBJINFO[[MT_TROOPSHOT + 1L]]$speed
    )
  }
  if (isTRUE(.fast_on) == want) return(invisible())
  .fast_on <<- want
  STATES[idx, 3] <<- if (want) pmax(1L, .fast_base %/% 2L) else .fast_base
  speeds <- if (want) rep(20 * FRACUNIT, 3) else .shot_base
  types <- c(MT_BRUISERSHOT, MT_HEADSHOT, MT_TROOPSHOT)
  for (i in seq_along(types)) {
    info <- MOBJINFO[[types[i] + 1L]]
    info$speed <- speeds[i]
    MOBJINFO[[types[i] + 1L]] <<- info
  }
  invisible()
}

prepare_mo <- function(mo, typ, game) {
  info <- MOBJINFO[[typ + 1L]]
  mo$type <- typ
  mo$damage <- info$damage
  mo$alive <- TRUE
  mo$target <- NULL
  mo$tracer <- NULL
  mo$movedir <- DI_NODIR
  mo$movecount <- 0L
  mo$threshold <- 0L
  mo$lastlook <- p_random() %% 4L
  mo$reactiontime <- if (!is.null(game) && game$skill != SK_NIGHTMARE) info$reactiontime else 0L
  mo
}

enemy_init <- function(game) {
  apply_fast(game)
  game$respawnmonsters <- isTRUE(game$skill == SK_NIGHTMARE)
  for (mo in game$world$mobjs) {
    if (!is.null(mo$player)) {
      mo$type <- MT_PLAYER
      mo$alive <- TRUE
      mo$damage <- 0
      next
    }
    if (is.null(mo$doomed)) next
    typ <- type_for_doomed(mo$doomed)
    if (is.null(typ)) next
    prepare_mo(mo, typ, game)
    st <- MOBJINFO[[typ + 1L]]$spawnstate
    if (st != S_NULL) set_mobj_state(mo, st, game$world, game)
  }
  invisible(game)
}

set_mobj_state <- function(mo, state, world, game) {
  safety <- 0L
  repeat {
    if (is.null(mo) || state == S_NULL) {
      if (!is.null(mo) && !is.null(world)) remove_mobj(world, mo)
      return(FALSE)
    }
    row <- STATES[state + 1L, ]
    mo$istate <- state
    mo$tics <- row[3]
    mo$sprite <- SPRNAMES[row[1] + 1L]
    mo$frame <- row[2]
    act <- ACTIONS[row[4] + 1L]
    if (!is.na(act) && nzchar(act)) {
      tryCatch(call_action(act, mo, world, game), error = function(e) {
        message(act, ": ", conditionMessage(e), " em ", paste(deparse(conditionCall(e)), collapse = " "))
      })
      if (!isTRUE(mo$alive)) return(FALSE)
    }
    state <- row[5]
    if (mo$tics != 0) {
      mark_dirty(world)
      return(TRUE)
    }
    safety <- safety + 1L
    if (safety > 100L) return(TRUE)
  }
}

mobj_thinker <- function(world, mo, game) {
  if (isFALSE(mo$alive)) return(invisible())
  if ((is.null(mo$momx) || mo$momx != 0) || (is.null(mo$momy) || mo$momy != 0) || has_flag(mo$flags, MF_SKULLFLY)) {
    if (!is.null(mo$momx) && (mo$momx != 0 || mo$momy != 0 || has_flag(mo$flags, MF_SKULLFLY))) {
      p_xy_movement(world, mo, game)
      if (!isTRUE(mo$alive)) return(invisible())
    }
  }
  if (!is.null(mo$z) && (mo$z != mo$floorz || mo$momz != 0)) {
    mobj_z(mo, world, game)
    if (!isTRUE(mo$alive)) return(invisible())
  }
  if (is.null(mo$tics) || mo$tics != -1) {
    mo$tics <- mo$tics - 1
    if (mo$tics <= 0 && !is.null(mo$istate)) set_mobj_state(mo, STATES[mo$istate + 1L, 5], world, game)
    return(invisible())
  }
  if (!has_flag(mo$flags, MF_COUNTKILL) || !isTRUE(game$respawnmonsters)) return(invisible())
  mo$movecount <- mo$movecount + 1L
  if (mo$movecount < 12 * TICRATE) return(invisible())
  if (bitwAnd(as.integer(game$leveltime), 31L) != 0L) return(invisible())
  if (p_random() > 4) return(invisible())
  nightmare_respawn(world, mo, game)
  invisible()
}

tick_enemies <- function(world, game) {
  player <- game$player
  if (is.null(player) || is.null(player$mo)) return(invisible())
  for (mo in world$mobjs) {
    if (identical(mo, player$mo) || is.null(mo$istate)) next
    mobj_thinker(world, mo, game)
  }
  invisible()
}

spawn_mobj <- function(world, x, y, z, typ, game = NULL) {
  info <- MOBJINFO[[typ + 1L]]
  sub <- point_in_subsector(world, x, y)
  mo <- new.env(parent = emptyenv())
  mo$x <- x
  mo$y <- y
  mo$floorz <- sub$sector$floorheight
  mo$ceilingz <- sub$sector$ceilingheight
  mo$radius <- info$radius
  mo$height <- info$height
  mo$flags <- info$flags
  mo$health <- info$spawnhealth
  mo$sprite <- ""
  mo$frame <- 0L
  mo$angle <- 0
  mo$momx <- 0
  mo$momy <- 0
  mo$momz <- 0
  mo$player <- NULL
  mo$fx <- FALSE
  mo$bnext <- NULL
  mo$bprev <- NULL
  mo$blocklinked <- FALSE
  mo$doomed <- info$doomed
  prepare_mo(mo, typ, game)
  if (is.null(z)) {
    mo$z <- if (has_flag(info$flags, MF_SPAWNCEILING)) mo$ceilingz - mo$height else mo$floorz
  } else {
    mo$z <- z
  }
  world$mobjs[[length(world$mobjs) + 1L]] <- mo
  set_thing_position(world, mo)
  set_mobj_state(mo, info$spawnstate, world, game)
  mo
}

nightmare_respawn <- function(world, mo, game) {
  if (is.null(mo$spawn_x)) return(invisible())
  x <- mo$spawn_x * FRACUNIT
  y <- mo$spawn_y * FRACUNIT
  chk <- check_position(world, mo, x, y)
  if (chk$blocked) return(invisible())
  spawn_mobj(world, mo$x, mo$y, mo$floorz, MT_TFOG, game)
  game$start_sound("telept")
  sub <- point_in_subsector(world, x, y)
  spawn_mobj(world, x, y, sub$sector$floorheight, MT_TFOG, game)
  game$start_sound("telept")
  spawned <- spawn_mobj(world, x, y, NULL, mo$type, game)
  spawned$spawn_x <- mo$spawn_x
  spawned$spawn_y <- mo$spawn_y
  spawned$spawn_angle <- mo$spawn_angle
  spawned$spawn_options <- mo$spawn_options
  spawned$angle <- as_u32((mo$spawn_angle %/% 45) * 536870912)
  if (!is.null(mo$spawn_options) && bitwAnd(as.integer(mo$spawn_options), 8L) != 0L) {
    spawned$flags <- bitwOr(as.integer(spawned$flags), MF_AMBUSH)
  }
  spawned$reactiontime <- 18L
  remove_mobj(world, mo)
  invisible()
}

MT_TFOG <- 39L

kill_monster <- function(mo, game, source) {
  if (is.null(mo$type)) return(invisible())
  info <- mi(mo)
  mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_SHOOTABLE + MF_FLOAT + MF_SKULLFLY))
  mo$flags <- bitwOr(as.integer(mo$flags), MF_CORPSE + MF_DROPOFF)
  mo$height <- shar(mo$height, 2L)
  if (!is.null(source) && !is.null(source$player) && has_flag(mo$flags, MF_COUNTKILL)) {
    source$player$killcount <- source$player$killcount + 1L
  }
  st <- info$deathstate
  if (mo$health < -info$spawnhealth && info$xdeathstate) st <- info$xdeathstate
  set_mobj_state(mo, st, game$world, game)
  if (isTRUE(mo$alive)) {
    mo$tics <- mo$tics - bitwAnd(p_random(), 3L)
    if (mo$tics < 1) mo$tics <- 1L
    drop <- if (mo$type == MT_POSSESSED) MT_CLIP else if (mo$type == MT_SHOTGUY) MT_SHOTGUN else if (mo$type == MT_CHAINGUY) MT_CHAINGUN else NULL
    if (!is.null(drop)) {
      item <- spawn_mobj(game$world, mo$x, mo$y, NULL, drop, game)
      item$flags <- bitwOr(as.integer(item$flags), MF_DROPPED)
    }
  }
  invisible()
}

pain_or_wake <- function(target, source, game) {
  if (is.null(target$type)) return(invisible())
  info <- mi(target)
  if (p_random() < info$painchance && !has_flag(target$flags, MF_SKULLFLY)) {
    target$flags <- bitwOr(as.integer(target$flags), MF_JUSTHIT)
    if (info$painstate) set_mobj_state(target, info$painstate, game$world, game)
  }
  target$reactiontime <- 0L
  if (!is.null(source) && !identical(source, target) && is.null(target$player)) {
    target$target <- source
    if (!is.null(target$istate) && target$istate == info$spawnstate && info$seestate) {
      set_mobj_state(target, info$seestate, game$world, game)
    }
  }
  invisible()
}

noise_alert <- function(world, emitter, game = NULL) {
  if (is.null(emitter) || is.null(world)) return(invisible())
  sec <- point_in_subsector(world, emitter$x, emitter$y)$sector
  world$validcount <- world$validcount + 1
  recursive_sound(world, sec, 0L, emitter)
  invisible()
}

recursive_sound <- function(world, sec, soundblocks, target) {
  if (is.null(sec$validcount)) sec$validcount <- -1
  if (is.null(sec$soundtraversed)) sec$soundtraversed <- 0L
  if (sec$validcount == world$validcount && sec$soundtraversed <= soundblocks + 1L) return(invisible())
  sec$validcount <- world$validcount
  sec$soundtraversed <- soundblocks + 1L
  sec$soundtarget <- target
  for (check in sec$lines) {
    if (!has_flag(check$flags, ML_TWOSIDED)) next
    open <- line_opening(check)
    if (open[1] - open[2] <= 0) next
    other <- if (identical(check$frontsector, sec)) check$backsector else check$frontsector
    if (is.null(other)) next
    if (has_flag(check$flags, ML_SOUNDBLOCK)) {
      if (soundblocks == 0L) recursive_sound(world, other, 1L, target)
    } else {
      recursive_sound(world, other, soundblocks, target)
    }
  }
  invisible()
}

play_see_sound <- function(mo, game) {
  s <- mi(mo)$seesound
  if (!nzchar(s)) return(invisible())
  if (s == "posit1") s <- c("posit1", "posit2", "posit3")[p_random() %% 3 + 1]
  else if (s == "bgsit1") s <- c("bgsit1", "bgsit2")[p_random() %% 2 + 1]
  game$start_sound(s)
}

play_death_sound <- function(mo, game) {
  s <- mi(mo)$deathsound
  if (!nzchar(s)) return(invisible())
  if (s == "podth1") s <- c("podth1", "podth2", "podth3")[p_random() %% 3 + 1]
  else if (s == "bgdth1") s <- c("bgdth1", "bgdth2")[p_random() %% 2 + 1]
  game$start_sound(s)
}

look_for_players <- function(world, mo, player_mo, allaround) {
  if (is.null(player_mo)) return(FALSE)
  c <- 0L
  stop <- bitwAnd(mo$lastlook - 1L, 3L)
  repeat {
    if (mo$lastlook != 0L) {
      mo$lastlook <- bitwAnd(mo$lastlook + 1L, 3L)
      next
    }
    if (c == 2L || mo$lastlook == stop) return(FALSE)
    c <- c + 1L
    if (player_mo$health <= 0 || !has_flag(player_mo$flags, MF_SHOOTABLE)) {
      mo$lastlook <- bitwAnd(mo$lastlook + 1L, 3L)
      next
    }
    if (!check_sight(world, mo, player_mo)) {
      mo$lastlook <- bitwAnd(mo$lastlook + 1L, 3L)
      next
    }
    if (!allaround) {
      an <- as_u32(angle_to(mo$x, mo$y, player_mo$x, player_mo$y) - mo$angle)
      if (an > ANG90 && an < ANG270) {
        dist <- approx_distance(player_mo$x - mo$x, player_mo$y - mo$y)
        if (dist > MELEERANGE) {
          mo$lastlook <- bitwAnd(mo$lastlook + 1L, 3L)
          next
        }
      }
    }
    mo$target <- player_mo
    return(TRUE)
  }
}

enemy_look <- function(world, mo, player_mo, game) {
  mo$threshold <- 0L
  see <- FALSE
  sec <- point_in_subsector(world, mo$x, mo$y)$sector
  targ <- sec$soundtarget
  if (!is.null(targ) && has_flag(targ$flags, MF_SHOOTABLE)) {
    mo$target <- targ
    see <- if (has_flag(mo$flags, MF_AMBUSH)) check_sight(world, mo, targ) else TRUE
  }
  if (!see && !look_for_players(world, mo, player_mo, FALSE)) return(invisible())
  mo$movedir <- DI_NODIR
  mo$movecount <- 0L
  play_see_sound(mo, game)
  set_mobj_state(mo, mi(mo)$seestate, world, game)
}

face_target <- function(mo, target) {
  mo$angle <- angle_to(mo$x, mo$y, target$x, target$y)
  if (has_flag(target$flags, MF_SHADOW)) mo$angle <- as_u32(mo$angle + (p_random() - p_random()) * 2097152)
  mark_dirty(mo$world)
}

face_movedir <- function(mo) {
  if (mo$movedir < 0L || mo$movedir >= 8L) return(invisible())
  mo$angle <- u32_and(mo$angle, 3758096384)
  delta <- as_i32(mo$angle - mo$movedir * ANG45)
  if (delta > 0) mo$angle <- as_u32(mo$angle - ANG45)
  else if (delta < 0) mo$angle <- as_u32(mo$angle + ANG45)
  invisible()
}

enemy_move <- function(world, mo, speed, game) {
  if (mo$movedir < 0L || mo$movedir >= 8L) return(FALSE)
  dir <- mo$movedir + 1L
  nx <- mo$x + speed * XSPEED[dir]
  ny <- mo$y + speed * YSPEED[dir]
  if (!try_move(world, mo, nx, ny, game)) {
    if (has_flag(mo$flags, MF_FLOAT) && isTRUE(world$floatok)) {
      if (mo$z < world$tmfloorz) mo$z <- mo$z + FLOATSPEED else mo$z <- mo$z - FLOATSPEED
      mo$flags <- bitwOr(as.integer(mo$flags), MF_INFLOAT)
      mark_dirty(world)
      return(TRUE)
    }
    hits <- world$last_spechit
    if (!length(hits)) return(FALSE)
    mo$movedir <- DI_NODIR
    good <- FALSE
    for (i in length(hits):1) {
      ln <- hits[[i]]
      if (ln$special && !is.null(game) && isTRUE(game$use_special(ln, mo, 0L))) good <- TRUE
    }
    return(good)
  }
  mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_INFLOAT))
  if (!has_flag(mo$flags, MF_FLOAT)) mo$z <- mo$floorz
  mark_dirty(world)
  TRUE
}

new_chase_dir <- function(world, mo, game) {
  target <- mo$target
  if (is.null(target)) return(invisible())
  old <- mo$movedir
  turn <- if (old >= 0L && old < 8L) OPPOSITE[old + 1L] else DI_NODIR
  dx <- target$x - mo$x
  dy <- target$y - mo$y
  d2 <- if (dx > 10 * FRACUNIT) DI_EAST else if (dx < -10 * FRACUNIT) DI_WEST else DI_NODIR
  d3 <- if (dy < -10 * FRACUNIT) DI_SOUTH else if (dy > 10 * FRACUNIT) DI_NORTH else DI_NODIR
  speed <- mi(mo)$speed
  if (d2 != DI_NODIR && d3 != DI_NODIR) {
    mo$movedir <- DIAGS[(if (dy < 0) 2 else 0) + (if (dx > 0) 1 else 0) + 1L]
    if (mo$movedir != turn && enemy_move(world, mo, speed, game)) {
      mo$movecount <- bitwAnd(p_random(), 15L)
      return(invisible())
    }
  }
  if (p_random() > 200 || abs(dy) > abs(dx)) {
    tmp <- d2
    d2 <- d3
    d3 <- tmp
  }
  if (d2 == turn) d2 <- DI_NODIR
  if (d3 == turn) d3 <- DI_NODIR
  if (d2 != DI_NODIR) {
    mo$movedir <- d2
    if (enemy_move(world, mo, speed, game)) {
      mo$movecount <- bitwAnd(p_random(), 15L)
      return(invisible())
    }
  }
  if (d3 != DI_NODIR) {
    mo$movedir <- d3
    if (enemy_move(world, mo, speed, game)) {
      mo$movecount <- bitwAnd(p_random(), 15L)
      return(invisible())
    }
  }
  if (old != DI_NODIR) {
    mo$movedir <- old
    if (enemy_move(world, mo, speed, game)) {
      mo$movecount <- bitwAnd(p_random(), 15L)
      return(invisible())
    }
  }
  start <- bitwAnd(p_random(), 1L)
  dirs <- if (start) 0:7 else 7:0
  for (tdir in dirs) {
    if (tdir != turn) {
      mo$movedir <- tdir
      if (enemy_move(world, mo, speed, game)) {
        mo$movecount <- bitwAnd(p_random(), 15L)
        return(invisible())
      }
    }
  }
  if (turn != DI_NODIR) {
    mo$movedir <- turn
    if (enemy_move(world, mo, speed, game)) {
      mo$movecount <- bitwAnd(p_random(), 15L)
      return(invisible())
    }
  }
  mo$movedir <- DI_NODIR
  mo$movecount <- bitwAnd(p_random(), 15L)
  invisible()
}

missile_ok <- function(world, mo, target, dist, has_melee) {
  if (!check_sight(world, mo, target)) return(FALSE)
  if (has_flag(mo$flags, MF_JUSTHIT)) {
    mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_JUSTHIT))
    return(TRUE)
  }
  if (mo$reactiontime) return(FALSE)
  d <- dist - 64 * FRACUNIT
  if (!has_melee) d <- d - 128 * FRACUNIT
  d <- shar(d, 16L)
  if (mo$type == MT_VILE && d > 14 * 64) return(FALSE)
  if (mo$type == MT_UNDEAD) {
    if (d < 196) return(FALSE)
    d <- shar(d, 1L)
  }
  if (mo$type %in% c(MT_CYBORG, MT_SPIDER, MT_SKULL)) d <- shar(d, 1L)
  if (d > 200) d <- 200
  if (mo$type == MT_CYBORG && d > 160) d <- 160
  p_random() >= d
}

enemy_chase <- function(world, mo, player_mo, game) {
  info <- mi(mo)
  if (mo$reactiontime) mo$reactiontime <- mo$reactiontime - 1L
  if (!is.null(mo$threshold) && mo$threshold) {
    target <- mo$target
    if (is.null(target) || target$health <= 0) mo$threshold <- 0L else mo$threshold <- mo$threshold - 1L
  }
  if (mo$movedir < 8L) face_movedir(mo)
  target <- mo$target
  if (is.null(target) || !has_flag(target$flags, MF_SHOOTABLE)) {
    if (look_for_players(world, mo, player_mo, TRUE)) return(invisible())
    set_mobj_state(mo, info$spawnstate, world, game)
    return(invisible())
  }
  if (has_flag(mo$flags, MF_JUSTATTACKED)) {
    mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_JUSTATTACKED))
    if (game$skill != SK_NIGHTMARE) new_chase_dir(world, mo, game)
    return(invisible())
  }
  dist <- approx_distance(target$x - mo$x, target$y - mo$y)
  melee_range <- MELEERANGE - 20 * FRACUNIT + target$radius
  if (info$meleestate && dist < melee_range && check_sight(world, mo, target)) {
    if (nzchar(info$attacksound)) game$start_sound(info$attacksound)
    set_mobj_state(mo, info$meleestate, world, game)
    return(invisible())
  }
  if (info$missilestate) {
    skip <- game$skill != SK_NIGHTMARE && mo$movecount != 0
    if (!skip && missile_ok(world, mo, target, dist, info$meleestate != 0)) {
      set_mobj_state(mo, info$missilestate, world, game)
      mo$flags <- bitwOr(as.integer(mo$flags), MF_JUSTATTACKED)
      return(invisible())
    }
  }
  mo$movecount <- mo$movecount - 1L
  if (mo$movecount < 0 || !enemy_move(world, mo, info$speed, game)) new_chase_dir(world, mo, game)
  if (nzchar(info$activesound) && p_random() < 3) game$start_sound(info$activesound)
  invisible()
}

check_missile_spawn <- function(mo) {
  mo$tics <- mo$tics - bitwAnd(p_random(), 3L)
  if (mo$tics < 1) mo$tics <- 1L
  mo$x <- mo$x + shar(mo$momx, 1L)
  mo$y <- mo$y + shar(mo$momy, 1L)
  mo$z <- mo$z + shar(mo$momz, 1L)
  invisible(mo)
}

spawn_player_missile <- function(world, source, typ, game) {
  info <- MOBJINFO[[typ + 1L]]
  speed <- info$speed
  aim <- bullet_aim(world, source, MISSILERANGE)
  ang <- aim$angle
  mo <- spawn_mobj(world, source$x, source$y, source$z + 32 * FRACUNIT, typ, game)
  mo$target <- source
  mo$angle <- ang
  mo$momx <- fixed_mul(speed, fine_cos(ang))
  mo$momy <- fixed_mul(speed, fine_sin(ang))
  mo$momz <- fixed_mul(speed, aim$slope)
  check_missile_spawn(mo)
  mo
}

spawn_missile_mt <- function(world, source, dest, typ, game, ang = NULL) {
  info <- MOBJINFO[[typ + 1L]]
  speed <- info$speed
  if (is.null(dest)) dest <- source
  if (is.null(ang)) {
    ang <- angle_to(source$x, source$y, dest$x, dest$y)
    if (has_flag(dest$flags, MF_SHADOW)) ang <- as_u32(ang + (p_random() - p_random()) * 1048576)
  }
  dist <- approx_distance(dest$x - source$x, dest$y - source$y)
  steps <- if (speed) dist %/% speed else 1
  if (steps < 1) steps <- 1
  mo <- spawn_mobj(world, source$x, source$y, source$z + 32 * FRACUNIT, typ, game)
  mo$target <- source
  mo$angle <- ang
  mo$momx <- fixed_mul(speed, fine_cos(ang))
  mo$momy <- fixed_mul(speed, fine_sin(ang))
  mo$momz <- trunc((dest$z - source$z) / steps)
  check_missile_spawn(mo)
  mo
}

explode_missile <- function(world, mo, game, hit) {
  if (!is.null(hit) && !is.null(game)) {
    src <- if (!is.null(mo$target)) mo$target else mo
    dmg <- if (!is.null(mo$damage) && mo$damage) mo$damage else mi(mo)$damage
    game$damage_mobj(hit, src, dmg * ((p_random() %% 8) + 1), mo)
  }
  mo$momx <- 0
  mo$momy <- 0
  mo$momz <- 0
  mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_MISSILE))
  set_mobj_state(mo, mi(mo)$deathstate, world, game)
}

p_xy_movement <- function(world, mo, game) {
  if (mo$momx == 0 && mo$momy == 0) {
    if (has_flag(mo$flags, MF_SKULLFLY)) {
      mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_SKULLFLY))
      mo$momx <- 0
      mo$momy <- 0
      mo$momz <- 0
      set_mobj_state(mo, mi(mo)$spawnstate, world, game)
    }
    return(invisible())
  }
  if (mo$momx > MAXMOVE) mo$momx <- MAXMOVE else if (mo$momx < -MAXMOVE) mo$momx <- -MAXMOVE
  if (mo$momy > MAXMOVE) mo$momy <- MAXMOVE else if (mo$momy < -MAXMOVE) mo$momy <- -MAXMOVE
  xmove <- mo$momx
  ymove <- mo$momy
  half <- MAXMOVE / 2
  steps <- 0L
  while ((xmove != 0 || ymove != 0) && steps < 64L) {
    steps <- steps + 1L
    if (xmove > half || ymove > half) {
      ptryx <- mo$x + trunc2(xmove)
      ptryy <- mo$y + trunc2(ymove)
      xmove <- trunc2(xmove)
      ymove <- trunc2(ymove)
    } else {
      ptryx <- mo$x + xmove
      ptryy <- mo$y + ymove
      xmove <- 0
      ymove <- 0
    }
    if (has_flag(mo$flags, MF_NOCLIP)) {
      unset_thing_position(world, mo)
      mo$x <- ptryx
      mo$y <- ptryy
      set_thing_position(world, mo)
      next
    }
    if (try_move(world, mo, ptryx, ptryy, game)) next
    if (!is.null(mo$player)) {
      slide_move(world, mo, mo$momx, mo$momy, game)
    } else if (has_flag(mo$flags, MF_MISSILE)) {
      line <- world$ceilingline
      sky <- if (!is.null(game) && !is.null(game$res)) game$res$skyflatnum else -1L
      if (!is.null(line) && !is.null(line$backsector) && line$backsector$ceilingpic == sky) {
        remove_mobj(world, mo)
        return(invisible())
      }
      explode_missile(world, mo, game, NULL)
      return(invisible())
    } else {
      mo$momx <- 0
      mo$momy <- 0
    }
  }
  if (!isTRUE(mo$alive)) return(invisible())
  mark_dirty(world)
  if (has_flag(mo$flags, MF_MISSILE + MF_SKULLFLY)) return(invisible())
  if (mo$z > mo$floorz) return(invisible())
  if (abs(mo$momx) < STOPSPEED && abs(mo$momy) < STOPSPEED) {
    mo$momx <- 0
    mo$momy <- 0
  } else {
    mo$momx <- fixed_mul(mo$momx, FRICTION)
    mo$momy <- fixed_mul(mo$momy, FRICTION)
  }
  invisible()
}

mobj_z <- function(mo, world, game) {
  mo$z <- mo$z + mo$momz
  if (has_flag(mo$flags, MF_FLOAT) && !is.null(mo$target) && !has_flag(mo$flags, MF_SKULLFLY + MF_INFLOAT)) {
    dist <- approx_distance(mo$x - mo$target$x, mo$y - mo$target$y)
    delta <- (mo$target$z + shar(mo$height, 1L)) - mo$z
    if (delta < 0 && dist < -(delta * 3)) mo$z <- mo$z - FLOATSPEED
    else if (delta > 0 && dist < (delta * 3)) mo$z <- mo$z + FLOATSPEED
  }
  if (mo$z <= mo$floorz) {
    if (mo$momz < 0) mo$momz <- 0
    mo$z <- mo$floorz
    if (has_flag(mo$flags, MF_SKULLFLY) && !has_flag(mo$flags, MF_MISSILE)) mo$momz <- -mo$momz
    if (has_flag(mo$flags, MF_MISSILE) && !has_flag(mo$flags, MF_NOCLIP)) {
      explode_missile(world, mo, game, NULL)
      return(invisible())
    }
  } else if (!has_flag(mo$flags, MF_NOGRAVITY)) {
    if (mo$momz == 0) mo$momz <- -GRAVITY * 2 else mo$momz <- mo$momz - GRAVITY
  }
  if (mo$z + mo$height > mo$ceilingz) {
    if (mo$momz > 0) mo$momz <- 0
    mo$z <- mo$ceilingz - mo$height
    if (has_flag(mo$flags, MF_SKULLFLY)) mo$momz <- -mo$momz
    if (has_flag(mo$flags, MF_MISSILE) && !has_flag(mo$flags, MF_NOCLIP)) {
      explode_missile(world, mo, game, NULL)
      return(invisible())
    }
  }
  mark_dirty(world)
  invisible()
}

radius_attack <- function(world, spot, source, damage, game) {
  for (other in world$mobjs) {
    if (identical(other, spot) || !has_flag(other$flags, MF_SHOOTABLE)) next
    if (!is.null(other$type) && other$type %in% c(MT_CYBORG, MT_SPIDER)) next
    dx <- abs(other$x - spot$x)
    dy <- abs(other$y - spot$y)
    dist <- (if (dx > dy) dx else dy) - other$radius
    if (dist < 0) dist <- 0
    dist <- shar(dist, 16L)
    if (dist >= damage) next
    if (check_sight(world, other, spot)) {
      who <- if (!is.null(source)) source else spot
      game$damage_mobj(other, who, damage - dist, spot)
    }
  }
  invisible()
}

skull_attack <- function(mo, game) {
  dest <- mo$target
  if (is.null(dest)) return(invisible())
  mo$flags <- bitwOr(as.integer(mo$flags), MF_SKULLFLY)
  game$start_sound("sklatk")
  face_target(mo, dest)
  mo$momx <- fixed_mul(SKULLSPEED, fine_cos(mo$angle))
  mo$momy <- fixed_mul(SKULLSPEED, fine_sin(mo$angle))
  dist <- approx_distance(dest$x - mo$x, dest$y - mo$y)
  steps <- if (SKULLSPEED) dist %/% SKULLSPEED else 1
  if (steps < 1) steps <- 1
  mo$momz <- trunc((dest$z + shar(dest$height, 1L) - mo$z) / steps)
  invisible()
}

tag_line <- function(tag) {
  ln <- new.env(parent = emptyenv())
  ln$tag <- tag
  ln$sides <- list(NULL)
  ln
}

alive_of_type <- function(world, typ) {
  for (other in world$mobjs) if (!is.null(other$type) && other$type == typ && other$health > 0) return(TRUE)
  FALSE
}

boss_death <- function(world, mo, game) {
  if (alive_of_type(world, mo$type) || is.null(game$specials)) return(invisible())
  commercial <- wad_check_num(game$wad, "MAP01") >= 0L
  if (commercial && game$mapn == 7L) {
    if (mo$type == MT_FATSO) do_floor(game$specials, tag_line(666L), spec_lowest_floor, -1L)
    else if (mo$type == MT_BABY) raise_to_texture(game$specials, tag_line(667L))
    return(invisible())
  }
  if (commercial) return(invisible())
  if (game$episode == 1L && game$mapn == 8L && mo$type == MT_BRUISER) {
    do_floor(game$specials, tag_line(666L), spec_lowest_floor, -1L)
  } else if (game$episode == 2L && game$mapn == 8L && mo$type == MT_CYBORG) {
    game$specials$exit_requested <- TRUE
  } else if (game$episode == 3L && game$mapn == 8L && mo$type == MT_SPIDER) {
    game$specials$exit_requested <- TRUE
  } else if (game$episode == 4L && game$mapn == 6L && mo$type == MT_CYBORG) {
    do_floor(game$specials, tag_line(666L), spec_lowest_floor, -1L)
  } else if (game$episode == 4L && game$mapn == 8L && mo$type == MT_BRUISER) {
    do_floor(game$specials, tag_line(666L), spec_lowest_floor, -1L)
  }
  invisible()
}

hitscan_attack <- function(world, mo, game, pellets, sound) {
  if (is.null(mo$target)) return(invisible())
  face_target(mo, mo$target)
  game$start_sound(sound)
  saved <- mo$angle
  slope <- aim_slope(world, mo, saved, MISSILERANGE)
  for (i in seq_len(pellets)) {
    mo$angle <- as_u32(saved + ((p_random() - p_random()) * 1048576))
    line_attack(world, mo, ((p_random() %% 5) + 1) * 3, game, MISSILERANGE, mo$angle, slope)
  }
  mo$angle <- saved
  invisible()
}

call_action <- function(name, mo, world, game) {
  pl <- if (!is.null(game) && !is.null(game$player)) game$player$mo else NULL
  if (name == "Look") enemy_look(world, mo, pl, game)
  else if (name == "Chase") enemy_chase(world, mo, pl, game)
  else if (name == "FaceTarget") { if (!is.null(mo$target)) face_target(mo, mo$target) }
  else if (name == "Fall") mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_SOLID))
  else if (name == "Scream") play_death_sound(mo, game)
  else if (name == "XScream") game$start_sound("slop")
  else if (name == "Pain") { if (nzchar(mi(mo)$painsound)) game$start_sound(mi(mo)$painsound) }
  else if (name == "Explode") radius_attack(world, mo, mo$target, 128, game)
  else if (name == "PosAttack") hitscan_attack(world, mo, game, 1L, "pistol")
  else if (name == "SPosAttack") hitscan_attack(world, mo, game, 3L, "shotgn")
  else if (name == "CPosAttack") hitscan_attack(world, mo, game, 1L, "shotgn")
  else if (name == "CPosRefire" || name == "SpidRefire") {
    if (!is.null(mo$target)) face_target(mo, mo$target)
    keep <- if (name == "CPosRefire") 40 else 10
    if (p_random() >= keep && (is.null(mo$target) || mo$target$health <= 0 || !check_sight(world, mo, mo$target))) {
      set_mobj_state(mo, mi(mo)$seestate, world, game)
    }
  }
  else if (name == "TroopAttack" || name == "HeadAttack" || name == "BruisAttack") {
    if (is.null(mo$target)) return(invisible())
    face_target(mo, mo$target)
    dist <- approx_distance(mo$target$x - mo$x, mo$target$y - mo$y)
    if (dist < MELEERANGE + mo$radius) {
      if (name == "TroopAttack") game$start_sound("claw")
      mult <- if (name == "TroopAttack") 3 else 10
      game$damage_mobj(mo$target, mo, ((p_random() %% 8) + 1) * mult)
    } else {
      typ <- if (name == "TroopAttack") MT_TROOPSHOT else if (name == "HeadAttack") MT_HEADSHOT else MT_BRUISERSHOT
      spawn_missile_mt(world, mo, mo$target, typ, game)
    }
  }
  else if (name == "SargAttack") {
    if (is.null(mo$target)) return(invisible())
    face_target(mo, mo$target)
    dist <- approx_distance(mo$target$x - mo$x, mo$target$y - mo$y)
    if (dist < MELEERANGE + mo$radius) game$damage_mobj(mo$target, mo, ((p_random() %% 8) + 1) * 4)
  }
  else if (name == "SkullAttack") skull_attack(mo, game)
  else if (name == "CyberAttack") {
    if (!is.null(mo$target)) {
      face_target(mo, mo$target)
      spawn_missile_mt(world, mo, mo$target, MT_ROCKET, game)
    }
  }
  else if (name == "Metal") { game$start_sound("metal"); enemy_chase(world, mo, pl, game) }
  else if (name == "Hoof") { game$start_sound("hoof"); enemy_chase(world, mo, pl, game) }
  else if (name == "BabyMetal") { game$start_sound("bspwlk"); enemy_chase(world, mo, pl, game) }
  else if (name == "BossDeath") boss_death(world, mo, game)
  else if (name == "KeenDie") {
    mo$flags <- bitwAnd(as.integer(mo$flags), bitwNot(MF_SOLID))
    if (!alive_of_type(world, MT_KEEN) && !is.null(game$specials)) do_door(game$specials, tag_line(666L), VLD_BLAZEOPEN)
  }
  else if (name == "PlayerScream") { if (!is.null(game)) game$start_sound("pldeth") }
  invisible()
}
