# Jogador: andar, gravidade, usar, atirar e trocar de arma.

RNDTABLE <- c(
  0, 8, 109, 220, 222, 241, 149, 107, 75, 248, 254, 140, 16, 66,
  74, 21, 211, 47, 80, 242, 154, 27, 205, 128, 161, 89, 77, 36,
  95, 110, 85, 48, 212, 140, 211, 249, 22, 79, 200, 50, 28, 188,
  52, 140, 202, 120, 68, 145, 62, 70, 184, 190, 91, 197, 152, 224,
  149, 104, 25, 178, 252, 182, 202, 182, 141, 197, 4, 81, 181, 242,
  145, 42, 39, 227, 156, 198, 225, 193, 219, 93, 122, 175, 249, 0,
  175, 143, 70, 239, 46, 246, 163, 53, 163, 109, 168, 135, 2, 235,
  25, 92, 20, 145, 138, 77, 69, 166, 78, 176, 173, 212, 166, 113,
  94, 161, 41, 50, 239, 49, 111, 164, 70, 60, 2, 37, 171, 75,
  136, 156, 11, 56, 42, 146, 138, 229, 73, 146, 77, 61, 98, 196,
  135, 106, 63, 197, 195, 86, 96, 203, 113, 101, 170, 247, 181, 113,
  80, 250, 108, 7, 255, 237, 129, 226, 79, 107, 112, 166, 103, 241,
  24, 223, 239, 120, 198, 58, 60, 82, 128, 3, 184, 66, 143, 224,
  145, 224, 81, 206, 163, 45, 63, 90, 168, 114, 59, 33, 159, 95,
  28, 139, 123, 98, 125, 196, 15, 70, 194, 253, 54, 14, 109, 226,
  71, 17, 161, 93, 186, 87, 244, 138, 20, 52, 123, 251, 26, 36,
  17, 46, 52, 231, 232, 76, 31, 221, 84, 37, 216, 165, 212, 106,
  197, 242, 98, 43, 39, 175, 254, 145, 190, 84, 118, 222, 187, 136,
  120, 163, 236, 249
)
prndindex <- 0L

p_random <- function() {
  prndindex <<- bitwAnd(prndindex + 1L, 255L)
  RNDTABLE[prndindex + 1L]
}

FORWARDMOVE <- c(25, 50)
SIDEMOVE <- c(24, 40)
ANGLETURN <- c(640, 1280, 320)

WEAPON_PATCH <- c("PUNGA0", "PISGA0", "SHTGA0", "CHGGA0", "MISGA0", "PLSGA0", "BFGGA0", "SAWGC0", "SHT2A0")
WEAPON_AMMO <- c(NA, AM_CLIP, AM_SHELL, AM_CLIP, AM_MISL, AM_CELL, AM_CELL, NA, AM_SHELL)

weapon_atk <- function(weapon) {
  if (weapon == WP_FIST) {
    list(
      c("PUNGB0", 4, 0), c("PUNGC0", 4, 1), c("PUNGD0", 5, 0),
      c("PUNGC0", 4, 0), c("PUNGB0", 5, 0)
    )
  } else if (weapon == WP_PISTOL) {
    list(
      c("PISGA0", 4, 0), c("PISGB0", 6, 1, "PISFA0", 7, 1),
      c("PISGC0", 4, 0), c("PISGB0", 5, 0)
    )
  } else if (weapon == WP_SHOTGUN) {
    list(
      c("SHTGA0", 3, 0), c("SHTGA0", 7, 1, "SHTFA0", 7, 1),
      c("SHTGB0", 5, 0), c("SHTGC0", 5, 0), c("SHTGD0", 4, 0),
      c("SHTGC0", 5, 0), c("SHTGB0", 5, 0), c("SHTGA0", 3, 0), c("SHTGA0", 7, 0)
    )
  } else if (weapon == WP_CHAINGUN) {
    list(c("CHGGA0", 4, 1, "CHGFA0", 5, 1), c("CHGGB0", 4, 1, "CHGFB0", 5, 2))
  } else if (weapon == WP_CHAINSAW) {
    list(c("SAWGA0", 4, 1), c("SAWGB0", 4, 1))
  } else if (weapon == WP_SUPERSHOTGUN) {
    list(
      c("SHT2A0", 3, 0), c("SHT2A0", 7, 1, "SHT2I0", 9, 1),
      c("SHT2B0", 7, 0), c("SHT2C0", 7, 0), c("SHT2D0", 7, 0),
      c("SHT2E0", 7, 0), c("SHT2F0", 7, 0), c("SHT2G0", 6, 0),
      c("SHT2H0", 6, 0), c("SHT2A0", 5, 0)
    )
  } else if (weapon == WP_MISSILE) {
    list(c("MISGB0", 8, 0, "MISFA0", 15, 1), c("MISGB0", 12, 1, "", 0, 2))
  } else if (weapon == WP_PLASMA) {
    list(c("PLSGA0", 3, 1, "PLSFA0", 4, 1), c("PLSGB0", 20, 0))
  } else if (weapon == WP_BFG) {
    list(
      c("BFGGA0", 20, 0), c("BFGGB0", 10, 0, "BFGFA0", 17, 1),
      c("BFGGB0", 10, 1, "", 0, 2), c("BFGGB0", 20, 0)
    )
  } else {
    list(c("PISGA0", 4, 0), c("PISGB0", 6, 1, "PISFA0", 7, 1))
  }
}

spawn_player <- function(world, start) {
  x <- start$x * FRACUNIT
  y <- start$y * FRACUNIT
  sub <- point_in_subsector(world, x, y)
  mo <- new.env(parent = emptyenv())
  mo$x <- x
  mo$y <- y
  mo$z <- sub$sector$floorheight
  mo$angle <- as_u32((start$angle %/% 45) * 536870912)
  mo$momx <- 0
  mo$momy <- 0
  mo$momz <- 0
  mo$radius <- PLAYER_RADIUS
  mo$height <- PLAYER_HEIGHT
  mo$floorz <- sub$sector$floorheight
  mo$ceilingz <- sub$sector$ceilingheight
  mo$flags <- MF_SOLID + MF_SHOOTABLE + MF_PICKUP + MF_DROPOFF
  mo$health <- 100
  mo$sprite <- "PLAY"
  mo$frame <- 0L
  mo$player <- NULL
  mo$tics <- 0L
  mo$bnext <- NULL
  mo$bprev <- NULL
  mo$blocklinked <- FALSE
  player <- new.env(parent = emptyenv())
  player$mo <- mo
  mo$player <- player
  player$cmd <- ticcmd_new()
  player$playerstate <- PST_LIVE
  player$viewz <- mo$z + VIEWHEIGHT
  player$viewheight <- VIEWHEIGHT
  player$deltaviewheight <- 0
  player$bob <- 0
  player$health <- 100
  player$armorpoints <- 0
  player$armortype <- 0
  player$ammo <- c(50, 0, 0, 0)
  player$maxammo <- c(200, 50, 300, 50)
  player$weaponowned <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  player$pendingweapon <- WP_NOCHANGE
  player$readyweapon <- WP_PISTOL
  player$cards <- rep(FALSE, 6L)
  player$cheats <- 0L
  player$attackdown <- FALSE
  player$usedown <- FALSE
  player$damagecount <- 0L
  player$bonuscount <- 0L
  player$message_tics <- 0L
  player$backpack <- FALSE
  player$itemcount <- 0L
  player$extralight <- 0L
  player$fixedcolormap <- 0L
  player$refire <- 0L
  player$killcount <- 0L
  player$secretcount <- 0L
  player$didsecret <- FALSE
  player$powers <- rep(0, 6L)
  player$psprite_sy <- WEAPONBOTTOM
  player$psprite_state <- "up"
  player$psprite_tics <- 0L
  player$psprite_step <- 0L
  player$psprite_body <- "PISGA0"
  player$psprite_flash <- ""
  player$flash_tics <- 0L
  player$message <- ""
  world$mobjs[[length(world$mobjs) + 1L]] <- mo
  set_thing_position(world, mo)
  player
}

ticcmd_new <- function() {
  e <- new.env(parent = emptyenv())
  e$forwardmove <- 0
  e$sidemove <- 0
  e$angleturn <- 0
  e$buttons <- 0L
  e
}

build_ticcmd <- function(game) {
  cmd <- ticcmd_new()
  speed <- if (key_down(game, "shift")) 2L else 1L
  strafe <- key_down(game, "alt")
  turning <- key_down(game, "right") || key_down(game, "left")
  game$turnheld <- if (turning) game$turnheld + 1L else 0L
  tspeed <- if (game$turnheld < 6L) 3L else speed
  if (strafe) {
    if (key_down(game, "right")) cmd$sidemove <- cmd$sidemove + SIDEMOVE[speed]
    if (key_down(game, "left")) cmd$sidemove <- cmd$sidemove - SIDEMOVE[speed]
  } else {
    if (key_down(game, "right")) cmd$angleturn <- cmd$angleturn - ANGLETURN[tspeed]
    if (key_down(game, "left")) cmd$angleturn <- cmd$angleturn + ANGLETURN[tspeed]
  }
  if (key_down(game, "up")) cmd$forwardmove <- cmd$forwardmove + FORWARDMOVE[speed]
  if (key_down(game, "down")) cmd$forwardmove <- cmd$forwardmove - FORWARDMOVE[speed]
  if (key_down(game, "comma")) cmd$sidemove <- cmd$sidemove - SIDEMOVE[speed]
  if (key_down(game, "period")) cmd$sidemove <- cmd$sidemove + SIDEMOVE[speed]
  if (key_down(game, "ctrl")) cmd$buttons <- bitwOr(cmd$buttons, BT_ATTACK)
  if (key_down(game, "space") || key_down(game, "e")) cmd$buttons <- bitwOr(cmd$buttons, BT_USE)
  w <- NA_integer_
  if (key_down(game, "1")) {
    w <- if (!is.null(game$player) && game$player$readyweapon == WP_CHAINSAW) {
      WP_FIST
    } else if (!is.null(game$player) && game$player$weaponowned[WP_CHAINSAW + 1L]) {
      WP_CHAINSAW
    } else {
      WP_FIST
    }
  } else if (key_down(game, "2")) w <- WP_PISTOL
  else if (key_down(game, "3")) w <- WP_SHOTGUN
  else if (key_down(game, "4")) w <- WP_CHAINGUN
  else if (key_down(game, "5")) w <- WP_MISSILE
  else if (key_down(game, "6")) w <- WP_PLASMA
  else if (key_down(game, "7")) w <- WP_BFG
  if (!is.na(w)) cmd$buttons <- bitwOr(cmd$buttons, bitwOr(BT_CHANGE, bitwShiftL(w, BT_WEAPONSHIFT)))
  cmd
}

thrust <- function(mo, angle, move) {
  mo$momx <- mo$momx + fixed_mul(move, fine_cos(angle))
  mo$momy <- mo$momy + fixed_mul(move, fine_sin(angle))
}

trunc2 <- function(n) {
  n <- as_i32(n)
  if (n < 0) -((-n) %/% 2) else n %/% 2
}

xy_movement <- function(world, mo, game) {
  if (mo$momx == 0 && mo$momy == 0) return(invisible())
  if (mo$momx > MAXMOVE) mo$momx <- MAXMOVE
  if (mo$momx < -MAXMOVE) mo$momx <- -MAXMOVE
  if (mo$momy > MAXMOVE) mo$momy <- MAXMOVE
  if (mo$momy < -MAXMOVE) mo$momy <- -MAXMOVE
  xmove <- mo$momx
  ymove <- mo$momy
  half <- MAXMOVE / 2
  while (xmove != 0 || ymove != 0) {
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
    if (try_move(world, mo, ptryx, ptryy, game)) next
    if (!is.null(mo$player)) {
      slide_move(world, mo, mo$momx, mo$momy, game)
    } else {
      mo$momx <- 0
      mo$momy <- 0
      break
    }
  }
  player <- mo$player
  if (mo$z > mo$floorz) return(invisible())
  if (abs(mo$momx) < STOPSPEED && abs(mo$momy) < STOPSPEED &&
      (is.null(player) || (player$cmd$forwardmove == 0 && player$cmd$sidemove == 0))) {
    mo$momx <- 0
    mo$momy <- 0
  } else {
    mo$momx <- fixed_mul(mo$momx, FRICTION)
    mo$momy <- fixed_mul(mo$momy, FRICTION)
  }
  invisible()
}

z_movement <- function(mo) {
  player <- mo$player
  if (!is.null(player) && mo$z < mo$floorz) {
    player$viewheight <- player$viewheight - (mo$floorz - mo$z)
    player$deltaviewheight <- shar(VIEWHEIGHT - player$viewheight, 3L)
  }
  mo$z <- mo$z + mo$momz
  if (mo$z <= mo$floorz) {
    if (mo$momz < 0) mo$momz <- 0
    mo$z <- mo$floorz
  } else if (!has_flag(mo$flags, MF_NOGRAVITY)) {
    if (mo$momz == 0) mo$momz <- -GRAVITY * 2 else mo$momz <- mo$momz - GRAVITY
  }
  if (mo$z + mo$height > mo$ceilingz) {
    if (mo$momz > 0) mo$momz <- 0
    mo$z <- mo$ceilingz - mo$height
  }
  invisible()
}

calc_height <- function(player, leveltime) {
  mo <- player$mo
  player$bob <- (fixed_mul(mo$momx, mo$momx) + fixed_mul(mo$momy, mo$momy)) / 4
  if (player$bob > MAXBOB) player$bob <- MAXBOB
  if (mo$z > mo$floorz) {
    player$viewz <- mo$z + player$viewheight
    if (is.finite(player$viewz) && is.finite(mo$ceilingz) && player$viewz > mo$ceilingz - 4 * FRACUNIT) {
      player$viewz <- mo$ceilingz - 4 * FRACUNIT
    }
    return(invisible())
  }
  angle <- bitwAnd(as.integer((FINEANGLES / 20) * leveltime), FINEMASK)
  bob <- fixed_mul(player$bob / 2, finesine[angle + 1L])
  if (player$playerstate == PST_LIVE) {
    player$viewheight <- player$viewheight + player$deltaviewheight
    if (player$viewheight > VIEWHEIGHT) {
      player$viewheight <- VIEWHEIGHT
      player$deltaviewheight <- 0
    }
    if (player$viewheight < VIEWHEIGHT / 2) {
      player$viewheight <- VIEWHEIGHT / 2
      if (player$deltaviewheight <= 0) player$deltaviewheight <- 1
    }
    if (player$deltaviewheight) player$deltaviewheight <- player$deltaviewheight + FRACUNIT / 4
  }
  player$viewz <- mo$z + player$viewheight + bob
  if (is.finite(player$viewz) && is.finite(mo$ceilingz) && player$viewz > mo$ceilingz - 4 * FRACUNIT) {
    player$viewz <- mo$ceilingz - 4 * FRACUNIT
  }
  invisible()
}

set_player_message <- function(player, text) {
  player$message <- text
  player$message_tics <- 4L * TICRATE
  invisible()
}

clip_rounds <- function(ammo, count) {
  sizes <- c(10, 4, 20, 1)
  sizes[ammo + 1L] * count
}

give_power <- function(player, power) {
  if (power == PW_INVULNERABILITY) {
    player$powers[power + 1L] <- 30L * TICRATE
    return(TRUE)
  }
  if (power == PW_INVISIBILITY) {
    player$powers[power + 1L] <- 60L * TICRATE
    if (!is.null(player$mo)) player$mo$flags <- bitwOr(as.integer(player$mo$flags), MF_SHADOW)
    return(TRUE)
  }
  if (power == PW_INFRARED) {
    player$powers[power + 1L] <- 120L * TICRATE
    return(TRUE)
  }
  if (power == PW_IRONFEET) {
    player$powers[power + 1L] <- 60L * TICRATE
    return(TRUE)
  }
  if (power == PW_STRENGTH) {
    if (player$health < 100) {
      player$health <- min(100, player$health + 100)
      if (!is.null(player$mo)) player$mo$health <- player$health
    }
    player$powers[power + 1L] <- 1
    return(TRUE)
  }
  if (player$powers[power + 1L]) return(FALSE)
  player$powers[power + 1L] <- 1
  TRUE
}

update_player_powers <- function(player) {
  if (player$powers[PW_STRENGTH + 1L]) player$powers[PW_STRENGTH + 1L] <- player$powers[PW_STRENGTH + 1L] + 1
  if (player$powers[PW_INVULNERABILITY + 1L]) {
    player$powers[PW_INVULNERABILITY + 1L] <- player$powers[PW_INVULNERABILITY + 1L] - 1
  }
  if (player$powers[PW_INVISIBILITY + 1L]) {
    player$powers[PW_INVISIBILITY + 1L] <- player$powers[PW_INVISIBILITY + 1L] - 1
    if (player$powers[PW_INVISIBILITY + 1L] == 0 && !is.null(player$mo)) {
      player$mo$flags <- bitwAnd(as.integer(player$mo$flags), bitwNot(MF_SHADOW))
    }
  }
  if (player$powers[PW_INFRARED + 1L]) player$powers[PW_INFRARED + 1L] <- player$powers[PW_INFRARED + 1L] - 1
  if (player$powers[PW_IRONFEET + 1L]) player$powers[PW_IRONFEET + 1L] <- player$powers[PW_IRONFEET + 1L] - 1
  inv <- player$powers[PW_INVULNERABILITY + 1L]
  ir <- player$powers[PW_INFRARED + 1L]
  if (inv) {
    player$fixedcolormap <- if (inv > 128 || bitwAnd(as.integer(inv), 8L) != 0L) INVERSECOLORMAP else 0L
  } else if (ir) {
    player$fixedcolormap <- if (ir > 128 || bitwAnd(as.integer(ir), 8L) != 0L) 1L else 0L
  } else {
    player$fixedcolormap <- 0L
  }
  if (player$damagecount > 0) player$damagecount <- player$damagecount - 1L
  if (player$bonuscount > 0) player$bonuscount <- player$bonuscount - 1L
  if (player$message_tics > 0) {
    player$message_tics <- player$message_tics - 1L
    if (player$message_tics <= 0L) player$message <- ""
  }
  invisible()
}

note_secret <- function(world, player) {
  mo <- player$mo
  if (is.null(mo)) return(invisible())
  sub <- point_in_subsector(world, mo$x, mo$y)
  sec <- if (is.null(sub)) NULL else sub$sector
  if (is.null(sec) || mo$z != sec$floorheight) return(invisible())
  if (as.integer(sec$special) == 9L) {
    player$secretcount <- player$secretcount + 1L
    sec$special <- 0L
  }
  invisible()
}

carry_player <- function(player, prev) {
  player$health <- prev$health
  player$mo$health <- prev$health
  player$armorpoints <- prev$armorpoints
  player$armortype <- prev$armortype
  player$ammo <- prev$ammo
  player$maxammo <- prev$maxammo
  player$weaponowned <- prev$weaponowned
  player$readyweapon <- prev$readyweapon
  player$pendingweapon <- WP_NOCHANGE
  player$backpack <- isTRUE(prev$backpack)
  player$didsecret <- isTRUE(prev$didsecret)
  player$powers <- prev$powers
  player$cheats <- if (is.null(prev$cheats)) 0L else prev$cheats
  if (bitwAnd(as.integer(player$cheats), CF_NOCLIP) != 0L && !is.null(player$mo)) {
    player$mo$flags <- bitwOr(as.integer(player$mo$flags), MF_NOCLIP)
  }
  player$psprite_state <- "up"
  player$psprite_sy <- WEAPONBOTTOM
  player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
  invisible(player)
}

player_think <- function(world, player, game, leveltime) {
  init_tables()
  update_player_powers(player)
  mo <- player$mo
  cmd <- player$cmd
  if (player$playerstate == PST_DEAD) {
    if (player$viewheight > 6 * FRACUNIT) player$viewheight <- player$viewheight - FRACUNIT
    if (player$viewheight < 6 * FRACUNIT) player$viewheight <- 6 * FRACUNIT
    xy_movement(world, mo, game)
    z_movement(mo)
    calc_height(player, leveltime)
    weapon_think(player, game)
    if (bitwAnd(cmd$buttons, BT_USE) != 0L) player$playerstate <- PST_REBORN
    return(invisible())
  }
  mo$angle <- as_u32(mo$angle + cmd$angleturn * 65536)
  onground <- mo$z <= mo$floorz
  if (cmd$forwardmove && onground) thrust(mo, mo$angle, cmd$forwardmove * 2048)
  if (cmd$sidemove && onground) thrust(mo, as_u32(mo$angle - ANG90), cmd$sidemove * 2048)
  xy_movement(world, mo, game)
  z_movement(mo)
  calc_height(player, leveltime)
  note_secret(world, player)
  if (bitwAnd(cmd$buttons, BT_USE) != 0L) {
    if (!player$usedown) {
      use_lines(world, player, game)
      player$usedown <- TRUE
    }
  } else {
    player$usedown <- FALSE
  }
  if (bitwAnd(cmd$buttons, BT_CHANGE) != 0L) {
    neww <- bitwShiftR(bitwAnd(cmd$buttons, BT_WEAPONMASK), BT_WEAPONSHIFT)
    if (neww >= 0L && neww <= WP_SUPERSHOTGUN && player$weaponowned[neww + 1L] && neww != player$readyweapon) {
      player$pendingweapon <- neww
    }
  }
  weapon_think(player, game)
  invisible()
}

ammo_of <- function(weapon) {
  if (weapon < 0 || weapon >= length(WEAPON_AMMO)) return(NA_integer_)
  WEAPON_AMMO[weapon + 1L]
}

ammo_needed <- function(weapon) if (weapon == WP_BFG) 40 else 1

can_fire_weapon <- function(player) {
  at <- ammo_of(player$readyweapon)
  if (is.na(at)) return(TRUE)
  player$ammo[at + 1L] >= ammo_needed(player$readyweapon)
}

weapon_think <- function(player, game) {
  if (player$playerstate == PST_DEAD || player$health <= 0) {
    lower_weapon(player, game)
    return(invisible())
  }
  firing <- bitwAnd(player$cmd$buttons, BT_ATTACK) != 0L
  can_fire <- can_fire_weapon(player)
  if (!can_fire && player$readyweapon != WP_FIST && player$readyweapon != WP_CHAINSAW) {
    for (w in c(WP_PISTOL, WP_SHOTGUN, WP_CHAINGUN, WP_FIST)) {
      at <- ammo_of(w)
      if (player$weaponowned[w + 1L] && (is.na(at) || player$ammo[at + 1L] >= ammo_needed(w))) {
        player$pendingweapon <- w
        break
      }
    }
    can_fire <- can_fire_weapon(player)
  }
  if (player$flash_tics > 0L) {
    player$flash_tics <- player$flash_tics - 1L
    if (player$flash_tics <= 0L) {
      player$psprite_flash <- ""
      player$extralight <- 0L
    }
  }
  if (player$psprite_state == "atk") {
    if (firing) player$attackdown <- TRUE
    if (player$psprite_tics > 0L) player$psprite_tics <- player$psprite_tics - 1L
    if (player$psprite_tics > 0L) return(invisible())
    player$psprite_step <- player$psprite_step + 1L
    enter_atk_step(player, game, firing, can_fire)
    return(invisible())
  }
  if (player$pendingweapon != WP_NOCHANGE || player$psprite_state == "down") {
    lower_weapon(player, game)
    return(invisible())
  }
  if (player$psprite_state == "up") {
    raise_weapon(player, game)
    return(invisible())
  }
  if (firing && can_fire) {
    hold_ok <- !isTRUE(player$attackdown) || !(player$readyweapon %in% c(WP_MISSILE, WP_BFG))
    if (!hold_ok) {
      if (!nzchar(player$psprite_body)) player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
      return(invisible())
    }
    player$psprite_state <- "atk"
    player$psprite_step <- 0L
    player$psprite_sy <- WEAPONTOP
    player$attackdown <- TRUE
    enter_atk_step(player, game, firing, can_fire)
    return(invisible())
  }
  if (player$psprite_state != "ready") {
    start_ready(player)
    return(invisible())
  }
  if (!nzchar(player$psprite_body)) player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
  if (!firing) {
    player$attackdown <- FALSE
    player$refire <- 0L
  }
  invisible()
}

lower_weapon <- function(player, game) {
  player$psprite_state <- "down"
  if (!nzchar(player$psprite_body)) player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
  player$psprite_sy <- player$psprite_sy + LOWERSPEED
  if (player$psprite_sy < WEAPONBOTTOM) return(invisible())
  player$psprite_sy <- WEAPONBOTTOM
  if (player$playerstate == PST_DEAD || player$health <= 0) return(invisible())
  if (player$pendingweapon != WP_NOCHANGE) {
    player$readyweapon <- player$pendingweapon
    player$pendingweapon <- WP_NOCHANGE
  }
  player$psprite_state <- "up"
  player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
  if (player$readyweapon == WP_CHAINSAW) game$start_sound("sawup")
  invisible()
}

raise_weapon <- function(player, game) {
  player$psprite_sy <- player$psprite_sy - RAISESPEED
  if (!nzchar(player$psprite_body)) player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
  if (player$psprite_sy > WEAPONTOP) return(invisible())
  player$psprite_sy <- WEAPONTOP
  start_ready(player)
}

start_ready <- function(player) {
  player$psprite_state <- "ready"
  player$psprite_step <- 0L
  player$psprite_body <- WEAPON_PATCH[player$readyweapon + 1L]
  player$psprite_tics <- 0L
  invisible()
}

enter_atk_step <- function(player, game, firing, can_fire) {
  seq <- weapon_atk(player$readyweapon)
  repeat {
    if (player$psprite_step >= length(seq)) {
      if (firing && can_fire && player$pendingweapon == WP_NOCHANGE) {
        player$psprite_step <- 0L
        next
      }
      start_ready(player)
      if (!firing) {
        player$attackdown <- FALSE
        player$refire <- 0L
      }
      return(invisible())
    }
    step <- seq[[player$psprite_step + 1L]]
    player$psprite_body <- step[[1]]
    player$psprite_tics <- as.integer(step[[2]])
    if (length(step) >= 6L && as.integer(step[[5]]) > 0L) {
      player$psprite_flash <- step[[4]]
      player$flash_tics <- as.integer(step[[5]])
      player$extralight <- as.integer(step[[6]])
    }
    if (as.integer(step[[3]]) != 0L) do_shot(player, game)
    if (player$psprite_tics > 0L) return(invisible())
    player$psprite_step <- player$psprite_step + 1L
  }
}

gun_shot <- function(player, game, accurate) {
  mo <- player$mo
  slope <- bullet_slope(game$world, mo)
  damage <- 5 * ((p_random() %% 3) + 1)
  angle <- mo$angle
  if (!accurate) angle <- as_u32(angle + (p_random() - p_random()) * 262144)
  line_attack(game$world, mo, damage, game, MISSILERANGE, angle, slope)
}

do_shot <- function(player, game) {
  noise_alert(game$world, player$mo, game)
  weapon <- player$readyweapon
  at <- ammo_of(weapon)
  need <- ammo_needed(weapon)
  if (!is.na(at)) {
    if (player$ammo[at + 1L] < need) return(invisible())
    player$ammo[at + 1L] <- player$ammo[at + 1L] - need
  }
  mo <- player$mo
  if (weapon == WP_MISSILE || weapon == WP_PLASMA || weapon == WP_BFG) {
    if (weapon == WP_PLASMA) bitwAnd(p_random(), 1L)
    kind <- if (weapon == WP_MISSILE) MT_ROCKET else if (weapon == WP_PLASMA) MT_PLASMA else MT_BFG
    spawn_player_missile(game$world, mo, kind, game)
    game$start_sound(if (weapon == WP_MISSILE) "rlaunc" else if (weapon == WP_PLASMA) "plasma" else "bfg")
  } else if (weapon == WP_FIST) {
    damage <- ((p_random() %% 10) + 1) * 2
    if (player$powers[PW_STRENGTH + 1L]) damage <- damage * 10
    angle <- as_u32(mo$angle + (p_random() - p_random()) * 262144)
    if (line_attack(game$world, mo, damage, game, MELEERANGE, angle, NULL)) game$start_sound("punch")
  } else if (weapon == WP_CHAINSAW) {
    damage <- 2 * ((p_random() %% 10) + 1)
    angle <- as_u32(mo$angle + (p_random() - p_random()) * 262144)
    hit <- line_attack(game$world, mo, damage, game, MELEERANGE + 1, angle, NULL)
    game$start_sound(if (hit) "sawhit" else "sawful")
  } else if (weapon == WP_SHOTGUN || weapon == WP_SUPERSHOTGUN) {
    game$start_sound(if (weapon == WP_SHOTGUN) "shotgn" else "dshtgn")
    n <- if (weapon == WP_SHOTGUN) 7L else 20L
    slope <- bullet_slope(game$world, mo)
    for (i in seq_len(n)) {
      damage <- 5 * ((p_random() %% 3) + 1)
      spread <- if (weapon == WP_SHOTGUN) 262144 else 524288
      angle <- as_u32(mo$angle + (p_random() - p_random()) * spread)
      pellet <- if (weapon == WP_SUPERSHOTGUN) slope + (p_random() - p_random()) * 32 else slope
      line_attack(game$world, mo, damage, game, MISSILERANGE, angle, pellet)
    }
  } else {
    game$start_sound("pistol")
    gun_shot(player, game, player$refire == 0L)
  }
  player$refire <- player$refire + 1L
  player$attackdown <- TRUE
  invisible()
}

remove_mobj <- function(world, mo) {
  mo$alive <- FALSE
  mo$istate <- 0L
  mo$flags <- 0L
  unset_thing_position(world, mo)
  world$mobjs <- Filter(function(x) !identical(x, mo), world$mobjs)
  invisible()
}

give_ammo <- function(player, ammo, count) {
  if (player$ammo[ammo + 1L] >= player$maxammo[ammo + 1L]) return(FALSE)
  player$ammo[ammo + 1L] <- min(player$maxammo[ammo + 1L], player$ammo[ammo + 1L] + count)
  TRUE
}

give_weapon <- function(player, weapon, ammo, count) {
  had <- player$weaponowned[weapon + 1L]
  player$weaponowned[weapon + 1L] <- TRUE
  if (!is.na(ammo)) give_ammo(player, ammo, count)
  if (!had) player$pendingweapon <- weapon
  TRUE
}

touch_special <- function(game, special, toucher) {
  player <- toucher$player
  if (is.null(player) || isFALSE(special$alive)) return(invisible())
  n <- special$doomed
  took <- TRUE
  sfx <- "itemup"
  text <- "pegou um item"
  if (n == 2011) {
    took <- give_body(player, 10)
    text <- "pegou um estimulante"
  } else if (n == 2012) {
    took <- give_body(player, 25)
    text <- "pegou um kit medico"
  } else if (n == 2014) {
    player$health <- min(200, player$health + 1)
    player$mo$health <- player$health
    text <- "pegou um bonus de vida"
  } else if (n == 2015) {
    took <- give_armor_bonus(player)
    text <- "pegou um bonus de armadura"
  } else if (n == 2018) {
    took <- give_armor(player, 100, 1)
    text <- "pegou a armadura"
  } else if (n == 2019) {
    took <- give_armor(player, 200, 2)
    text <- "pegou a mega-armadura"
  } else if (n == 2013) {
    player$health <- min(200, player$health + 100)
    player$mo$health <- player$health
    text <- "super carga"
  } else if (n == 83) {
    player$health <- 200
    player$mo$health <- 200
    player$armorpoints <- 200
    player$armortype <- 2
    text <- "megaesfera"
  } else if (n == 2023) {
    took <- give_power(player, PW_STRENGTH)
    if (took && player$readyweapon != WP_FIST) player$pendingweapon <- WP_FIST
    text <- "berserk"
    sfx <- "getpow"
  } else if (n == 2022) {
    took <- give_power(player, PW_INVULNERABILITY)
    text <- "invulnerabilidade"
    sfx <- "getpow"
  } else if (n == 2024) {
    took <- give_power(player, PW_INVISIBILITY)
    text <- "invisibilidade parcial"
    sfx <- "getpow"
  } else if (n == 2025) {
    took <- give_power(player, PW_IRONFEET)
    text <- "traje anti-radiacao"
    sfx <- "getpow"
  } else if (n == 2026) {
    took <- give_power(player, PW_ALLMAP)
    text <- "mapa do computador"
    sfx <- "getpow"
  } else if (n == 2045) {
    took <- give_power(player, PW_INFRARED)
    text <- "visor de luz"
    sfx <- "getpow"
  } else if (n == 5) {
    player$cards[IT_BLUECARD + 1L] <- TRUE
    text <- "pegou a chave azul"
  } else if (n == 6) {
    player$cards[IT_YELLOWCARD + 1L] <- TRUE
    text <- "pegou a chave amarela"
  } else if (n == 13) {
    player$cards[IT_REDCARD + 1L] <- TRUE
    text <- "pegou a chave vermelha"
  } else if (n == 40) {
    player$cards[IT_BLUESKULL + 1L] <- TRUE
    text <- "pegou a caveira azul"
  } else if (n == 39) {
    player$cards[IT_YELLOWSKULL + 1L] <- TRUE
    text <- "pegou a caveira amarela"
  } else if (n == 38) {
    player$cards[IT_REDSKULL + 1L] <- TRUE
    text <- "pegou a caveira vermelha"
  } else if (n == 2001) {
    took <- give_weapon(player, WP_SHOTGUN, AM_SHELL, clip_rounds(AM_SHELL, 1))
    text <- "pegou a escopeta"
    sfx <- "wpnup"
  } else if (n == 82) {
    took <- give_weapon(player, WP_SUPERSHOTGUN, AM_SHELL, clip_rounds(AM_SHELL, 1))
    text <- "pegou a super espingarda"
    sfx <- "wpnup"
  } else if (n == 2002) {
    took <- give_weapon(player, WP_CHAINGUN, AM_CLIP, clip_rounds(AM_CLIP, 1))
    text <- "pegou a metralhadora"
    sfx <- "wpnup"
  } else if (n == 2003) {
    took <- give_weapon(player, WP_MISSILE, AM_MISL, clip_rounds(AM_MISL, 1))
    text <- "pegou o lancador"
    sfx <- "wpnup"
  } else if (n == 2004) {
    took <- give_weapon(player, WP_PLASMA, AM_CELL, clip_rounds(AM_CELL, 1))
    text <- "pegou o rifle de plasma"
    sfx <- "wpnup"
  } else if (n == 2005) {
    took <- give_weapon(player, WP_CHAINSAW, NA, 0)
    text <- "pegou a motosserra"
    sfx <- "wpnup"
  } else if (n == 2006) {
    took <- give_weapon(player, WP_BFG, AM_CELL, clip_rounds(AM_CELL, 1))
    text <- "pegou a bfg"
    sfx <- "wpnup"
  } else if (n == 2007) {
    took <- give_ammo(player, AM_CLIP, clip_rounds(AM_CLIP, 1))
    text <- "pegou municao"
  } else if (n == 2048) {
    took <- give_ammo(player, AM_CLIP, clip_rounds(AM_CLIP, 5))
    text <- "pegou municao"
  } else if (n == 2008) {
    took <- give_ammo(player, AM_SHELL, clip_rounds(AM_SHELL, 1))
    text <- "pegou municao"
  } else if (n == 2049) {
    took <- give_ammo(player, AM_SHELL, clip_rounds(AM_SHELL, 5))
    text <- "pegou municao"
  } else if (n == 2047) {
    took <- give_ammo(player, AM_CELL, clip_rounds(AM_CELL, 1))
    text <- "pegou municao"
  } else if (n == 17) {
    took <- give_ammo(player, AM_CELL, clip_rounds(AM_CELL, 5))
    text <- "pegou municao"
  } else if (n == 2010) {
    took <- give_ammo(player, AM_MISL, clip_rounds(AM_MISL, 1))
    text <- "pegou municao"
  } else if (n == 2046) {
    took <- give_ammo(player, AM_MISL, clip_rounds(AM_MISL, 5))
    text <- "pegou municao"
  } else if (n == 8) {
    if (!isTRUE(player$backpack)) {
      player$maxammo <- player$maxammo * 2
      player$backpack <- TRUE
    }
    for (ammo in c(AM_CLIP, AM_SHELL, AM_CELL, AM_MISL)) give_ammo(player, ammo, clip_rounds(ammo, 1))
    text <- "pegou uma mochila de municao"
  } else {
    took <- FALSE
  }
  if (!took) return(invisible())
  game$start_sound(sfx)
  set_player_message(player, text)
  player$bonuscount <- player$bonuscount + 6
  if (has_flag(special$flags, MF_COUNTITEM)) player$itemcount <- player$itemcount + 1L
  game$world$dirty <- TRUE
  remove_mobj(game$world, special)
  invisible()
}

give_body <- function(player, amount) {
  if (player$health >= 100 && amount < 100) return(FALSE)
  player$health <- min(if (amount >= 100) 200 else 100, player$health + amount)
  player$mo$health <- player$health
  TRUE
}

give_armor <- function(player, points, type) {
  if (player$armorpoints >= points) return(FALSE)
  player$armortype <- type
  player$armorpoints <- points
  TRUE
}

give_armor_bonus <- function(player) {
  player$armorpoints <- min(200, player$armorpoints + 1)
  if (!player$armortype) player$armortype <- 1
  TRUE
}

give_card <- function(player, card) {
  if (player$cards[card + 1L]) return(FALSE)
  player$cards[card + 1L] <- TRUE
  TRUE
}

has_card <- function(player, a, b) player$cards[a + 1L] || player$cards[b + 1L]

damage_mobj <- function(game, target, source, damage, inflictor = NULL) {
  if (is.null(target) || damage <= 0) return(invisible())
  if (isFALSE(target$alive)) return(invisible())
  if (!has_flag(target$flags, MF_SHOOTABLE)) return(invisible())
  if (!is.null(target$player) && game$skill == 0L) damage <- shar(damage, 1L)
  src <- if (!is.null(inflictor)) inflictor else source
  skip_saw <- !is.null(source) && !is.null(source$player) && source$player$readyweapon == WP_CHAINSAW
  if (!is.null(src) && !has_flag(target$flags, MF_NOCLIP) && !skip_saw && !is.null(src$x)) {
    ang <- angle_to(src$x, src$y, target$x, target$y)
    mass <- 100
    if (!is.null(target$type) && target$type + 1L <= length(MOBJINFO)) mass <- MOBJINFO[[target$type + 1L]]$mass
    if (!mass) mass <- 100
    thrust <- damage * (FRACUNIT %/% 8) * 100 / mass
    if (damage < 40 && damage > target$health && target$z - src$z > 64 * FRACUNIT && bitwAnd(p_random(), 1L) != 0L) {
      ang <- as_u32(ang + ANG180)
      thrust <- thrust * 4
    }
    target$momx <- target$momx + fixed_mul(thrust, fine_cos(ang))
    target$momy <- target$momy + fixed_mul(thrust, fine_sin(ang))
  }
  if (!is.null(target$player)) {
    cheats <- if (is.null(target$player$cheats)) 0L else as.integer(target$player$cheats)
    if (bitwAnd(cheats, CF_GODMODE) != 0L && damage < 1000) return(invisible())
    if (target$player$powers[PW_INVULNERABILITY + 1L] > 0) return(invisible())
  }
  if (!is.null(target$player)) {
    player <- target$player
    player$attacker <- source
    saved <- 0
    if (player$armortype) {
      saved <- damage %/% (if (player$armortype == 1) 3 else 2)
      if (player$armorpoints <= saved) {
        saved <- player$armorpoints
        player$armortype <- 0
      }
      player$armorpoints <- player$armorpoints - saved
    }
    damage <- damage - saved
    player$health <- player$health - damage
    target$health <- player$health
    player$damagecount <- min(100L, player$damagecount + damage)
    if (player$health <= 0) {
      player$health <- 0
      target$health <- 0
      player$playerstate <- PST_DEAD
      target$alive <- FALSE
      target$flags <- bitwAnd(as.integer(target$flags), bitwNot(MF_SOLID + MF_SHOOTABLE))
      game$start_sound("pldeth")
    } else {
      game$start_sound("plpain")
    }
  } else {
    target$health <- target$health - damage
    if (target$health <= 0) kill_monster(target, game, source)
    else pain_or_wake(target, source, game)
  }
  game$world$dirty <- TRUE
  invisible()
}

tick_fx <- function(world) {
  keep <- list()
  for (mo in world$mobjs) {
    if (!isTRUE(mo$fx)) {
      keep[[length(keep) + 1L]] <- mo
      next
    }
    mo$tics <- mo$tics - 1L
    mo$z <- mo$z + mo$momz
    if (mo$tics > 0L) keep[[length(keep) + 1L]] <- mo
    else world$dirty <- TRUE
  }
  world$mobjs <- keep
  invisible()
}

weapon_psprite_xy <- function(player, leveltime) {
  state <- player$psprite_state
  if (state %in% c("up", "down", "atk")) return(c(FRACUNIT, player$psprite_sy))
  bob <- player$bob
  angle <- bitwAnd(as.integer(128 * leveltime), FINEMASK)
  sx <- FRACUNIT + fixed_mul(bob, finesine[bitwAnd(angle + FINEANGLES %/% 4L, FINEMASK) + 1L])
  angle <- bitwAnd(angle, FINEANGLES %/% 2L - 1L)
  sy <- WEAPONTOP + fixed_mul(bob, finesine[angle + 1L])
  player$psprite_sy <- sy
  c(sx, sy)
}

view_signature <- function(game) {
  p <- game$player
  if (is.null(p) || is.null(p$mo)) return("")
  mo <- p$mo
  paste(
    mo$x, mo$y, mo$z, mo$angle, p$viewz, p$readyweapon, p$psprite_state,
    p$psprite_body, p$psprite_flash, p$psprite_sy, p$extralight, p$health,
    length(game$world$mobjs)
  )
}
