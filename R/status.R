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
# Barra de status (st_stuff): vida, municao, armas, chaves e rosto.

ST_FACESTRIDE <- 8L
ST_NUMPAINFACES <- 5L
ST_TURNOFFSET <- 3L
ST_OUCHOFFSET <- 5L
ST_EVILGRINOFFSET <- 6L
ST_RAMPAGEOFFSET <- 7L
ST_GODFACE <- ST_NUMPAINFACES * ST_FACESTRIDE
ST_DEADFACE <- ST_GODFACE + 1L

status_optional <- function(wad, name) {
  n <- wad_check_num(wad, name)
  if (n < 0L) NULL else wad_cache_num(wad, n)
}

status_new <- function(wad) {
  st <- new.env(parent = emptyenv())
  st$sbar <- wad_cache_name(wad, "STBAR")
  st$tallnum <- lapply(0:9, function(i) wad_cache_name(wad, sprintf("STTNUM%d", i)))
  st$shortnum <- lapply(0:9, function(i) wad_cache_name(wad, sprintf("STYSNUM%d", i)))
  st$tallpercent <- wad_cache_name(wad, "STTPRCNT")
  st$keys <- lapply(0:5, function(i) status_optional(wad, sprintf("STKEYS%d", i)))
  st$armsbg <- status_optional(wad, "STARMS")
  st$arms_off <- lapply(2:7, function(i) status_optional(wad, sprintf("STGNUM%d", i)))
  st$fallback <- wad_cache_name(wad, "STFST00")
  faces <- list()
  for (pain in 0:4) {
    for (look in 0:2) faces[[length(faces) + 1L]] <- status_optional(wad, sprintf("STFST%d%d", pain, look))
    faces[[length(faces) + 1L]] <- status_optional(wad, sprintf("STFTR%d0", pain))
    faces[[length(faces) + 1L]] <- status_optional(wad, sprintf("STFTL%d0", pain))
    faces[[length(faces) + 1L]] <- status_optional(wad, sprintf("STFOUCH%d", pain))
    faces[[length(faces) + 1L]] <- status_optional(wad, sprintf("STFEVL%d", pain))
    faces[[length(faces) + 1L]] <- status_optional(wad, sprintf("STFKILL%d", pain))
  }
  faces[[length(faces) + 1L]] <- status_optional(wad, "STFGOD0")
  faces[[length(faces) + 1L]] <- status_optional(wad, "STFDEAD0")
  st$faces <- faces
  st$face_index <- 0L
  st$face_count <- 0L
  st$face_priority <- 0L
  st$old_health <- -1
  st$pain_old <- -1
  st$last_calc <- 0L
  st$last_attack <- -1L
  st$old_weapons <- rep(FALSE, 9L)
  st$rnd <- 1
  st
}

status_pain_offset <- function(st, player) {
  health <- max(0, min(100, as.integer(player$health)))
  if (health != st$pain_old) {
    st$last_calc <- ST_FACESTRIDE * ((100 - health) * ST_NUMPAINFACES) %/% 101
    st$pain_old <- health
  }
  st$last_calc
}

status_ticker <- function(st, player) {
  if (is.null(st) || is.null(player)) return(invisible())
  st$rnd <- (as.numeric(st$rnd) * 1103515245 + 12345) %% 4294967296
  roll <- bitwAnd(as.integer(st$rnd %/% 65536), 255L)
  if (is.na(roll)) roll <- 0L
  if (st$face_priority < 10L && player$health <= 0) {
    st$face_priority <- 9L
    st$face_index <- ST_DEADFACE
    st$face_count <- 1L
  }
  if (st$face_priority < 9L && isTRUE(player$bonuscount > 0)) {
    owned <- player$weaponowned
    n <- min(length(st$old_weapons), length(owned))
    grin <- FALSE
    for (i in seq_len(n)) {
      if (!identical(st$old_weapons[i], owned[i])) {
        grin <- TRUE
        st$old_weapons[i] <- owned[i]
      }
    }
    if (grin) {
      st$face_priority <- 8L
      st$face_count <- 2L * TICRATE
      st$face_index <- status_pain_offset(st, player) + ST_EVILGRINOFFSET
    }
  }
  if (st$face_priority < 7L && isTRUE(player$damagecount > 0)) {
    st$face_priority <- 6L
    st$face_count <- TICRATE
    st$face_index <- status_pain_offset(st, player) + ST_RAMPAGEOFFSET
    bad <- player$attacker
    if (!is.null(bad) && !is.null(player$mo) && !identical(bad, player$mo) && !is.null(bad$x)) {
      badang <- angle_to(player$mo$x, player$mo$y, bad$x, bad$y)
      diffang <- as_u32(badang - player$mo$angle)
      turn_right <- diffang > ANG180
      if (turn_right) diffang <- as_u32(ANG180 * 2 - diffang)
      st$face_index <- status_pain_offset(st, player)
      if (diffang < ANG45) st$face_index <- st$face_index + ST_RAMPAGEOFFSET
      else if (turn_right) st$face_index <- st$face_index + ST_TURNOFFSET
      else st$face_index <- st$face_index + ST_TURNOFFSET + 1L
      if (player$health - st$old_health > 20) st$face_index <- status_pain_offset(st, player) + ST_OUCHOFFSET
    }
  }
  if (st$face_priority < 6L && isTRUE(player$attackdown)) {
    if (st$last_attack < 0L) st$last_attack <- 2L * TICRATE
    else {
      st$last_attack <- st$last_attack - 1L
      if (st$last_attack == 0L) {
        st$face_priority <- 5L
        st$face_index <- status_pain_offset(st, player) + ST_RAMPAGEOFFSET
        st$face_count <- 1L
        st$last_attack <- 1L
      }
    }
  } else if (st$face_priority < 6L) {
    st$last_attack <- -1L
  }
  cheats <- if (is.null(player$cheats)) 0L else as.integer(player$cheats)
  invul <- player$powers[PW_INVULNERABILITY + 1L] > 0
  if (st$face_priority < 5L && (bitwAnd(cheats, CF_GODMODE) != 0L || invul)) {
    st$face_priority <- 4L
    st$face_index <- ST_GODFACE
    st$face_count <- 1L
  }
  if (st$face_count <= 0L) {
    st$face_index <- status_pain_offset(st, player) + (roll %% 3L)
    st$face_count <- TICRATE %/% 2L
    st$face_priority <- 0L
  }
  st$face_count <- st$face_count - 1L
  st$old_health <- player$health
  invisible()
}

status_digit <- function(fb, x, y, n, font) {
  n <- max(0L, min(9L, as.integer(n)))
  glyph <- font[[n + 1L]]
  if (!is.null(glyph)) draw_patch(fb, x, y, glyph) else fb
}

status_num <- function(fb, x, y, value, digits, font) {
  w <- i16_at(font[[1]], 0L)
  if (is.na(w) || w < 1L) w <- 8L
  x <- x - w
  value <- abs(as.integer(value))
  if (is.na(value)) value <- 0L
  for (i in seq_len(digits)) {
    fb <- status_digit(fb, x, y, value %% 10L, font)
    x <- x - w
    value <- value %/% 10L
    if (value == 0L) break
  }
  fb
}

status_face <- function(st) {
  i <- st$face_index + 1L
  if (i >= 1L && i <= length(st$faces) && !is.null(st$faces[[i]])) st$faces[[i]] else st$fallback
}

status_draw <- function(st, fb, player) {
  if (is.null(st) || is.null(player) || is.null(st$sbar)) return(fb)
  fb <- draw_patch(fb, 0L, 168L, st$sbar)
  if (!is.null(st$armsbg)) fb <- draw_patch(fb, 104L, 168L, st$armsbg)
  at <- ammo_of(player$readyweapon)
  ammo <- if (is.na(at)) 0 else player$ammo[at + 1L]
  fb <- status_num(fb, 44L, 171L, ammo, 3L, st$tallnum)
  fb <- status_num(fb, 90L, 171L, player$health, 3L, st$tallnum)
  fb <- draw_patch(fb, 90L, 171L, st$tallpercent)
  fb <- status_num(fb, 221L, 171L, player$armorpoints, 3L, st$tallnum)
  fb <- draw_patch(fb, 221L, 171L, st$tallpercent)
  owned <- c(
    isTRUE(player$weaponowned[WP_SHOTGUN + 1L]) || isTRUE(player$weaponowned[WP_SUPERSHOTGUN + 1L]),
    isTRUE(player$weaponowned[WP_CHAINGUN + 1L]),
    isTRUE(player$weaponowned[WP_MISSILE + 1L]),
    isTRUE(player$weaponowned[WP_PLASMA + 1L]),
    isTRUE(player$weaponowned[WP_BFG + 1L]),
    FALSE
  )
  for (i in 0:5) {
    x <- 111L + (i %% 3L) * 12L
    y <- 172L + (i %/% 3L) * 10L
    if (owned[i + 1L]) fb <- status_digit(fb, x, y, i + 2L, st$shortnum)
    else if (i < length(st$arms_off) && !is.null(st$arms_off[[i + 1L]])) {
      fb <- draw_patch(fb, x, y, st$arms_off[[i + 1L]])
    }
  }
  face <- status_face(st)
  if (!is.null(face)) fb <- draw_patch(fb, 143L, 168L, face)
  slots <- list(c(IT_BLUECARD, IT_BLUESKULL), c(IT_YELLOWCARD, IT_YELLOWSKULL), c(IT_REDCARD, IT_REDSKULL))
  for (slot in seq_along(slots)) {
    card <- slots[[slot]][1]
    skull <- slots[[slot]][2]
    if (isTRUE(player$cards[card + 1L]) || isTRUE(player$cards[skull + 1L])) {
      idx <- if (isTRUE(player$cards[skull + 1L])) skull else card
      patch <- if (idx + 1L <= length(st$keys)) st$keys[[idx + 1L]] else NULL
      if (!is.null(patch)) fb <- draw_patch(fb, 239L, 171L + (slot - 1L) * 10L, patch)
    }
  }
  order <- c(AM_CLIP, AM_SHELL, AM_CELL, AM_MISL)
  pos <- list(c(288L, 173L), c(288L, 179L), c(288L, 191L), c(288L, 185L))
  mx <- list(c(314L, 173L), c(314L, 179L), c(314L, 191L), c(314L, 185L))
  for (i in seq_along(order)) {
    am <- order[i]
    fb <- status_num(fb, pos[[i]][1], pos[[i]][2], player$ammo[am + 1L], 3L, st$shortnum)
    fb <- status_num(fb, mx[[i]][1], mx[[i]][2], player$maxammo[am + 1L], 3L, st$shortnum)
  }
  fb
}
