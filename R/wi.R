# Intermissao de um jogador. Contagem, mapa e entrada da fase seguinte.

WI_TITLEY <- 2L
SP_STATSX <- 50L
SP_STATSY <- 50L
SP_TIMEX <- 16L
SP_TIMEY <- SCREENHEIGHT - 32L
SHOWNEXTLOCDELAY <- 4L
WI_NO_STATE <- -1L
WI_STAT_COUNT <- 0L
WI_SHOW_NEXT <- 1L
WI_ANIM_ALWAYS <- 0L
WI_ANIM_LEVEL <- 2L

WI_PARS <- list(
  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  c(0, 30, 75, 120, 90, 165, 180, 180, 30, 165),
  c(0, 90, 90, 90, 120, 90, 360, 240, 30, 170),
  c(0, 90, 45, 90, 150, 90, 90, 165, 30, 135)
)
WI_CPARS <- c(
  30, 90, 120, 120, 90, 150, 120, 120, 270, 90,
  210, 150, 150, 150, 210, 150, 420, 150, 210, 150,
  240, 150, 180, 150, 150, 300, 330, 420, 300, 180,
  120, 30
)
WI_LNODES <- list(
  list(c(185, 164), c(148, 143), c(69, 122), c(209, 102), c(116, 89), c(166, 55), c(71, 56), c(135, 29), c(71, 24)),
  list(c(254, 25), c(97, 50), c(188, 64), c(128, 78), c(214, 92), c(133, 130), c(208, 136), c(148, 140), c(235, 158)),
  list(c(156, 168), c(48, 154), c(174, 95), c(265, 75), c(130, 48), c(279, 23), c(198, 48), c(140, 25), c(281, 136))
)
WI_ANIMS <- list(
  list(
    c(0, TICRATE %/% 3, 3, 224, 104, 0), c(0, TICRATE %/% 3, 3, 184, 160, 0),
    c(0, TICRATE %/% 3, 3, 112, 136, 0), c(0, TICRATE %/% 3, 3, 72, 112, 0),
    c(0, TICRATE %/% 3, 3, 88, 96, 0), c(0, TICRATE %/% 3, 3, 64, 48, 0),
    c(0, TICRATE %/% 3, 3, 192, 40, 0), c(0, TICRATE %/% 3, 3, 136, 16, 0),
    c(0, TICRATE %/% 3, 3, 80, 16, 0), c(0, TICRATE %/% 3, 3, 64, 24, 0)
  ),
  list(
    c(2, TICRATE %/% 3, 1, 128, 136, 1), c(2, TICRATE %/% 3, 1, 128, 136, 2),
    c(2, TICRATE %/% 3, 1, 128, 136, 3), c(2, TICRATE %/% 3, 1, 128, 136, 4),
    c(2, TICRATE %/% 3, 1, 128, 136, 5), c(2, TICRATE %/% 3, 1, 128, 136, 6),
    c(2, TICRATE %/% 3, 1, 128, 136, 7), c(0, TICRATE %/% 3, 3, 192, 144, 8),
    c(2, TICRATE %/% 3, 1, 128, 136, 8)
  ),
  list(
    c(0, TICRATE %/% 3, 3, 104, 168, 0), c(0, TICRATE %/% 3, 3, 40, 136, 0),
    c(0, TICRATE %/% 3, 3, 160, 96, 0), c(0, TICRATE %/% 3, 3, 104, 80, 0),
    c(0, TICRATE %/% 3, 3, 120, 32, 0), c(0, TICRATE %/% 4, 3, 40, 0, 0)
  )
)

wi_partime <- function(episode, mapn, commercial) {
  if (commercial) {
    i <- max(1L, min(length(WI_CPARS), as.integer(mapn)))
    return(TICRATE * WI_CPARS[i])
  }
  if (episode >= 1L && episode <= 3L && mapn >= 1L && mapn <= 9L) {
    return(TICRATE * WI_PARS[[episode + 1L]][mapn + 1L])
  }
  TICRATE * 30L
}

wi_pct <- function(value, maximum) (as.integer(value) * 100L) %/% max(1L, as.integer(maximum))

wi_lump <- function(wad, name) {
  n <- wad_check_num(wad, name)
  if (n < 0L) NULL else wad_cache_num(wad, n)
}

wi_new <- function(game, wbs) {
  wi <- new.env(parent = emptyenv())
  wi$game <- game
  wi$wbs <- wbs
  wi$state <- WI_STAT_COUNT
  wi$accelerate <- 0L
  wi$sp_state <- 1L
  wi$cnt_kills <- -1L
  wi$cnt_items <- -1L
  wi$cnt_secret <- -1L
  wi$cnt_time <- -1L
  wi$cnt_par <- -1L
  wi$cnt_pause <- TICRATE
  wi$cnt <- 0L
  wi$bcnt <- 0L
  wi$snl_pointeron <- FALSE
  wi$done <- FALSE
  wad <- game$wad
  names <- c(
    finished = "WIF", entering = "WIENTER", kills = "WIOSTK", items = "WIOSTI",
    sp_secret = "WISCRT2", percent = "WIPCNT", colon = "WICOLON", time = "WITIME",
    par = "WIPAR", sucks = "WISUCKS", minus = "WIMINUS", splat = "WISPLAT",
    yah0 = "WIURH0", yah1 = "WIURH1"
  )
  wi$p <- lapply(names, function(lump) wi_lump(wad, lump))
  wi$num <- lapply(0:9, function(i) wi_lump(wad, sprintf("WINUM%d", i)))
  bg <- if (wbs$commercial || wbs$epsd == 3L) "INTERPIC" else sprintf("WIMAP%d", wbs$epsd)
  wi$background <- wi_lump(wad, bg)
  if (is.null(wi$background)) wi$background <- wi_lump(wad, "INTERPIC")
  nmaps <- if (wbs$commercial) 32L else 9L
  wi$lnames <- lapply(seq_len(nmaps) - 1L, function(i) {
    name <- if (wbs$commercial) sprintf("CWILV%02d", i) else sprintf("WILV%d%d", wbs$epsd, i)
    wi_lump(wad, name)
  })
  wi$anims <- list()
  if (!wbs$commercial && wbs$epsd < 3L) {
    specs <- WI_ANIMS[[wbs$epsd + 1L]]
    for (j in seq_along(specs)) {
      spec <- specs[[j]]
      a <- list(
        type = spec[1], period = spec[2], nanims = spec[3],
        x = spec[4], y = spec[5], data1 = spec[6],
        patches = list(), ctr = -1L, nexttic = 0L
      )
      for (i in seq_len(spec[3]) - 1L) {
        patch <- if (wbs$epsd == 1L && j == 9L && length(wi$anims) >= 5L) {
          wi$anims[[5]]$patches[[i + 1L]]
        } else {
          wi_lump(wad, sprintf("WIA%d%02d%02d", wbs$epsd, j - 1L, i))
        }
        a$patches[[i + 1L]] <- patch
      }
      wi$anims[[j]] <- a
    }
  }
  wi_init_animated(wi)
  wi
}

wi_init_animated <- function(wi) {
  if (wi$wbs$commercial || wi$wbs$epsd > 2L) return(invisible())
  for (i in seq_along(wi$anims)) {
    a <- wi$anims[[i]]
    a$ctr <- -1L
    a$nexttic <- if (a$type == WI_ANIM_ALWAYS) {
      wi$bcnt + 1L + sample.int(max(1L, a$period), 1L) - 1L
    } else {
      wi$bcnt + 1L
    }
    wi$anims[[i]] <- a
  }
  invisible()
}

wi_update_animated <- function(wi) {
  if (wi$wbs$commercial || wi$wbs$epsd > 2L) return(invisible())
  for (i in seq_along(wi$anims)) {
    a <- wi$anims[[i]]
    if (wi$bcnt != a$nexttic) next
    if (a$type == WI_ANIM_ALWAYS) {
      a$ctr <- a$ctr + 1L
      if (a$ctr >= a$nanims) a$ctr <- 0L
      a$nexttic <- wi$bcnt + a$period
    } else if (a$type == WI_ANIM_LEVEL) {
      if (!(wi$state == WI_STAT_COUNT && i == 8L) && wi$wbs$nxt == a$data1) {
        a$ctr <- a$ctr + 1L
        if (a$ctr == a$nanims) a$ctr <- a$ctr - 1L
        a$nexttic <- wi$bcnt + a$period
      }
    }
    wi$anims[[i]] <- a
  }
  invisible()
}

wi_key_down <- function(game, name) isTRUE(!is.null(game$down[[name]]))

wi_check_accelerate <- function(wi) {
  game <- wi$game
  if (isTRUE(game$menu$active)) return(invisible())
  attack <- wi_key_down(game, "ctrl")
  use <- wi_key_down(game, "space") || wi_key_down(game, "e") || wi_key_down(game, "return")
  p <- game$player
  if (is.null(p)) {
    if (attack || use) wi$accelerate <- 1L
    return(invisible())
  }
  if (attack) {
    if (!isTRUE(p$attackdown)) wi$accelerate <- 1L
    p$attackdown <- TRUE
  } else {
    p$attackdown <- FALSE
  }
  if (use) {
    if (!isTRUE(p$usedown)) wi$accelerate <- 1L
    p$usedown <- TRUE
  } else {
    p$usedown <- FALSE
  }
  invisible()
}

wi_ticker <- function(wi) {
  wi$bcnt <- wi$bcnt + 1L
  if (wi$bcnt == 1L) {
    name <- if (wi$wbs$commercial) "dm2int" else "inter"
    sound_change_music(wi$game$sound, name, TRUE)
  }
  wi_check_accelerate(wi)
  if (wi$state == WI_STAT_COUNT) wi_update_stats(wi)
  else if (wi$state == WI_SHOW_NEXT) wi_update_show_next(wi)
  else if (wi$state == WI_NO_STATE) wi_update_no_state(wi)
  TRUE
}

wi_update_stats <- function(wi) {
  w <- wi$wbs
  game <- wi$game
  wi_update_animated(wi)
  if (wi$accelerate && wi$sp_state != 10L) {
    wi$accelerate <- 0L
    wi$cnt_kills <- wi_pct(w$skills, w$maxkills)
    wi$cnt_items <- wi_pct(w$sitems, w$maxitems)
    wi$cnt_secret <- wi_pct(w$ssecret, w$maxsecret)
    wi$cnt_time <- w$stime %/% TICRATE
    wi$cnt_par <- w$partime %/% TICRATE
    game$start_sound("barexp")
    wi$sp_state <- 10L
  }
  if (wi$sp_state == 2L) {
    wi$cnt_kills <- wi$cnt_kills + 2L
    if (bitwAnd(wi$bcnt, 3L) == 0L) game$start_sound("pistol")
    target <- wi_pct(w$skills, w$maxkills)
    if (wi$cnt_kills >= target) {
      wi$cnt_kills <- target
      game$start_sound("barexp")
      wi$sp_state <- wi$sp_state + 1L
    }
  } else if (wi$sp_state == 4L) {
    wi$cnt_items <- wi$cnt_items + 2L
    if (bitwAnd(wi$bcnt, 3L) == 0L) game$start_sound("pistol")
    target <- wi_pct(w$sitems, w$maxitems)
    if (wi$cnt_items >= target) {
      wi$cnt_items <- target
      game$start_sound("barexp")
      wi$sp_state <- wi$sp_state + 1L
    }
  } else if (wi$sp_state == 6L) {
    wi$cnt_secret <- wi$cnt_secret + 2L
    if (bitwAnd(wi$bcnt, 3L) == 0L) game$start_sound("pistol")
    target <- wi_pct(w$ssecret, w$maxsecret)
    if (wi$cnt_secret >= target) {
      wi$cnt_secret <- target
      game$start_sound("barexp")
      wi$sp_state <- wi$sp_state + 1L
    }
  } else if (wi$sp_state == 8L) {
    if (bitwAnd(wi$bcnt, 3L) == 0L) game$start_sound("pistol")
    wi$cnt_time <- wi$cnt_time + 3L
    ttime <- w$stime %/% TICRATE
    if (wi$cnt_time >= ttime) wi$cnt_time <- ttime
    wi$cnt_par <- wi$cnt_par + 3L
    ptime <- w$partime %/% TICRATE
    if (wi$cnt_par >= ptime) {
      wi$cnt_par <- ptime
      if (wi$cnt_time >= ttime) {
        game$start_sound("barexp")
        wi$sp_state <- wi$sp_state + 1L
      }
    }
  } else if (wi$sp_state == 10L) {
    if (wi$accelerate) {
      game$start_sound("wpnup")
      if (w$commercial) wi_init_no_state(wi) else wi_init_show_next(wi)
    }
  } else if (bitwAnd(wi$sp_state, 1L) != 0L) {
    wi$cnt_pause <- wi$cnt_pause - 1L
    if (wi$cnt_pause == 0L) {
      wi$sp_state <- wi$sp_state + 1L
      wi$cnt_pause <- TICRATE
      if (wi$sp_state == 2L) wi$cnt_kills <- 0L
      else if (wi$sp_state == 4L) wi$cnt_items <- 0L
      else if (wi$sp_state == 6L) wi$cnt_secret <- 0L
      else if (wi$sp_state == 8L) {
        wi$cnt_time <- 0L
        wi$cnt_par <- 0L
      }
    }
  }
  invisible()
}

wi_init_show_next <- function(wi) {
  wi$state <- WI_SHOW_NEXT
  wi$accelerate <- 0L
  wi$cnt <- SHOWNEXTLOCDELAY * TICRATE
  wi_init_animated(wi)
}

wi_update_show_next <- function(wi) {
  wi_update_animated(wi)
  wi$cnt <- wi$cnt - 1L
  if (wi$cnt == 0L || wi$accelerate) wi_init_no_state(wi)
  else wi$snl_pointeron <- bitwAnd(wi$cnt, 31L) < 20L
  invisible()
}

wi_init_no_state <- function(wi) {
  wi$state <- WI_NO_STATE
  wi$accelerate <- 0L
  wi$cnt <- 10L
}

wi_update_no_state <- function(wi) {
  wi_update_animated(wi)
  wi$cnt <- wi$cnt - 1L
  if (wi$cnt == 0L) wi$done <- TRUE
  invisible()
}

wi_pw <- function(patch) if (is.null(patch)) 8L else i16_at(patch, 0L)
wi_ph <- function(patch) if (is.null(patch)) 16L else i16_at(patch, 2L)

wi_draw_num <- function(wi, fb, x, y, n, digits) {
  fontw <- wi_pw(wi$num[[1]])
  if (digits < 0L) {
    digits <- if (n == 0L) 1L else 0L
    temp <- abs(as.integer(n))
    while (temp) {
      temp <- temp %/% 10L
      digits <- digits + 1L
    }
  }
  neg <- n < 0L
  if (neg) n <- -n
  while (digits > 0L) {
    digits <- digits - 1L
    x <- x - fontw
    d <- n %% 10L
    if (!is.null(wi$num[[d + 1L]])) fb <- draw_patch(fb, x, y, wi$num[[d + 1L]])
    n <- n %/% 10L
  }
  if (neg && !is.null(wi$p$minus)) {
    x <- x - 8L
    fb <- draw_patch(fb, x, y, wi$p$minus)
  }
  list(fb = fb, x = x)
}

wi_draw_percent <- function(wi, fb, x, y, value) {
  if (value < 0L) return(fb)
  if (!is.null(wi$p$percent)) fb <- draw_patch(fb, x, y, wi$p$percent)
  wi_draw_num(wi, fb, x, y, value, -1L)$fb
}

wi_draw_time <- function(wi, fb, x, y, t) {
  if (t < 0L) return(fb)
  if (t > 61L * 59L) {
    if (!is.null(wi$p$sucks)) fb <- draw_patch(fb, x - wi_pw(wi$p$sucks), y, wi$p$sucks)
    return(fb)
  }
  div <- 1L
  repeat {
    n <- (t %/% div) %% 60L
    drawn <- wi_draw_num(wi, fb, x, y, n, 2L)
    fb <- drawn$fb
    x <- drawn$x - wi_pw(wi$p$colon)
    div <- div * 60L
    if ((div == 60L || t %/% div) && !is.null(wi$p$colon)) fb <- draw_patch(fb, x, y, wi$p$colon)
    if (t %/% div == 0L) break
  }
  fb
}

wi_draw_centered <- function(fb, y, patch) {
  if (is.null(patch)) return(list(fb = fb, y = y))
  fb <- draw_patch(fb, (SCREENWIDTH - wi_pw(patch)) %/% 2L, y, patch)
  list(fb = fb, y = y + (5L * wi_ph(patch)) %/% 4L)
}

wi_draw_lf <- function(wi, fb) {
  y <- WI_TITLEY
  if (wi$wbs$last + 1L <= length(wi$lnames)) {
    placed <- wi_draw_centered(fb, y, wi$lnames[[wi$wbs$last + 1L]])
    fb <- placed$fb
    y <- placed$y
  }
  placed <- wi_draw_centered(fb, y, wi$p$finished)
  placed$fb
}

wi_draw_el <- function(wi, fb) {
  placed <- wi_draw_centered(fb, WI_TITLEY, wi$p$entering)
  fb <- placed$fb
  y <- placed$y
  if (wi$wbs$nxt + 1L <= length(wi$lnames)) {
    fb <- wi_draw_centered(fb, y, wi$lnames[[wi$wbs$nxt + 1L]])$fb
  }
  fb
}

wi_draw_on_lnode <- function(wi, fb, n, patches) {
  epsd <- wi$wbs$epsd
  if (epsd + 1L > length(WI_LNODES) || n + 1L > length(WI_LNODES[[epsd + 1L]])) return(fb)
  node <- WI_LNODES[[epsd + 1L]][[n + 1L]]
  for (patch in patches) {
    if (is.null(patch)) next
    w <- wi_pw(patch)
    h <- wi_ph(patch)
    left <- i16_at(patch, 4L)
    top <- i16_at(patch, 6L)
    if (node[1] - left >= 0L && node[1] - left + w < SCREENWIDTH &&
        node[2] - top >= 0L && node[2] - top + h < SCREENHEIGHT) {
      return(draw_patch(fb, node[1], node[2], patch))
    }
  }
  fb
}

wi_draw_bg <- function(wi, fb) {
  if (!is.null(wi$background)) fb <- draw_patch(fb, 0L, 0L, wi$background)
  if (wi$wbs$commercial || wi$wbs$epsd > 2L) return(fb)
  for (a in wi$anims) {
    if (a$ctr >= 0L && a$ctr < length(a$patches) && !is.null(a$patches[[a$ctr + 1L]])) {
      fb <- draw_patch(fb, a$x, a$y, a$patches[[a$ctr + 1L]])
    }
  }
  fb
}

wi_draw_stats <- function(wi, fb) {
  fb <- wi_draw_bg(wi, fb)
  fb <- wi_draw_lf(wi, fb)
  lh <- if (is.null(wi$num[[1]])) 24L else (3L * wi_ph(wi$num[[1]])) %/% 2L
  if (!is.null(wi$p$kills)) fb <- draw_patch(fb, SP_STATSX, SP_STATSY, wi$p$kills)
  fb <- wi_draw_percent(wi, fb, SCREENWIDTH - SP_STATSX, SP_STATSY, wi$cnt_kills)
  if (!is.null(wi$p$items)) fb <- draw_patch(fb, SP_STATSX, SP_STATSY + lh, wi$p$items)
  fb <- wi_draw_percent(wi, fb, SCREENWIDTH - SP_STATSX, SP_STATSY + lh, wi$cnt_items)
  if (!is.null(wi$p$sp_secret)) fb <- draw_patch(fb, SP_STATSX, SP_STATSY + 2L * lh, wi$p$sp_secret)
  fb <- wi_draw_percent(wi, fb, SCREENWIDTH - SP_STATSX, SP_STATSY + 2L * lh, wi$cnt_secret)
  if (!is.null(wi$p$time)) fb <- draw_patch(fb, SP_TIMEX, SP_TIMEY, wi$p$time)
  fb <- wi_draw_time(wi, fb, SCREENWIDTH %/% 2L - SP_TIMEX, SP_TIMEY, wi$cnt_time)
  if (wi$wbs$epsd < 3L) {
    if (!is.null(wi$p$par)) fb <- draw_patch(fb, SCREENWIDTH %/% 2L + SP_TIMEX, SP_TIMEY, wi$p$par)
    fb <- wi_draw_time(wi, fb, SCREENWIDTH - SP_TIMEX, SP_TIMEY, wi$cnt_par)
  }
  fb
}

wi_draw_show_next <- function(wi, fb) {
  fb <- wi_draw_bg(wi, fb)
  pointer <- wi$snl_pointeron || wi$state == WI_NO_STATE
  if (!wi$wbs$commercial && wi$wbs$epsd <= 2L) {
    last <- if (wi$wbs$last == 8L) wi$wbs$nxt - 1L else wi$wbs$last
    for (i in seq_len(max(0L, last + 1L)) - 1L) fb <- wi_draw_on_lnode(wi, fb, i, list(wi$p$splat))
    if (isTRUE(wi$wbs$didsecret)) fb <- wi_draw_on_lnode(wi, fb, 8L, list(wi$p$splat))
    if (pointer) fb <- wi_draw_on_lnode(wi, fb, wi$wbs$nxt, list(wi$p$yah0, wi$p$yah1))
  }
  if (!wi$wbs$commercial || wi$wbs$nxt != 30L) fb <- wi_draw_el(wi, fb)
  fb
}

wi_draw <- function(wi, fb) {
  if (wi$state == WI_STAT_COUNT) wi_draw_stats(wi, fb) else wi_draw_show_next(wi, fb)
}
