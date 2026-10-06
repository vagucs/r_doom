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
# Arranque: acha o IWAD, desenha o TITLEPIC e segura a janela.

find_iwad <- function(explicit = NULL) {
  if (!is.null(explicit) && nzchar(explicit)) {
    if (file.exists(explicit)) return(normalizePath(explicit, winslash = "/"))
    stop("IWAD not found: ", explicit)
  }
  here <- normalizePath(".", winslash = "/")
  roots <- c(here, dirname(here), dirname(dirname(here)))
  env <- Sys.getenv(c("DOOMWADDIR", "DOOMWADPATH"), unset = "")
  env <- env[nzchar(env)]
  if (length(env)) roots <- c(strsplit(env[[1]], .Platform$path.sep, fixed = TRUE)[[1]], roots)
  for (root in unique(roots)) {
    for (name in IWAD_NAMES) {
      path <- file.path(root, name)
      if (file.exists(path)) return(normalizePath(path, winslash = "/"))
    }
  }
  stop("No IWAD found. Put doom1.wad in this folder or pass -iwad file.wad")
}

parse_args <- function(argv) {
  game <- new.env(parent = emptyenv())
  game$iwad <- NULL
  game$files <- character()
  game$snapshot <- NULL
  game$show_fps <- FALSE
  game$crt <- FALSE
  i <- 1L
  n <- length(argv)
  while (i <= n) {
    a <- argv[[i]]
    if (a == "-iwad" && i < n) {
      game$iwad <- argv[[i + 1L]]
      i <- i + 2L
    } else if (a == "-file") {
      i <- i + 1L
      while (i <= n && !startsWith(argv[[i]], "-")) {
        game$files <- c(game$files, argv[[i]])
        i <- i + 1L
      }
    } else if (a == "--snapshot" && i < n) {
      game$snapshot <- argv[[i + 1L]]
      i <- i + 2L
    } else if (a == "-fps") {
      game$show_fps <- TRUE
      i <- i + 1L
    } else if (a == "-crt") {
      game$crt <- TRUE
      i <- i + 1L
    } else if (!startsWith(a, "-") && grepl("\\.wad$", a, ignore.case = TRUE)) {
      game$iwad <- a
      i <- i + 1L
    } else {
      i <- i + 1L
    }
  }
  game
}

draw_title <- function(video, wad) {
  video$fb <- raw(SCREENPIXELS)
  if (wad_check_num(wad, "TITLEPIC") >= 0L) {
    video$fb <- draw_patch(video$fb, 0L, 0L, wad_cache_name(wad, "TITLEPIC"))
  }
  invisible(video)
}

norm_key <- function(key) {
  k <- as.character(key)[[1]]
  if (k %in% c("\033", "escape", "Esc", "Escape")) return("escape")
  if (k %in% c("\r", "\n", "Return", "Enter", "KP_Enter")) return("return")
  if (k %in% c("\b", "BackSpace", "Backspace")) return("backspace")
  if (k %in% c(" ", "space")) return("space")
  if (k %in% c("Control_L", "Control_R", "Control")) return("ctrl")
  if (k %in% c("Shift_L", "Shift_R", "Shift")) return("shift")
  if (k %in% c("Alt_L", "Alt_R", "Alt", "Meta_L", "Meta_R")) return("alt")
  if (k %in% c("comma", "less")) return("comma")
  if (k %in% c("period", "greater")) return("period")
  if (k %in% c("Up", "Down", "Left", "Right")) return(tolower(k))
  if (grepl("^[Ff][0-9]{1,2}$", k)) return(tolower(k))
  if (grepl("^ctrl-", k, ignore.case = TRUE)) return(tolower(k))
  if (nchar(k) == 1L) return(tolower(k))
  tolower(k)
}

note_key <- function(game, key) {
  now <- proc.time()[["elapsed"]]
  game$down[[key]] <- now
  if (startsWith(key, "ctrl")) game$down[["ctrl"]] <- now
}

key_down <- function(game, name) !is.null(game$down[[name]])

draw_fps <- function(game, fb) {
  text <- sprintf("%d FPS", game$fps_value)
  width <- menu_string_width(game$menu, text)
  x <- max(0L, SCREENWIDTH - as.integer(width) - 2L)
  menu_write_text(game$menu, fb, x, 1L, text)$fb
}

compose_frame <- function(game) {
  if (identical(game$gamestate, GS_INTERMISSION) && !is.null(game$wi)) {
    game$video$fb <- wi_draw(game$wi, raw(SCREENPIXELS))
  } else if (identical(game$gamestate, GS_FINALE)) {
    game$video$fb <- raw(SCREENPIXELS)
    game$video$fb <- menu_write_text(game$menu, game$video$fb, 48L, 80L, "FIM DO EPISODIO")$fb
    game$video$fb <- menu_write_text(game$menu, game$video$fb, 16L, 100L, "ENTER VOLTA AO TITULO")$fb
  } else if (identical(game$gamestate, GS_LEVEL) && !is.null(game$am) && isTRUE(game$am$active)) {
    game$video$fb <- am_draw(game)
  } else {
    game$video$fb <- game$base_fb
  }
  if (identical(game$gamestate, GS_LEVEL) && !is.null(game$player) && !is.null(game$status)) {
    game$video$fb <- status_draw(game$status, game$video$fb, game$player)
  }
  player <- game$player
  if (identical(game$gamestate, GS_LEVEL) && !is.null(player) && isTRUE(game$show_messages) &&
      is.character(player$message) && nzchar(player$message)) {
    game$video$fb <- menu_write_text(game$menu, game$video$fb, 0L, 0L, player$message)$fb
  }
  if (game$menu$active) game$video$fb <- menu_draw(game$menu, game$video$fb)
  if (isTRUE(game$show_fps)) game$video$fb <- draw_fps(game, game$video$fb)
}

count_level_stats <- function(game) {
  kills <- 0L
  items <- 0L
  for (mo in game$world$mobjs) {
    if (!is.null(mo$player)) next
    if (has_flag(mo$flags, MF_COUNTKILL)) kills <- kills + 1L
    if (has_flag(mo$flags, MF_COUNTITEM)) items <- items + 1L
  }
  secrets <- 0L
  for (sec in game$world$sectors) if (as.integer(sec$special) == 9L) secrets <- secrets + 1L
  game$totalkills <- kills
  game$totalitems <- items
  game$totalsecret <- secrets
  invisible()
}

start_level <- function(game, carry = FALSE) {
  prev <- if (isTRUE(carry)) game$player else NULL
  init_tables()
  resources_set_sky(game$res, game$episode, game$mapn)
  game$world <- world_setup(game$wad, game$episode, game$mapn)
  world_bind_textures(game$world, game$res)
  world_spawn_things(game$world, game$skill)
  for (mo in game$world$mobjs) set_thing_position(game$world, mo)
  game$world$dirty <- FALSE
  game$start_sound <- function(name) sound_play(game$sound, name)
  game$touch_special <- function(special, toucher) touch_special(game, special, toucher)
  game$use_special <- function(line, mo, side) specials_use(game$specials, line, mo, side)
  game$shoot_special <- function(line, source) specials_shoot(game$specials, line, source)
  game$cross_special <- function(line, side, thing) specials_cross(game$specials, line, side, thing)
  game$damage_mobj <- function(target, source, damage, inflictor = NULL) {
    damage_mobj(game, target, source, damage, inflictor)
  }
  game$specials <- specials_new(game$world, game$res, game)
  game$player <- spawn_player(game$world, player_start(game$world))
  if (!is.null(prev)) carry_player(game$player, prev)
  game$player$killcount <- 0L
  game$player$itemcount <- 0L
  game$player$secretcount <- 0L
  count_level_stats(game)
  game$leveltime <- 0
  sound_play_level(game$sound, game$episode, game$mapn)
  enemy_init(game)
  game$turnheld <- 0L
  game$view_sig <- ""
  game$need_view <- TRUE
  game$view_blocks <- NULL
  am_reset(game$am)
  game$gamestate <- GS_LEVEL
  game$menu$dirty <- TRUE
  w <- game$world
  sec <- if (length(w$sectors)) w$sectors[[1]] else NULL
  message(sprintf(
    "%s carregado: %d vertices, %d linhas, %d lados, %d setores, %d segs, %d ssectors, %d nodes, %d things, chao %s, teto %s",
    w$name, length(w$vertexes), length(w$lines), length(w$sides),
    length(w$sectors), length(w$segs), length(w$subsectors),
    length(w$nodes), length(w$things),
    if (is.null(sec)) "-" else sec$floorflat,
    if (is.null(sec)) "-" else sec$ceilingflat
  ))
}

return_to_title <- function(game) {
  game$gamestate <- GS_TITLE
  game$st_palette <- -1L
  if (!is.null(game$video)) video_select_palette(game$video, 0L)
  game$st_palette <- 0L
  sound_play_title(game$sound)
  game$player <- NULL
  if (!is.null(game$am)) game$am$active <- FALSE
  game$specials <- NULL
  game$world <- NULL
  game$wi <- NULL
  game$pending_start <- FALSE
  game$need_view <- FALSE
  if (!is.null(game$title_fb)) game$base_fb <- game$title_fb
  game$menu$dirty <- TRUE
  invisible(TRUE)
}

level_lump_name <- function(game, mapn) {
  if (wad_check_num(game$wad, "MAP01") >= 0L) sprintf("MAP%02d", as.integer(mapn))
  else sprintf("E%dM%d", as.integer(game$episode), as.integer(mapn))
}

next_map_number <- function(game, secret) {
  mapn <- as.integer(game$mapn)
  commercial <- wad_check_num(game$wad, "MAP01") >= 0L
  if (commercial) {
    if (secret && mapn == 15L) 31L
    else if (secret && mapn == 31L) 32L
    else if (mapn %in% c(31L, 32L)) 16L
    else mapn + 1L
  } else if (secret) {
    9L
  } else if (mapn == 9L) {
    c(4L, 6L, 7L, 3L)[max(1L, min(4L, as.integer(game$episode)))]
  } else {
    mapn + 1L
  }
}

begin_finale <- function(game) {
  game$gamestate <- GS_FINALE
  game$st_palette <- -1L
  if (!is.null(game$video)) video_select_palette(game$video, 0L)
  game$st_palette <- 0L
  if (!is.null(game$player)) {
    game$player$attackdown <- TRUE
    game$player$usedown <- TRUE
  }
  sound_change_music(game$sound, "victor", TRUE)
  game$menu$dirty <- TRUE
  invisible()
}

complete_level <- function(game) {
  secret <- isTRUE(game$specials$secret_exit)
  if (secret && !is.null(game$player)) game$player$didsecret <- TRUE
  commercial <- wad_check_num(game$wad, "MAP01") >= 0L
  if (!is.null(game$player)) {
    game$player$attackdown <- TRUE
    game$player$usedown <- TRUE
    game$player$cards <- rep(FALSE, 6L)
    game$player$damagecount <- 0L
    game$player$bonuscount <- 0L
    game$player$extralight <- 0L
  }
  game$st_palette <- -1L
  if (!is.null(game$video)) video_select_palette(game$video, 0L)
  game$st_palette <- 0L
  if (!commercial && game$mapn == 8L && !secret) {
    begin_finale(game)
    return(invisible())
  }
  nxt <- next_map_number(game, secret)
  p <- game$player
  game$next_mapn <- nxt
  game$wi <- wi_new(game, list(
    epsd = as.integer(game$episode) - 1L,
    last = as.integer(game$mapn) - 1L,
    nxt = nxt - 1L,
    maxkills = max(1L, game$totalkills),
    maxitems = max(1L, game$totalitems),
    maxsecret = max(1L, game$totalsecret),
    partime = wi_partime(game$episode, game$mapn, commercial),
    skills = if (is.null(p)) 0L else p$killcount,
    sitems = if (is.null(p)) 0L else p$itemcount,
    ssecret = if (is.null(p)) 0L else p$secretcount,
    stime = game$leveltime,
    didsecret = !is.null(p) && isTRUE(p$didsecret),
    commercial = commercial
  ))
  game$gamestate <- GS_INTERMISSION
  game$menu$dirty <- TRUE
  invisible()
}

finish_intermission <- function(game) {
  nxt <- game$next_mapn
  if (wad_check_num(game$wad, level_lump_name(game, nxt)) < 0L) {
    return_to_title(game)
    return(invisible())
  }
  game$mapn <- nxt
  game$wi <- NULL
  start_level(game, TRUE)
  invisible()
}

game_ticker <- function(game) {
  if (isTRUE(game$pending_start)) {
    game$pending_start <- FALSE
    start_level(game)
    return(TRUE)
  }
  if (identical(game$gamestate, GS_INTERMISSION) && !is.null(game$wi)) {
    wi_ticker(game$wi)
    if (isTRUE(game$wi$done)) finish_intermission(game)
    return(TRUE)
  }
  if (identical(game$gamestate, GS_FINALE)) {
    p <- game$player
    attack <- key_down(game, "ctrl")
    use <- key_down(game, "space") || key_down(game, "e") || key_down(game, "return")
    if (is.null(p)) {
      if (attack || use) return_to_title(game)
    } else {
      go <- (attack && !isTRUE(p$attackdown)) || (use && !isTRUE(p$usedown))
      p$attackdown <- attack
      p$usedown <- use
      if (go) return_to_title(game)
    }
    return(TRUE)
  }
  if (!identical(game$gamestate, GS_LEVEL) || is.null(game$player)) return(FALSE)
  if (isTRUE(game$menu$active)) game$player$cmd <- ticcmd_new() else game$player$cmd <- build_ticcmd(game)
  tryCatch(
    player_think(game$world, game$player, game, game$leveltime),
    error = function(e) message("jogador: ", conditionMessage(e), " em ", paste(deparse(conditionCall(e)), collapse = " "))
  )
  tryCatch(
    tick_enemies(game$world, game),
    error = function(e) message("inimigo: ", conditionMessage(e))
  )
  if (!is.null(game$status)) status_ticker(game$status, game$player)
  tick_fx(game$world)
  if (!is.null(game$specials)) {
    specials_tick(game$specials)
    if (isTRUE(game$specials$exit_requested)) {
      message(if (isTRUE(game$specials$secret_exit)) "saida secreta" else "saiu da fase")
      complete_level(game)
      return(TRUE)
    }
  }
  game$leveltime <- game$leveltime + 1
  if (game$player$playerstate == PST_REBORN) {
    start_level(game)
    return(TRUE)
  }
  FALSE
}

apply_status_palette <- function(game) {
  pal <- 0L
  p <- game$player
  if (!is.null(p) && identical(game$gamestate, GS_LEVEL)) {
    cnt <- p$damagecount
    strength <- p$powers[PW_STRENGTH + 1L]
    if (strength) {
      bzc <- 12 - (strength %/% 64)
      if (bzc > cnt) cnt <- bzc
    }
    if (cnt) {
      pal <- bitwShiftR(as.integer(cnt + 7), 3L)
      if (is.na(pal) || pal >= 8L) pal <- 7L
      pal <- pal + 1L
    } else if (p$bonuscount) {
      pal <- bitwShiftR(as.integer(p$bonuscount + 7), 3L)
      if (is.na(pal) || pal >= 4L) pal <- 3L
      pal <- pal + 9L
    } else {
      iron <- p$powers[PW_IRONFEET + 1L]
      if (iron > 128 || bitwAnd(as.integer(iron), 8L) != 0L) pal <- 13L
    }
  }
  pal <- as.integer(pal)
  if (identical(pal, game$st_palette)) return(FALSE)
  game$st_palette <- pal
  if (!is.null(game$video)) video_select_palette(game$video, pal)
  TRUE
}

present_level <- function(game) {
  if (!identical(game$gamestate, GS_LEVEL) || is.null(game$player)) return(FALSE)
  if (!is.null(game$am) && isTRUE(game$am$active)) return(TRUE)
  sig <- view_signature(game)
  size_changed <- !identical(game$view_blocks, game$screen_size) ||
    !identical(as.integer(game$view_detail), as.integer(game$detail_level))
  if (isTRUE(game$menu$active) && !size_changed && !isTRUE(game$need_view)) return(FALSE)
  moved <- !is.null(game$world) && isTRUE(game$world$dirty)
  if (!isTRUE(game$need_view) && !size_changed && !moved && identical(sig, game$view_sig)) return(FALSE)
  render_level_view(game)
  game$view_sig <- sig
  game$world$dirty <- FALSE
  TRUE
}

change_screen_size <- function(game, delta) {
  size <- game$screen_size + delta
  if (size < 0L || size > 8L) return(invisible())
  game$screen_size <- size
  game$need_view <- TRUE
  game$menu$dirty <- TRUE
  sound_play(game$sound, "stnmov")
  invisible()
}

on_key <- function(game, down, key) {
  if (!isTRUE(game$running) || !isTRUE(game$video$open)) return(invisible(NULL))
  name <- norm_key(key)
  if (!nzchar(name)) return(invisible(NULL))
  if (down && name == "return" && key_down(game, "alt")) {
    video_toggle_fullscreen(game$video)
    return(invisible(NULL))
  }
  if (down && (name == "minus" || name == "equals")) {
    if (!isTRUE(game$menu$active) && !is.null(game$am) && isTRUE(game$am$active)) {
      am_responder(game, name)
      game$menu$dirty <- TRUE
    } else {
      change_screen_size(game, if (name == "minus") -1L else 1L)
    }
  } else if (down) {
    if (!menu_responder(game$menu, name)) {
      if (!isTRUE(game$menu$active) && am_responder(game, name)) {
        game$menu$dirty <- TRUE
      } else {
        note_key(game, name)
        feed_cheats(game, name)
      }
    }
    if (name == "ctrl") note_key(game, "ctrl")
  } else {
    game$down[[name]] <- NULL
  }
  if (!isTRUE(game$running)) return(invisible(NULL))
  if (isTRUE(game$menu$dirty)) {
    game$menu$dirty <- FALSE
    compose_frame(game)
    video_blit(game$video)
  }
  invisible(NULL)
}

dispatch_events <- function(game) {
  for (ev in video_poll_events()) {
    kind <- ev[[1]]
    if (identical(kind, "quit")) {
      game$running <- FALSE
      return(invisible(NULL))
    }
    on_key(game, identical(kind, "down"), ev[[2]])
  }
  invisible(NULL)
}

run_loop <- function(game) {
  video_init(game$video)
  on.exit(video_shutdown(game$video), add = TRUE)
  compose_frame(game)
  video_present(game$video)
  sound_play_title(game$sound)
  message("Esc ou Enter abre o menu. Setas, Enter, Backspace, F1 ajuda, Y/N confirma.")
  game$last_t <- proc.time()[["elapsed"]]
  game$accum <- 0
  game$tic <- 0L
  game$tic_budget <- 4L
  while (isTRUE(game$running)) {
    dispatch_events(game)
    if (!isTRUE(game$running)) break
    tryCatch(tick_body(game), error = function(e) message(conditionMessage(e)))
    sound_pump()
    sound_update(game$sound)
    Sys.sleep(0.001)
  }
  invisible(NULL)
}

arm_wipe <- function(game) {
  if (is.null(game$shown_state) || is.null(game$wipe)) return(FALSE)
  if (identical(game$shown_state, game$gamestate)) return(FALSE)
  if (identical(game$shown_state, GS_TITLE)) return(FALSE)
  TRUE
}

tick_body <- function(game) {
    now <- proc.time()[["elapsed"]]
    game$accum <- game$accum + (now - game$last_t) * 1000
    game$last_t <- now
    game$tic_budget <- 4L
    stepped <- FALSE
    while (game$accum >= 1000 / TICRATE && game$tic_budget > 0L) {
      game$accum <- game$accum - 1000 / TICRATE
      game$tic <- game$tic + 1L
      game$tic_budget <- game$tic_budget - 1L
      if (isTRUE(game$wipe$active)) {
        game$video$fb <- wipe_tick(game$wipe, game$video$fb)
        stepped <- TRUE
        next
      }
      tryCatch({
        if (menu_ticker(game$menu)) stepped <- TRUE
        if (game_ticker(game)) stepped <- TRUE
      }, error = function(e) message(conditionMessage(e)))
    }
    if (game$accum > 1000) game$accum <- 1000
    if (isTRUE(game$wipe$active)) {
      if (isTRUE(game$menu$active)) game$video$fb <- menu_draw(game$menu, game$video$fb)
      video_blit(game$video)
      return(invisible(NULL))
    }
    do_wipe <- arm_wipe(game)
    old_fb <- if (do_wipe) c(game$video$fb) else NULL
    view_drew <- present_level(game)
    if (view_drew) stepped <- TRUE
    if (apply_status_palette(game)) stepped <- TRUE
    now <- proc.time()[["elapsed"]]
    fps_refresh <- FALSE
    if (isTRUE(game$show_fps)) {
      if (!is.finite(game$fps_stamp)) game$fps_stamp <- now
      if (now - game$fps_stamp >= 1) {
        game$fps_value <- game$fps_frames
        game$fps_frames <- 0L
        game$fps_stamp <- now
        fps_refresh <- TRUE
      }
    }
    if (stepped || isTRUE(game$menu$dirty) || fps_refresh || do_wipe) {
      game$menu$dirty <- FALSE
      compose_frame(game)
      if (do_wipe && wipe_begin(game$wipe, old_fb, game$video$fb)) {
        game$video$fb <- c(game$wipe$start)
      }
      game$shown_state <- game$gamestate
      video_blit(game$video)
      if (isTRUE(game$show_fps) && isTRUE(view_drew)) game$fps_frames <- game$fps_frames + 1L
    } else if (is.null(game$shown_state)) {
      game$shown_state <- game$gamestate
    }
    invisible(NULL)
}

doom_main <- function(argv = commandArgs(trailingOnly = TRUE)) {
  args <- parse_args(argv)
  iwad <- find_iwad(args$iwad)
  message("IWAD: ", iwad)
  wad <- wad_new()
  wad_add_file(wad, iwad)
  for (extra in args$files) {
    message("PWAD: ", extra)
    wad_add_file(wad, extra)
  }
  video <- video_new()
  video_set_playpal(video, wad_cache_name(wad, "PLAYPAL"))
  draw_title(video, wad)
  game <- args
  game$wad <- wad
  game$video <- video
  video$crt <- isTRUE(game$crt)
  game$wipe <- wipe_new()
  game$cheats <- cheats_new()
  game$am <- am_new()
  game$shown_state <- NULL
  game$title_fb <- c(video$fb)
  game$base_fb <- game$title_fb
  game$world <- NULL
  game$gamestate <- GS_TITLE
  game$skill <- 2L
  game$episode <- 1L
  game$mapn <- 1L
  game$show_messages <- TRUE
  game$fps_frames <- 0L
  game$fps_stamp <- NA_real_
  game$fps_value <- 0L
  game$detail_level <- 0L
  game$screen_size <- 7L
  game$mouse_sensitivity <- 5L
  game$player <- NULL
  game$pending_start <- FALSE
  game$running <- TRUE
  game$down <- list()
  game$sound <- sound_new()
  sound_init(game$sound, wad)
  game$st_palette <- 0L
  game$res <- resources_new(wad)
  resources_init(game$res)
  message(sprintf(
    "texturas: %d, flats: %d, ceu %s",
    length(game$res$textures), flat_count(game$res),
    game$res$textures[[game$res$skytexture + 1L]]$name
  ))
  game$status <- status_new(wad)
  game$menu <- menu_new(wad, game$sound, game)
  if (!is.null(game$snapshot)) {
    video_snapshot(video, game$snapshot)
    message("snapshot: ", normalizePath(game$snapshot, winslash = "/", mustWork = FALSE))
    return(0L)
  }
  run_loop(game)
  0L
}
