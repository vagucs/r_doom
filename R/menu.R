# Menu do Doom: Novo Jogo, Opcoes, Load, Save, Leia isto, Sair.
# Teclas no nome que o device windows() entrega.

LINEHEIGHT <- 16L
SKULLXOFF <- -32L

menu_item <- function(status, name, action, alpha = 0L) {
  list(status = status, name = name, action = action, alpha = alpha)
}

menu_def <- function(items, routine, x, y, last_on = 0L, prev = NULL) {
  list(items = items, routine = routine, x = x, y = y, last_on = last_on, prev = prev)
}

menu_new <- function(wad, sound, game) {
  e <- new.env(parent = emptyenv())
  e$wad <- wad
  e$sound <- sound
  e$game <- game
  e$active <- FALSE
  e$screen <- "main"
  e$item_on <- 0L
  e$which_skull <- 0L
  e$skull_tics <- 8L
  e$epi <- 0L
  e$message <- NULL
  e$message_confirm <- FALSE
  e$message_action <- NULL
  e$save_strings <- rep(LOADSAVEEMPTY, 6L)
  e$save_slot_ok <- rep(FALSE, 6L)
  e$save_string_enter <- FALSE
  e$save_slot <- 0L
  e$save_old_string <- ""
  e$save_char_index <- 0L
  e$dirty <- TRUE
  e$menus <- list(
    main = menu_def(list(
      menu_item(1L, "M_NGAME", "newgame", 110L),
      menu_item(1L, "M_OPTION", "options", 111L),
      menu_item(1L, "M_LOADG", "loadgame", 108L),
      menu_item(1L, "M_SAVEG", "savegame", 115L),
      menu_item(1L, "M_RDTHIS", "readthis", 114L),
      menu_item(1L, "M_QUITG", "quit", 113L)
    ), "main", 97L, 64L),
    episode = menu_def(list(
      menu_item(1L, "M_EPI1", "episode", 107L),
      menu_item(1L, "M_EPI2", "episode", 116L),
      menu_item(1L, "M_EPI3", "episode", 105L),
      menu_item(1L, "M_EPI4", "episode", 116L)
    ), "episode", 48L, 63L, prev = "main"),
    skill = menu_def(list(
      menu_item(1L, "M_JKILL", "skill", 105L),
      menu_item(1L, "M_ROUGH", "skill", 104L),
      menu_item(1L, "M_HURT", "skill", 104L),
      menu_item(1L, "M_ULTRA", "skill", 117L),
      menu_item(1L, "M_NMARE", "skill", 110L)
    ), "skill", 48L, 63L, last_on = 2L, prev = "episode"),
    options = menu_def(list(
      menu_item(1L, "M_ENDGAM", "endgame", 101L),
      menu_item(1L, "M_MESSG", "messages", 109L),
      menu_item(1L, "M_DETAIL", "detail", 103L),
      menu_item(2L, "M_SCRNSZ", "scrnsize", 115L),
      menu_item(-1L, "", "", 0L),
      menu_item(2L, "M_MSENS", "mousesens", 109L),
      menu_item(-1L, "", "", 0L),
      menu_item(1L, "M_SVOL", "sound", 115L)
    ), "options", 60L, 37L, prev = "main"),
    sound = menu_def(list(
      menu_item(2L, "M_SFXVOL", "sfxvol", 115L),
      menu_item(-1L, "", "", 0L),
      menu_item(2L, "M_MUSVOL", "musvol", 109L),
      menu_item(-1L, "", "", 0L)
    ), "sound", 80L, 64L, prev = "options"),
    load = menu_def(lapply(seq_len(6L), function(i) menu_item(1L, "", "loadslot")),
                    "load", 80L, 54L, prev = "main"),
    save = menu_def(lapply(seq_len(6L), function(i) menu_item(1L, "", "saveslot")),
                    "save", 80L, 54L, prev = "main"),
    read1 = menu_def(list(menu_item(1L, "", "read2", 0L)), "read1", 280L, 185L, prev = "main"),
    read2 = menu_def(list(menu_item(1L, "", "finishread", 0L)), "read2", 330L, 175L, prev = "read1")
  )
  if (!menu_has_episodes(e)) e$menus$skill$prev <- "main"
  e
}

menu_has_episodes <- function(menu) {
  if (wad_check_num(menu$wad, "MAP01") >= 0L) return(FALSE)
  wad_check_num(menu$wad, "E2M1") >= 0L
}

menu_has <- function(menu, name) wad_check_num(menu$wad, name) >= 0L

menu_patch <- function(menu, name) {
  n <- wad_check_num(menu$wad, name)
  if (n < 0L) NULL else wad_cache_num(menu$wad, n)
}

menu_ticker <- function(menu) {
  if (!menu$active) return(FALSE)
  menu$skull_tics <- menu$skull_tics - 1L
  if (menu$skull_tics <= 0L) {
    menu$which_skull <- bitwXor(menu$which_skull, 1L)
    menu$skull_tics <- 8L
    menu$dirty <- TRUE
    return(TRUE)
  }
  FALSE
}

menu_start <- function(menu) {
  if (menu$active) return(invisible())
  menu$active <- TRUE
  menu$screen <- "main"
  menu$item_on <- menu$menus$main$last_on
  menu$message <- NULL
  menu$save_string_enter <- FALSE
  menu$dirty <- TRUE
  sound_play(menu$sound, "swtchn")
}

menu_clear <- function(menu) {
  menu$active <- FALSE
  menu$message <- NULL
  menu$save_string_enter <- FALSE
  menu$dirty <- TRUE
}

menu_goto <- function(menu, name) {
  menu$menus[[menu$screen]]$last_on <- menu$item_on
  menu$screen <- name
  menu$item_on <- menu$menus[[name]]$last_on
  menu$dirty <- TRUE
}

menu_responder <- function(menu, key) {
  if (menu$save_string_enter) return(menu_save_string_key(menu, key))
  if (!is.null(menu$message)) return(menu_message_key(menu, key))
  if (key == "f1") {
    menu_open_help(menu)
    return(TRUE)
  }
  if (key == "f2") {
    menu_action(menu, "savegame", 0L)
    return(TRUE)
  }
  if (key == "f3") {
    menu_action(menu, "loadgame", 0L)
    return(TRUE)
  }
  if (!menu$active) {
    on_title <- is.null(menu$game) || identical(menu$game$gamestate, GS_TITLE)
    if (key == "escape" || (key == "return" && on_title)) {
      menu_start(menu)
      return(TRUE)
    }
    return(FALSE)
  }
  def <- menu$menus[[menu$screen]]
  n <- length(def$items)
  if (key == "escape") {
    def$last_on <- menu$item_on
    menu$menus[[menu$screen]] <- def
    menu_clear(menu)
    sound_play(menu$sound, "swtchx")
    return(TRUE)
  }
  if (key == "backspace") {
    def$last_on <- menu$item_on
    menu$menus[[menu$screen]] <- def
    if (!is.null(def$prev)) {
      menu$screen <- def$prev
      menu$item_on <- menu$menus[[menu$screen]]$last_on
    } else {
      menu_clear(menu)
    }
    sound_play(menu$sound, "swtchx")
    menu$dirty <- TRUE
    return(TRUE)
  }
  if (key == "down") {
    repeat {
      menu$item_on <- (menu$item_on + 1L) %% n
      if (def$items[[menu$item_on + 1L]]$status != -1L) break
    }
    sound_play(menu$sound, "pstop")
    menu$dirty <- TRUE
    return(TRUE)
  }
  if (key == "up") {
    repeat {
      menu$item_on <- (menu$item_on - 1L) %% n
      if (def$items[[menu$item_on + 1L]]$status != -1L) break
    }
    sound_play(menu$sound, "pstop")
    menu$dirty <- TRUE
    return(TRUE)
  }
  if (key %in% c("left", "right")) {
    item <- def$items[[menu$item_on + 1L]]
    if (item$status == 2L && nzchar(item$action)) {
      sound_play(menu$sound, "stnmov")
      menu_action(menu, item$action, if (key == "left") 0L else 1L)
    }
    return(TRUE)
  }
  if (key == "return") {
    item <- def$items[[menu$item_on + 1L]]
    if (item$status != 0L) {
      def$last_on <- menu$item_on
      menu$menus[[menu$screen]] <- def
      if (!identical(item$action, "skill")) sound_play(menu$sound, "pistol")
      choice <- if (item$status == 2L) 1L else menu$item_on
      menu_action(menu, item$action, choice)
    }
    return(TRUE)
  }
  TRUE
}

menu_message_key <- function(menu, key) {
  if (menu$message_confirm) {
    if (key %in% c("y", "return")) {
      action <- menu$message_action
      menu$message <- NULL
      menu$dirty <- TRUE
      if (identical(action, "quit")) menu$game$running <- FALSE
      if (identical(action, "endgame")) {
        menu$game$gamestate <- GS_TITLE
        menu$game$pending_start <- FALSE
        menu$game$world <- NULL
        menu$game$need_view <- FALSE
        if (!is.null(menu$game$title_fb)) menu$game$base_fb <- menu$game$title_fb
        menu_clear(menu)
      }
      return(TRUE)
    }
    if (key %in% c("n", "escape")) {
      menu$message <- NULL
      menu$dirty <- TRUE
    }
    return(TRUE)
  }
  menu$message <- NULL
  menu$dirty <- TRUE
  TRUE
}

menu_action <- function(menu, action, choice) {
  game <- menu$game
  if (action == "newgame") {
    if (wad_check_num(menu$wad, "MAP01") >= 0L || !menu_has_episodes(menu)) {
      menu$epi <- 0L
      menu_goto(menu, "skill")
    } else {
      menu_goto(menu, "episode")
    }
  } else if (action == "options") {
    menu_goto(menu, "options")
  } else if (action == "loadgame") {
    menu_open_load(menu)
  } else if (action == "savegame") {
    if (!identical(game$gamestate, GS_LEVEL) || is.null(game$player)) {
      sound_play(menu$sound, "oof")
      return(invisible())
    }
    menu_open_save(menu)
  } else if (action == "loadslot") {
    if (!menu$save_slot_ok[choice + 1L]) {
      sound_play(menu$sound, "oof")
      return(invisible())
    }
    sound_play(menu$sound, "oof")
  } else if (action == "saveslot") {
    menu_begin_save_name(menu, choice)
  } else if (action == "readthis") {
    menu_goto(menu, "read1")
  } else if (action == "read2") {
    if (menu_has(menu, "HELP1") && menu$screen == "read1") menu_goto(menu, "read2")
    else menu_goto(menu, "main")
  } else if (action == "finishread") {
    menu_goto(menu, "main")
  } else if (action == "quit") {
    menu$message <- "ARE YOU SURE YOU WANT TO QUIT?"
    menu$message_confirm <- TRUE
    menu$message_action <- "quit"
    menu$dirty <- TRUE
  } else if (action == "endgame") {
    if (!identical(game$gamestate, GS_LEVEL)) {
      sound_play(menu$sound, "oof")
      return(invisible())
    }
    menu$message <- "END GAME?"
    menu$message_confirm <- TRUE
    menu$message_action <- "endgame"
    menu$dirty <- TRUE
  } else if (action == "sound") {
    menu_goto(menu, "sound")
  } else if (action == "messages") {
    game$show_messages <- !game$show_messages
    menu$dirty <- TRUE
  } else if (action == "detail") {
    game$detail_level <- if (game$detail_level == 0L) 1L else 0L
    game$need_view <- TRUE
    menu$dirty <- TRUE
  } else if (action == "scrnsize") {
    if (choice) {
      if (game$screen_size < 8L) game$screen_size <- game$screen_size + 1L
    } else if (game$screen_size > 0L) {
      game$screen_size <- game$screen_size - 1L
    }
    game$need_view <- TRUE
    menu$dirty <- TRUE
  } else if (action == "mousesens") {
    if (choice) {
      if (game$mouse_sensitivity < 9L) game$mouse_sensitivity <- game$mouse_sensitivity + 1L
    } else if (game$mouse_sensitivity > 0L) {
      game$mouse_sensitivity <- game$mouse_sensitivity - 1L
    }
    menu$dirty <- TRUE
  } else if (action == "sfxvol") {
    vol <- game$sound$sfx_volume
    sound_set_sfx_volume(game$sound, if (choice) vol + 1L else vol - 1L)
    menu$dirty <- TRUE
  } else if (action == "musvol") {
    vol <- game$sound$music_volume
    sound_set_music_volume(game$sound, if (choice) vol + 1L else vol - 1L)
    menu$dirty <- TRUE
  } else if (action == "episode") {
    if (!menu_has(menu, "E2M1") && choice != 0L) {
      menu$message <- "ONLY AVAILABLE IN THE REGISTERED VERSION."
      menu$message_confirm <- FALSE
      menu$message_action <- NULL
      menu_goto(menu, "read1")
      return(invisible())
    }
    menu$epi <- choice
    menu_goto(menu, "skill")
  } else if (action == "skill") {
    game$skill <- choice
    game$episode <- menu$epi + 1L
    game$mapn <- 1L
    game$pending_start <- TRUE
    message(sprintf("new game: skill %d episode %d map %d", game$skill, game$episode, game$mapn))
    menu_clear(menu)
  }
  invisible()
}

menu_open_help <- function(menu) {
  menu$active <- TRUE
  menu$message <- NULL
  menu$save_string_enter <- FALSE
  menu$menus$read1$last_on <- 0L
  menu$screen <- "read1"
  menu$item_on <- 0L
  menu$dirty <- TRUE
  sound_play(menu$sound, "swtchn")
}

menu_read_slots <- function(menu) {
  for (i in seq_len(6L)) {
    menu$save_strings[i] <- LOADSAVEEMPTY
    menu$save_slot_ok[i] <- FALSE
    menu$menus$load$items[[i]]$status <- 0L
    menu$menus$save$items[[i]]$status <- 1L
  }
}

menu_open_load <- function(menu) {
  menu_read_slots(menu)
  menu$save_string_enter <- FALSE
  menu$message <- NULL
  if (!menu$active) {
    menu$active <- TRUE
    menu$screen <- "load"
    menu$item_on <- menu$menus$load$last_on
  } else {
    menu_goto(menu, "load")
  }
  menu$dirty <- TRUE
  sound_play(menu$sound, "swtchn")
}

menu_open_save <- function(menu) {
  menu_read_slots(menu)
  menu$save_string_enter <- FALSE
  menu$message <- NULL
  if (!menu$active) {
    menu$active <- TRUE
    menu$screen <- "save"
    menu$item_on <- menu$menus$save$last_on
  } else {
    menu_goto(menu, "save")
  }
  menu$dirty <- TRUE
  sound_play(menu$sound, "swtchn")
}

menu_begin_save_name <- function(menu, slot) {
  menu$save_string_enter <- TRUE
  menu$save_slot <- slot
  menu$save_old_string <- menu$save_strings[slot + 1L]
  if (identical(menu$save_strings[slot + 1L], LOADSAVEEMPTY)) menu$save_strings[slot + 1L] <- ""
  menu$save_char_index <- nchar(menu$save_strings[slot + 1L])
  menu$dirty <- TRUE
}

menu_save_string_key <- function(menu, key) {
  slot <- menu$save_slot + 1L
  text <- menu$save_strings[slot]
  if (key == "backspace") {
    if (menu$save_char_index > 0L) {
      menu$save_char_index <- menu$save_char_index - 1L
      menu$save_strings[slot] <- substr(text, 1L, menu$save_char_index)
    }
    menu$dirty <- TRUE
    return(TRUE)
  }
  if (key == "escape") {
    menu$save_string_enter <- FALSE
    menu$save_strings[slot] <- menu$save_old_string
    menu$dirty <- TRUE
    return(TRUE)
  }
  if (key == "return") {
    menu$save_string_enter <- FALSE
    if (!nzchar(menu$save_strings[slot])) menu$save_strings[slot] <- menu$save_old_string
    else sound_play(menu$sound, "oof")
    menu$dirty <- TRUE
    return(TRUE)
  }
  ch <- toupper(key)
  if (nchar(ch) != 1L) return(TRUE)
  code <- utf8ToInt(ch)
  if (length(code) != 1L) return(TRUE)
  if (ch != " ") {
    idx <- code - HU_FONTSTART
    if (idx < 0L || idx >= HU_FONTSIZE) return(TRUE)
  }
  if (code >= 32L && code <= 127L && menu$save_char_index < SAVESTRINGSIZE - 1L) {
    menu$save_strings[slot] <- paste0(menu$save_strings[slot], ch)
    menu$save_char_index <- menu$save_char_index + 1L
    menu$dirty <- TRUE
  }
  TRUE
}

menu_patch_size <- function(patch) {
  c(i16_at(patch, 0), i16_at(patch, 2), i16_at(patch, 4), i16_at(patch, 6))
}

menu_string_width <- function(menu, text) {
  w <- 0L
  chars <- strsplit(toupper(text), "", fixed = TRUE)[[1]]
  for (ch in chars) {
    code <- utf8ToInt(ch)
    c <- code - HU_FONTSTART
    if (length(c) != 1L || c < 0L || c >= HU_FONTSIZE) {
      w <- w + 4L
      next
    }
    p <- menu_patch(menu, sprintf("STCFN%03d", code))
    if (is.null(p)) w <- w + 8L
    else w <- w + max(4L, menu_patch_size(p)[1])
  }
  w
}

menu_write_text <- function(menu, fb, x, y, text) {
  xx <- x
  chars <- strsplit(toupper(text), "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (ch == " ") {
      xx <- xx + 4L
      next
    }
    code <- utf8ToInt(ch)
    p <- menu_patch(menu, sprintf("STCFN%03d", code))
    if (!is.null(p)) {
      fb <- draw_patch(fb, xx, y, p)
      xx <- xx + max(4L, menu_patch_size(p)[1])
    } else {
      xx <- xx + 8L
    }
  }
  list(fb = fb, x = xx)
}

menu_draw_thermo <- function(menu, fb, x, y, width, dot) {
  left <- menu_patch(menu, "M_THERML")
  mid <- menu_patch(menu, "M_THERMM")
  right <- menu_patch(menu, "M_THERMR")
  knob <- menu_patch(menu, "M_THERMO")
  xx <- x
  if (!is.null(left)) fb <- draw_patch(fb, xx, y, left)
  xx <- xx + 8L
  if (width > 0L) {
    for (i in seq_len(width)) {
      if (!is.null(mid)) fb <- draw_patch(fb, xx, y, mid)
      xx <- xx + 8L
    }
  }
  if (!is.null(right)) fb <- draw_patch(fb, xx, y, right)
  if (!is.null(knob)) {
    pos <- max(0L, min(width - 1L, as.integer(dot)))
    fb <- draw_patch(fb, x + 8L + pos * 8L, y, knob)
  }
  fb
}

menu_draw_border <- function(menu, fb, x, y) {
  left <- menu_patch(menu, "M_LSLEFT")
  mid <- menu_patch(menu, "M_LSCNTR")
  right <- menu_patch(menu, "M_LSRGHT")
  if (!is.null(left)) fb <- draw_patch(fb, x - 8L, y + 7L, left)
  xx <- x
  for (i in seq_len(SAVESTRINGSIZE)) {
    if (!is.null(mid)) fb <- draw_patch(fb, xx, y + 7L, mid)
    xx <- xx + 8L
  }
  if (!is.null(right)) fb <- draw_patch(fb, xx, y + 7L, right)
  fb
}

menu_draw_message <- function(menu, fb) {
  text <- menu$message
  if (is.null(text)) return(fb)
  if (menu$message_confirm) text <- paste0(text, "  (Y/N)")
  x <- 10L
  y <- 80L
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (ch == " ") {
      x <- x + 8L
      next
    }
    code <- utf8ToInt(ch)
    p <- menu_patch(menu, sprintf("STCFN%03d", code))
    if (!is.null(p)) {
      fb <- draw_patch(fb, x, y, p)
      x <- x + max(4L, menu_patch_size(p)[1])
    } else {
      x <- x + 8L
    }
    if (x > 300L) {
      x <- 10L
      y <- y + 10L
    }
  }
  fb
}

menu_draw <- function(menu, fb) {
  if (!menu$active) return(fb)
  if (!is.null(menu$message)) return(menu_draw_message(menu, fb))
  def <- menu$menus[[menu$screen]]
  routine <- def$routine
  if (routine == "main") {
    p <- menu_patch(menu, "M_DOOM")
    if (!is.null(p)) fb <- draw_patch(fb, 94L, 2L, p)
  } else if (routine == "skill") {
    p <- menu_patch(menu, "M_NEWG")
    if (!is.null(p)) fb <- draw_patch(fb, 96L, 14L, p)
    p <- menu_patch(menu, "M_SKILL")
    if (!is.null(p)) fb <- draw_patch(fb, 54L, 38L, p)
  } else if (routine == "episode") {
    p <- menu_patch(menu, "M_EPISOD")
    if (!is.null(p)) fb <- draw_patch(fb, 54L, 38L, p)
  } else if (routine == "options") {
    p <- menu_patch(menu, "M_OPTTTL")
    if (!is.null(p)) fb <- draw_patch(fb, 108L, 15L, p)
    msg <- if (menu$game$show_messages) "M_MSGON" else "M_MSGOFF"
    p <- menu_patch(menu, msg)
    if (!is.null(p)) fb <- draw_patch(fb, def$x + 120L, def$y + LINEHEIGHT, p)
    det <- if (menu$game$detail_level == 0L) "M_GDHIGH" else "M_GDLOW"
    p <- menu_patch(menu, det)
    if (!is.null(p)) fb <- draw_patch(fb, def$x + 175L, def$y + LINEHEIGHT * 2L, p)
  } else if (routine == "sound") {
    p <- menu_patch(menu, "M_SVOL")
    if (!is.null(p)) fb <- draw_patch(fb, 60L, 38L, p)
  } else if (routine == "read1") {
    lump <- if (menu_has(menu, "HELP2")) "HELP2" else if (menu_has(menu, "HELP1")) "HELP1" else if (menu_has(menu, "HELP")) "HELP" else "CREDIT"
    p <- menu_patch(menu, lump)
    if (!is.null(p)) fb <- draw_patch(fb, 0L, 0L, p)
  } else if (routine == "read2") {
    p <- menu_patch(menu, "HELP1")
    if (is.null(p)) p <- menu_patch(menu, "CREDIT")
    if (!is.null(p)) fb <- draw_patch(fb, 0L, 0L, p)
  } else if (routine %in% c("load", "save")) {
    p <- menu_patch(menu, if (routine == "load") "M_LOADG" else "M_SAVEG")
    if (!is.null(p)) fb <- draw_patch(fb, 72L, 28L, p)
    for (i in seq_len(6L)) {
      y <- def$y + LINEHEIGHT * (i - 1L)
      fb <- menu_draw_border(menu, fb, def$x, y)
      wrote <- menu_write_text(menu, fb, def$x, y, menu$save_strings[i])
      fb <- wrote$fb
      if (menu$save_string_enter && (i - 1L) == menu$save_slot) {
        wrote <- menu_write_text(menu, fb, wrote$x, y, "_")
        fb <- wrote$fb
      }
    }
  }
  if (!routine %in% c("read1", "read2", "load", "save")) {
    y <- def$y
    for (item in def$items) {
      if (nzchar(item$name)) {
        p <- menu_patch(menu, item$name)
        if (!is.null(p)) fb <- draw_patch(fb, def$x, y, p)
      }
      y <- y + LINEHEIGHT
    }
  }
  if (routine == "options") {
    fb <- menu_draw_thermo(menu, fb, def$x, def$y + LINEHEIGHT * 4L, 9L, menu$game$screen_size)
    fb <- menu_draw_thermo(menu, fb, def$x, def$y + LINEHEIGHT * 6L, 10L, menu$game$mouse_sensitivity)
  } else if (routine == "sound") {
    fb <- menu_draw_thermo(menu, fb, def$x, def$y + LINEHEIGHT, 16L, menu$game$sound$sfx_volume)
    fb <- menu_draw_thermo(menu, fb, def$x, def$y + LINEHEIGHT * 3L, 16L, menu$game$sound$music_volume)
  }
  if (!routine %in% c("read1", "read2")) {
    skull <- if (menu$which_skull) "M_SKULL2" else "M_SKULL1"
    p <- menu_patch(menu, skull)
    if (!is.null(p)) {
      fb <- draw_patch(fb, def$x + SKULLXOFF, def$y - 5L + menu$item_on * LINEHEIGHT, p)
    }
  }
  fb
}
