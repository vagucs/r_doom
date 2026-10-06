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
# Sequencias do DOOM: iddqd, idkfa, idclip, idclev, idmus e as demais.

cheat_new <- function(action, sequence, param_chars = 0L) {
  e <- new.env(parent = emptyenv())
  e$action <- action
  e$sequence <- strsplit(sequence, "", fixed = TRUE)[[1]]
  e$param_chars <- as.integer(param_chars)
  e$chars_read <- 0L
  e$param_buf <- ""
  e
}

cheats_new <- function() {
  list(
    cheat_new("god", "iddqd"),
    cheat_new("kfa", "idkfa"),
    cheat_new("fa", "idfa"),
    cheat_new("noclip2", "idclip"),
    cheat_new("noclip", "idspispopd"),
    cheat_new("iddt", "iddt"),
    cheat_new("beholdv", "idbeholdv"),
    cheat_new("beholds", "idbeholds"),
    cheat_new("beholdi", "idbeholdi"),
    cheat_new("beholdr", "idbeholdr"),
    cheat_new("beholda", "idbeholda"),
    cheat_new("beholdl", "idbeholdl"),
    cheat_new("behold", "idbehold"),
    cheat_new("choppers", "idchoppers"),
    cheat_new("mypos", "idmypos"),
    cheat_new("clev", "idclev", 2L),
    cheat_new("mus", "idmus", 2L)
  )
}

cheat_feed <- function(cheat, ch) {
  seq <- cheat$sequence
  n <- length(seq)
  if (!n) return(NULL)
  if (cheat$chars_read < n) {
    if (identical(ch, seq[cheat$chars_read + 1L])) cheat$chars_read <- cheat$chars_read + 1L
    else cheat$chars_read <- if (identical(ch, seq[[1]])) 1L else 0L
    if (cheat$chars_read < n) return(NULL)
    if (cheat$param_chars <= 0L) {
      cheat$chars_read <- 0L
      return("")
    }
    return(NULL)
  }
  if (nchar(cheat$param_buf) < cheat$param_chars) cheat$param_buf <- paste0(cheat$param_buf, ch)
  if (nchar(cheat$param_buf) < cheat$param_chars) return(NULL)
  buf <- cheat$param_buf
  cheat$chars_read <- 0L
  cheat$param_buf <- ""
  buf
}

cheat_god <- function(player) {
  player$cheats <- bitwXor(as.integer(player$cheats), CF_GODMODE)
  if (bitwAnd(player$cheats, CF_GODMODE) != 0L) {
    player$health <- 100
    if (!is.null(player$mo)) player$mo$health <- 100
    set_player_message(player, "MODO DEUS LIGADO")
  } else {
    set_player_message(player, "MODO DEUS DESLIGADO")
  }
}

cheat_ammo <- function(player, keys) {
  player$armorpoints <- 200
  player$armortype <- 2
  player$weaponowned[] <- TRUE
  player$maxammo <- c(200, 50, 300, 50)
  player$ammo <- player$maxammo
  if (keys) player$cards[] <- TRUE
  set_player_message(player, if (keys) "MUNICAO E CHAVES" else "MUNICAO ADICIONADA")
}

cheat_noclip <- function(player) {
  player$cheats <- bitwXor(as.integer(player$cheats), CF_NOCLIP)
  on <- bitwAnd(player$cheats, CF_NOCLIP) != 0L
  if (!is.null(player$mo)) {
    flags <- as.integer(player$mo$flags)
    player$mo$flags <- if (on) bitwOr(flags, MF_NOCLIP) else bitwAnd(flags, bitwNot(MF_NOCLIP))
  }
  set_player_message(player, if (on) "ATRAVESSAR LIGADO" else "ATRAVESSAR DESLIGADO")
}

cheat_behold <- function(player, pw) {
  if (pw < 0L) return(invisible())
  if (!player$powers[pw + 1L]) {
    give_power(player, pw)
    if (pw == PW_STRENGTH && player$readyweapon != WP_FIST) player$pendingweapon <- WP_FIST
  } else if (pw == PW_STRENGTH) {
    player$powers[pw + 1L] <- 0
  } else {
    player$powers[pw + 1L] <- 1
  }
  set_player_message(player, "PODER ALTERNADO")
}

cheat_clev <- function(game, param) {
  if (nchar(param) < 2L || !grepl("^[0-9]{2}$", param)) return(invisible())
  a <- as.integer(substr(param, 1L, 1L))
  b <- as.integer(substr(param, 2L, 2L))
  commercial <- wad_check_num(game$wad, "MAP01") >= 0L
  if (commercial) {
    game$episode <- 1L
    game$mapn <- a * 10L + b
  } else {
    game$episode <- a
    game$mapn <- b
  }
  if (game$episode < 1L || game$mapn < 1L) return(invisible())
  if (wad_check_num(game$wad, level_lump_name(game, game$mapn)) < 0L) return(invisible())
  start_level(game, FALSE)
  set_player_message(game$player, "TROCANDO A FASE")
}

cheat_mus <- function(game, param) {
  player <- game$player
  if (nchar(param) < 2L || !grepl("^[0-9]{2}$", param)) return(invisible())
  a <- as.integer(substr(param, 1L, 1L))
  b <- as.integer(substr(param, 2L, 2L))
  commercial <- wad_check_num(game$wad, "MAP01") >= 0L
  if (commercial) {
    mapn <- a * 10L + b
    if (mapn < 1L || mapn > length(DOOM2_MUSIC)) {
      set_player_message(player, "SELECAO IMPOSSIVEL")
      return(invisible())
    }
    name <- DOOM2_MUSIC[mapn]
  } else {
    if (a < 1L || b < 1L || b > 9L) {
      set_player_message(player, "SELECAO IMPOSSIVEL")
      return(invisible())
    }
    name <- sprintf("e%dm%d", a, b)
  }
  lump <- paste0("D_", toupper(substr(name, 1L, 6L)))
  if (wad_check_num(game$wad, lump) < 0L) {
    set_player_message(player, "SELECAO IMPOSSIVEL")
    return(invisible())
  }
  sound_change_music(game$sound, name, TRUE)
  set_player_message(player, "MUSICA TROCADA")
}

cheat_mypos <- function(player) {
  mo <- player$mo
  if (is.null(mo)) return(invisible())
  ang <- as.integer((as.numeric(mo$angle) %% 4294967296) / 4294967296 * 360)
  set_player_message(player, sprintf(
    "X %d Y %d A %d",
    as.integer(mo$x / FRACUNIT), as.integer(mo$y / FRACUNIT), ang
  ))
}

do_cheat <- function(game, action, param) {
  player <- game$player
  if (is.null(player)) return(invisible())
  if (action == "god") cheat_god(player)
  else if (action == "kfa") cheat_ammo(player, TRUE)
  else if (action == "fa") cheat_ammo(player, FALSE)
  else if (action == "noclip" || action == "noclip2") cheat_noclip(player)
  else if (action == "behold") set_player_message(player, "INVIN VISIS RAD ALLMAP LITE AMP")
  else if (startsWith(action, "behold") && nchar(action) == 7L) {
    letter <- substr(action, 7L, 7L)
    cheat_behold(player, as.integer(regexpr(letter, "vsiral", fixed = TRUE) - 1L))
  } else if (action == "choppers") {
    player$weaponowned[WP_CHAINSAW + 1L] <- TRUE
    player$pendingweapon <- WP_CHAINSAW
    player$powers[PW_INVULNERABILITY + 1L] <- 1
    set_player_message(player, "MOTOSSERRA")
  } else if (action == "iddt") {
    if (am_cycle(game$am)) game$menu$dirty <- TRUE
  } else if (action == "mypos") cheat_mypos(player)
  else if (action == "clev") cheat_clev(game, param)
  else if (action == "mus") cheat_mus(game, param)
  if (!is.null(game$menu)) game$menu$dirty <- TRUE
  invisible()
}

feed_cheats <- function(game, key) {
  if (!identical(game$gamestate, GS_LEVEL) || is.null(game$player) || is.null(game$cheats)) return(invisible())
  ch <- tolower(key)
  if (nchar(ch) != 1L || !grepl("^[a-z0-9]$", ch)) return(invisible())
  nightmare <- identical(as.integer(game$skill), SK_NIGHTMARE)
  for (cheat in game$cheats) {
    param <- cheat_feed(cheat, ch)
    if (is.null(param)) next
    if (nightmare && cheat$action != "clev" && cheat$action != "iddt") next
    do_cheat(game, cheat$action, param)
  }
  invisible()
}
