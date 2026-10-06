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
# Efeitos DS* na fila SDL e musica MUS convertida para MIDI (MCI).

DOOM2_MUSIC <- c(
  "runnin", "stalks", "countd", "betwee", "doom", "the_da", "shawn", "ddtblu",
  "in_cit", "dead", "stlks2", "theda2", "doom2", "ddtbl2", "runni2", "dead2",
  "stlks3", "romero", "shawn2", "messag", "count2", "ddtbl3", "ampie", "theda3",
  "adrian", "messg2", "romer2", "tense", "shawn3", "openin", "evil", "ultima"
)

sound_new <- function() {
  e <- new.env(parent = emptyenv())
  e$wad <- NULL
  e$cache <- new.env(parent = emptyenv())
  e$sfx_volume <- 8L
  e$music_volume <- 8L
  e$last <- ""
  e$music_name <- ""
  e$music_path <- NULL
  e$music_loop <- FALSE
  e$music_on <- FALSE
  e$music_check <- 0
  e
}

sound_init <- function(sound, wad) {
  sound$wad <- wad
  invisible(sound)
}

sound_mci <- function(cmd) {
  if (!isTRUE(sdl_state$loaded)) return(1L)
  .Call("rdoom_mci", cmd, FALSE, PACKAGE = "rdoom_sdl")
}

sound_mci_text <- function(cmd) {
  if (!isTRUE(sdl_state$loaded)) return("")
  .Call("rdoom_mci", cmd, TRUE, PACKAGE = "rdoom_sdl")
}

decode_ds <- function(data) {
  if (length(data) < 8L) return(NULL)
  b <- as.integer(data)
  if (b[1] != 3L || b[2] != 0L) return(NULL)
  rate <- b[3] + b[4] * 256L
  len <- b[5] + b[6] * 256L + b[7] * 65536L + b[8] * 16777216
  if (is.na(len) || rate <= 0L || len > length(b) - 8L || len <= 48L) return(NULL)
  len <- len - 32L
  end <- 16L + len
  if (end > length(b)) return(NULL)
  samples <- b[seq.int(17L, end)]
  if (rate != 11025L) {
    out_n <- max(1L, as.integer(length(samples) * 11025 / rate))
    src <- pmin(length(samples), ((seq_len(out_n) - 1L) * length(samples)) %/% out_n + 1L)
    samples <- samples[src]
  }
  (samples - 128L) * 256L
}

sound_pcm <- function(sound, name) {
  key <- tolower(name)
  if (exists(key, sound$cache, inherits = FALSE)) return(sound$cache[[key]])
  if (is.null(sound$wad) || !nzchar(key)) {
    sound$cache[[key]] <- NULL
    return(NULL)
  }
  lump <- paste0("DS", toupper(substr(key, 1L, 6L)))
  n <- wad_check_num(sound$wad, lump)
  pcm <- if (n < 0L) NULL else decode_ds(wad_cache_num(sound$wad, n))
  sound$cache[[key]] <- pcm
  pcm
}

sound_play <- function(sound, name) {
  if (is.null(sound) || !nzchar(name)) return(invisible(NULL))
  sound$last <- name
  if (sound$sfx_volume <= 0L || !isTRUE(sdl_state$loaded)) return(invisible(name))
  pcm <- sound_pcm(sound, name)
  if (!is.null(pcm) && length(pcm)) {
    .Call("rdoom_play_sfx", as.integer(pcm), as.integer(sound$sfx_volume), PACKAGE = "rdoom_sdl")
  }
  invisible(name)
}

sound_pump <- function() {
  if (!isTRUE(sdl_state$loaded)) return(invisible(NULL))
  .Call("rdoom_pump_audio", PACKAGE = "rdoom_sdl")
  invisible(NULL)
}

sound_set_sfx_volume <- function(sound, volume) {
  sound$sfx_volume <- max(0L, min(15L, as.integer(volume)))
}

sound_set_music_volume <- function(sound, volume) {
  sound$music_volume <- max(0L, min(15L, as.integer(volume)))
  if (isTRUE(sound$music_on)) {
    sound_mci(sprintf("setaudio doommus volume to %d", as.integer(sound$music_volume * 1000 / 15)))
  }
  invisible(sound$music_volume)
}

sound_stop_music <- function(sound) {
  if (isTRUE(sound$music_on)) sound_mci("close doommus")
  sound$music_on <- FALSE
  sound$music_name <- ""
  if (!is.null(sound$music_path) && file.exists(sound$music_path)) unlink(sound$music_path)
  sound$music_path <- NULL
  invisible(NULL)
}

sound_change_music <- function(sound, name, looping = TRUE) {
  if (is.null(sound$wad) || !nzchar(name)) return(invisible(NULL))
  if (identical(tolower(name), sound$music_name)) return(invisible(NULL))
  lump <- paste0("D_", toupper(substr(name, 1L, 6L)))
  n <- wad_check_num(sound$wad, lump)
  if (n < 0L) return(invisible(NULL))
  midi <- mus2mid(wad_cache_num(sound$wad, n))
  if (is.null(midi) || !length(midi)) return(invisible(NULL))
  sound_stop_music(sound)
  path <- tempfile(pattern = "doommus", fileext = ".mid")
  writeBin(midi, path)
  quoted <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  sound_mci("close doommus")
  opened <- sound_mci(sprintf('open "%s" type sequencer alias doommus', quoted)) == 0L ||
    sound_mci(sprintf('open "%s" alias doommus', quoted)) == 0L
  if (opened && sound_mci("play doommus from 0") == 0L) {
    sound$music_on <- TRUE
    sound$music_name <- tolower(name)
    sound$music_path <- path
    sound$music_loop <- isTRUE(looping)
    sound_set_music_volume(sound, sound$music_volume)
  } else if (file.exists(path)) {
    unlink(path)
  }
  invisible(NULL)
}

sound_play_title <- function(sound) {
  if (is.null(sound$wad)) return(invisible(NULL))
  if (wad_check_num(sound$wad, "MAP01") >= 0L) sound_change_music(sound, "dm2ttl", FALSE)
  else if (wad_check_num(sound$wad, "D_INTROA") >= 0L) sound_change_music(sound, "introa", FALSE)
  else sound_change_music(sound, "intro", FALSE)
}

sound_play_level <- function(sound, episode, mapn) {
  if (is.null(sound$wad)) return(invisible(NULL))
  name <- if (wad_check_num(sound$wad, "MAP01") >= 0L) {
    DOOM2_MUSIC[((max(1L, as.integer(mapn)) - 1L) %% length(DOOM2_MUSIC)) + 1L]
  } else {
    sprintf("e%dm%d", as.integer(episode), as.integer(mapn))
  }
  sound_change_music(sound, name, TRUE)
}

sound_update <- function(sound) {
  if (is.null(sound) || !isTRUE(sound$music_on) || !isTRUE(sound$music_loop)) return(invisible(NULL))
  now <- proc.time()[["elapsed"]]
  if (now - sound$music_check < 0.25) return(invisible(NULL))
  sound$music_check <- now
  mode <- tolower(sound_mci_text("status doommus mode"))
  if (nzchar(mode) && mode != "playing") sound_mci("play doommus from 0")
  invisible(NULL)
}

mus_write_time <- function(st, time) {
  time <- as.numeric(time)
  if (time < 0) time <- 0
  buffer <- time %% 128
  t <- time %/% 128
  while (t != 0) {
    buffer <- buffer * 256 + (t %% 128) + 128
    t <- t %/% 128
  }
  repeat {
    byte <- as.integer(buffer %% 256)
    st$body[length(st$body) + 1L] <- byte
    st$tracksize <- st$tracksize + 1L
    if (bitwAnd(byte, 128L) != 0L) buffer <- buffer %/% 256 else break
  }
  st$queued <- 0
}

mus_write <- function(st, data) {
  mus_write_time(st, st$queued)
  st$body <- c(st$body, as.integer(data))
  st$tracksize <- st$tracksize + length(data)
}

mus_alloc_channel <- function(st) {
  result <- max(st$channel_map) + 1L
  if (result == 9L) result <- 10L
  result
}

mus_channel <- function(st, mus_channel) {
  if (mus_channel == 15L) return(9L)
  mapped <- st$channel_map[mus_channel + 1L]
  if (mapped == -1L) {
    mapped <- mus_alloc_channel(st)
    st$channel_map[mus_channel + 1L] <- mapped
    mus_write(st, c(bitwOr(176L, mapped), 123L, 0L))
  }
  mapped
}

mus2mid <- function(mus) {
  b <- as.integer(mus)
  if (length(b) >= 4L && identical(b[1:4], c(77L, 84L, 104L, 100L))) return(as.raw(b))
  if (length(b) < 16L || !identical(b[1:4], c(77L, 85L, 83L, 26L))) return(NULL)
  scorestart <- b[7] + b[8] * 256L
  if (scorestart < 1L || scorestart > length(b)) return(NULL)
  pos <- scorestart + 1L
  st <- new.env(parent = emptyenv())
  st$body <- integer()
  st$queued <- 0
  st$tracksize <- 0L
  st$velocities <- rep(127L, 16L)
  st$channel_map <- rep(-1L, 16L)
  ctrl_map <- c(0L, 32L, 1L, 7L, 10L, 11L, 91L, 93L, 64L, 67L, 120L, 123L, 126L, 127L, 121L)
  read_u8 <- function() {
    if (pos > length(b)) return(NULL)
    v <- b[pos]
    pos <<- pos + 1L
    v
  }
  hits <- FALSE
  while (!hits) {
    repeat {
      descriptor <- read_u8()
      if (is.null(descriptor)) return(NULL)
      channel <- mus_channel(st, bitwAnd(descriptor, 15L))
      event <- bitwAnd(descriptor, 112L)
      if (event == 0L) {
        key <- read_u8()
        if (is.null(key)) return(NULL)
        mus_write(st, c(bitwOr(128L, channel), bitwAnd(key, 127L), 0L))
      } else if (event == 16L) {
        key <- read_u8()
        if (is.null(key)) return(NULL)
        if (bitwAnd(key, 128L) != 0L) {
          vel <- read_u8()
          if (is.null(vel)) return(NULL)
          st$velocities[channel + 1L] <- bitwAnd(vel, 127L)
        }
        mus_write(st, c(bitwOr(144L, channel), bitwAnd(key, 127L), st$velocities[channel + 1L]))
      } else if (event == 32L) {
        key <- read_u8()
        if (is.null(key)) return(NULL)
        wheel <- key * 64L
        mus_write(st, c(bitwOr(224L, channel), bitwAnd(wheel, 127L), bitwAnd(bitwShiftR(wheel, 7L), 127L)))
      } else if (event == 48L) {
        ctrl <- read_u8()
        if (is.null(ctrl) || ctrl < 10L || ctrl > 14L) return(NULL)
        mus_write(st, c(bitwOr(176L, channel), ctrl_map[ctrl + 1L], 0L))
      } else if (event == 64L) {
        ctrl <- read_u8()
        val <- read_u8()
        if (is.null(ctrl) || is.null(val)) return(NULL)
        if (ctrl == 0L) {
          mus_write(st, c(bitwOr(192L, channel), bitwAnd(val, 127L)))
        } else {
          if (ctrl < 1L || ctrl > 9L) return(NULL)
          working <- if (bitwAnd(val, 128L) != 0L) 127L else bitwAnd(val, 127L)
          mus_write(st, c(bitwOr(176L, channel), ctrl_map[ctrl + 1L], working))
        }
      } else if (event == 96L) {
        hits <- TRUE
      } else {
        return(NULL)
      }
      if (hits || bitwAnd(descriptor, 128L) != 0L) break
    }
    if (!hits) {
      timedelay <- 0
      repeat {
        working <- read_u8()
        if (is.null(working)) return(NULL)
        timedelay <- timedelay * 128 + bitwAnd(working, 127L)
        if (bitwAnd(working, 128L) == 0L) break
      }
      st$queued <- st$queued + timedelay
    }
  }
  mus_write_time(st, st$queued)
  st$body <- c(st$body, 255L, 47L, 0L)
  st$tracksize <- st$tracksize + 3L
  header <- c(
    77L, 84L, 104L, 100L, 0L, 0L, 0L, 6L, 0L, 0L, 0L, 1L, 0L, 70L,
    77L, 84L, 114L, 107L, 0L, 0L, 0L, 0L
  )
  size <- st$tracksize
  header[19] <- bitwAnd(bitwShiftR(size, 24L), 255L)
  header[20] <- bitwAnd(bitwShiftR(size, 16L), 255L)
  header[21] <- bitwAnd(bitwShiftR(size, 8L), 255L)
  header[22] <- bitwAnd(size, 255L)
  as.raw(c(header, st$body))
}
