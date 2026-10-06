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
# DOOM em R. Entrada: Rscript main.R [-iwad DOOM1.WAD] [-file pwad] [-fps]

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args_all, value = TRUE))
root <- if (length(file_arg)) dirname(normalizePath(file_arg[[1]], winslash = "/")) else normalizePath(".", winslash = "/")
setwd(root)
for (name in c(
  "defs.R", "compat.R", "wad.R", "v_video.R", "video.R", "sound.R", "tables.R",
  "sprites.R", "r_data.R", "info_spawn.R", "info_states.R", "p_setup.R", "collision.R", "player.R", "specials.R", "enemy.R",
  "render.R", "menu.R", "status.R", "wi.R", "wipe.R", "cheats.R", "am_map.R", "game.R"
)) {
  sys.source(file.path(root, "R", name), envir = .GlobalEnv)
}

quit(status = doom_main(), save = "no")
