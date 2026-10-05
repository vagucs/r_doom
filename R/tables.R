# finesine, finetangent e tantoangle. Gerados uma vez.

finesine <- numeric()
finetangent <- numeric()
tantoangle <- numeric()

init_tables <- function() {
  if (length(finesine)) return(invisible())
  nsin <- FINEANGLES %/% 4L * 5L
  ang <- (seq_len(nsin) - 1 + 0.5) * pi * 2 / FINEANGLES
  finesine <<- trunc(FRACUNIT * sin(ang))
  ntan <- FINEANGLES %/% 2L
  tang <- (seq_len(ntan) - 1 - FINEANGLES %/% 4 + 0.5) * pi * 2 / FINEANGLES
  tv <- FRACUNIT * tan(tang)
  tv[!is.finite(tv) & tang > 0] <- 2147483647
  tv[!is.finite(tv) & tang <= 0] <- -2147483647
  tv[tv > 2147483647] <- 2147483647
  tv[tv < -2147483647] <- -2147483647
  finetangent <<- trunc(tv)
  tantoangle <<- as_u32(trunc(atan((seq_len(SLOPERANGE + 1L) - 1L) / SLOPERANGE) / (pi * 2) * 4294967295))
  invisible()
}

fine_sin <- function(angle) {
  idx <- bitwAnd(as.integer(u32_shr(angle, ANGLETOFINESHIFT)), FINEMASK)
  finesine[idx + 1L]
}

fine_cos <- function(angle) {
  idx <- bitwAnd(as.integer(u32_shr(angle, ANGLETOFINESHIFT)) + FINEANGLES %/% 4L, FINEMASK)
  finesine[idx + 1L]
}

slope_div <- function(num, den) {
  num <- as.numeric(num)
  den <- as.numeric(den)
  if (den < 512) return(SLOPERANGE)
  ans <- trunc((num * 8) / trunc(den / 256))
  if (ans > SLOPERANGE) SLOPERANGE else ans
}
