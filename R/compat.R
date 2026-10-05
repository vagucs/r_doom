# Aritmetica de 32 bits em double. O inteiro do R para em 2^31-1,
# e os angulos do Doom passam disso.

MASK32 <- 4294967296

as_u32 <- function(n) {
  as.numeric(n) %% MASK32
}

as_i32 <- function(n) {
  n <- as.numeric(n) %% MASK32
  n - (n >= 2147483648) * MASK32
}

u32_and <- function(a, b) {
  a <- as_u32(a)
  b <- as_u32(b)
  ah <- a %/% 65536
  al <- a %% 65536
  bh <- b %/% 65536
  bl <- b %% 65536
  bitwAnd(as.integer(ah), as.integer(bh)) * 65536 +
    bitwAnd(as.integer(al), as.integer(bl))
}

u32_shr <- function(n, bits) {
  n <- as_u32(n)
  bits <- as.integer(bits)[1]
  if (is.na(bits) || bits <= 0L) return(n)
  if (bits >= 32L) return(rep(0, length(n)))
  floor(n / 2^bits)
}

shar <- function(n, bits) {
  n <- as_i32(n)
  bits <- as.integer(bits)[1]
  if (is.na(bits) || bits <= 0L) return(n)
  if (bits >= 31L) return(ifelse(n < 0, -1, 0))
  floor(n / 2^bits)
}

abs_fixed <- function(n) abs(as_i32(n))

.recycle2 <- function(a, b) {
  n <- max(length(a), length(b))
  if (length(a) == 1L && n > 1L) a <- rep(a, n)
  if (length(b) == 1L && n > 1L) b <- rep(b, n)
  list(a, b)
}

fixed_mul <- function(a, b) {
  pair <- .recycle2(as_i32(a), as_i32(b))
  a <- pair[[1]]
  b <- pair[[2]]
  ah <- trunc(a / 65536)
  al <- a - ah * 65536
  bh <- trunc(b / 65536)
  bl <- b - bh * 65536
  as_i32(ah * bh * 65536 + ah * bl + al * bh + trunc(al * bl / 65536))
}

fixed_div <- function(a, b) {
  pair <- .recycle2(as_i32(a), as_i32(b))
  a <- pair[[1]]
  b <- pair[[2]]
  out <- numeric(length(a))
  zero <- b == 0
  out[zero] <- ifelse(a[zero] >= 0, 2147483647, -2147483648)
  ok <- !zero
  if (!any(ok)) return(as_i32(out))
  aa <- a[ok]
  bb <- b[ok]
  overflow <- trunc(abs(aa) / 16384) >= abs(bb)
  res <- numeric(length(aa))
  neg <- (aa < 0) != (bb < 0)
  res[overflow] <- ifelse(neg[overflow], -2147483648, 2147483647)
  keep <- !overflow
  res[keep] <- trunc(aa[keep] * 65536 / bb[keep])
  out[ok] <- res
  as_i32(out)
}
