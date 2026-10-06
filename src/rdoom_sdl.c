/* DOOM generic portado do python_doom para R com SDL2.
 *
 * Por Wagner Nunes da Silva
 *
 * vagucs@bol.com.br
 * vagucs@vagucs.com.br
 * vagucs@gmail.com
 *
 * www.vagucs.com.br
 *
 * Ponte R -> SDL2. Janela, paleta, textura, teclado e fila de audio.
 * Nada de BSP, coluna, span, thinker ou qualquer logica do DOOM.
 */

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <SDL.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static SDL_Window *window = NULL;
static SDL_Renderer *renderer = NULL;
static SDL_Texture *texture = NULL;
static SDL_PixelFormat *pixfmt = NULL;
static uint32_t *pixels = NULL;
static uint32_t palette32[256];
static int tex_w = 0;
static int tex_h = 0;
static int sdl_up = 0;
static int held_ctrl = 0;
static int held_shift = 0;
static int held_alt = 0;
static SDL_AudioDeviceID audio_dev = 0;
static int16_t *mix = NULL;
static int mix_len = 0;
static int mix_cap = 0;
static int crt_on = 0;
static int crt_dw = 0;
static int crt_dh = 0;
static uint16_t *crt_map = NULL;
static uint8_t *crt_gain = NULL;
static uint8_t *crt_blur = NULL;
static uint32_t *crt_pixels = NULL;
static SDL_Texture *crt_texture = NULL;
static int crt_mask[9];
static uint8_t pal_r[256];
static uint8_t pal_g[256];
static uint8_t pal_b[256];

static void crt_free(void) {
  if (crt_texture) SDL_DestroyTexture(crt_texture);
  free(crt_map);
  free(crt_gain);
  free(crt_pixels);
  free(crt_blur);
  crt_texture = NULL;
  crt_map = NULL;
  crt_gain = NULL;
  crt_pixels = NULL;
  crt_blur = NULL;
  crt_dw = 0;
  crt_dh = 0;
}

extern unsigned long __stdcall mciSendStringA(const char *cmd, char *ret, unsigned int retlen, void *wnd);

static void audio_close(void) {
  if (audio_dev) {
    SDL_CloseAudioDevice(audio_dev);
    audio_dev = 0;
  }
  free(mix);
  mix = NULL;
  mix_len = 0;
  mix_cap = 0;
}

static void audio_open(void) {
  SDL_AudioSpec want;
  SDL_AudioSpec have;
  audio_close();
  SDL_InitSubSystem(SDL_INIT_AUDIO);
  SDL_zero(want);
  want.freq = 11025;
  want.format = AUDIO_S16LSB;
  want.channels = 1;
  want.samples = 512;
  audio_dev = SDL_OpenAudioDevice(NULL, 0, &want, &have, 0);
  if (!audio_dev) return;
  if (have.freq != 11025 || have.format != AUDIO_S16LSB || have.channels != 1) {
    SDL_CloseAudioDevice(audio_dev);
    audio_dev = 0;
    return;
  }
  SDL_PauseAudioDevice(audio_dev, 0);
}

static void video_teardown(void) {
  audio_close();
  crt_free();
  if (texture) SDL_DestroyTexture(texture);
  if (renderer) SDL_DestroyRenderer(renderer);
  if (window) SDL_DestroyWindow(window);
  if (pixfmt) SDL_FreeFormat(pixfmt);
  free(pixels);
  texture = NULL;
  renderer = NULL;
  window = NULL;
  pixfmt = NULL;
  pixels = NULL;
  tex_w = 0;
  tex_h = 0;
  held_ctrl = 0;
  held_shift = 0;
  held_alt = 0;
  if (sdl_up) {
    SDL_Quit();
    sdl_up = 0;
  }
}

static int name_of(SDL_Keycode sym, char *out, int n) {
  const char *s = NULL;
  if (n < 2) return 0;
  if (sym >= SDLK_F1 && sym <= SDLK_F12) {
    snprintf(out, (size_t)n, "f%d", (int)(sym - SDLK_F1) + 1);
    return 1;
  }
  switch (sym) {
  case SDLK_TAB: s = "tab"; break;
  case SDLK_ESCAPE: s = "escape"; break;
  case SDLK_RETURN:
  case SDLK_KP_ENTER: s = "return"; break;
  case SDLK_BACKSPACE: s = "backspace"; break;
  case SDLK_SPACE: s = "space"; break;
  case SDLK_UP: s = "up"; break;
  case SDLK_DOWN: s = "down"; break;
  case SDLK_LEFT: s = "left"; break;
  case SDLK_RIGHT: s = "right"; break;
  case SDLK_COMMA: s = "comma"; break;
  case SDLK_PERIOD: s = "period"; break;
  case SDLK_MINUS:
  case SDLK_KP_MINUS: s = "minus"; break;
  case SDLK_EQUALS:
  case SDLK_KP_PLUS: s = "equals"; break;
  default:
    if ((sym >= SDLK_a && sym <= SDLK_z) || (sym >= SDLK_0 && sym <= SDLK_9)) {
      out[0] = (char)sym;
      out[1] = '\0';
      return 1;
    }
    return 0;
  }
  snprintf(out, (size_t)n, "%s", s);
  return 1;
}

static void note_mod(int *held, int bit, int down, int repeat,
                     char types[][8], char keys[][16], int *n, int maxn, const char *name) {
  if (down) {
    int was = *held != 0;
    *held |= bit;
    if ((!was || repeat) && *n < maxn) {
      snprintf(types[*n], 8, "down");
      snprintf(keys[*n], 16, "%s", name);
      (*n)++;
    }
  } else {
    *held &= ~bit;
    if (*held == 0 && *n < maxn) {
      snprintf(types[*n], 8, "up");
      snprintf(keys[*n], 16, "%s", name);
      (*n)++;
    }
  }
}

SEXP rdoom_video_init(SEXP width, SEXP height, SEXP scale) {
  int w = Rf_asInteger(width);
  int h = Rf_asInteger(height);
  int s = Rf_asInteger(scale);
  if (w < 1 || h < 1) Rf_error("tamanho de textura invalido");
  if (s < 1) s = 1;
  video_teardown();
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "0");
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO) != 0 && SDL_Init(SDL_INIT_VIDEO) != 0) {
    Rf_error("SDL_Init: %s", SDL_GetError());
  }
  sdl_up = 1;
  window = SDL_CreateWindow("DOOM (R)", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                            w * s, h * s, SDL_WINDOW_SHOWN);
  if (!window) {
    const char *err = SDL_GetError();
    video_teardown();
    Rf_error("SDL_CreateWindow: %s", err);
  }
  renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (!renderer) renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_SOFTWARE);
  if (!renderer) {
    const char *err = SDL_GetError();
    video_teardown();
    Rf_error("SDL_CreateRenderer: %s", err);
  }
  texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, w, h);
  if (!texture) {
    const char *err = SDL_GetError();
    video_teardown();
    Rf_error("SDL_CreateTexture: %s", err);
  }
  pixfmt = SDL_AllocFormat(SDL_PIXELFORMAT_ARGB8888);
  if (!pixfmt) {
    const char *err = SDL_GetError();
    video_teardown();
    Rf_error("SDL_AllocFormat: %s", err);
  }
  pixels = (uint32_t *)malloc((size_t)w * (size_t)h * sizeof(uint32_t));
  if (!pixels) {
    video_teardown();
    Rf_error("sem memoria para o quadro de %d x %d", w, h);
  }
  tex_w = w;
  tex_h = h;
  for (int i = 0; i < 256; i++) palette32[i] = SDL_MapRGBA(pixfmt, (Uint8)i, (Uint8)i, (Uint8)i, 255);
  audio_open();
  return Rf_ScalarLogical(1);
}

SEXP rdoom_play_sfx(SEXP samples, SEXP volume) {
  if (!audio_dev || TYPEOF(samples) != INTSXP) return R_NilValue;
  int n = Rf_length(samples);
  int vol = Rf_asInteger(volume);
  if (n < 1 || vol <= 0) return R_NilValue;
  if (vol > 15) vol = 15;
  if (n > 11025 * 8) n = 11025 * 8;
  if (mix_cap < n) {
    int cap = n;
    int16_t *grown = (int16_t *)realloc(mix, (size_t)cap * sizeof(int16_t));
    if (!grown) return R_NilValue;
    if (cap > mix_cap) memset(grown + mix_cap, 0, (size_t)(cap - mix_cap) * sizeof(int16_t));
    mix = grown;
    mix_cap = cap;
  }
  if (mix_len < n) {
    memset(mix + mix_len, 0, (size_t)(n - mix_len) * sizeof(int16_t));
    mix_len = n;
  }
  const int *src = INTEGER(samples);
  for (int i = 0; i < n; i++) {
    int s = src[i];
    if (s == NA_INTEGER) continue;
    int mixed = mix[i] + (s * vol) / 15;
    if (mixed > 32767) mixed = 32767;
    if (mixed < -32768) mixed = -32768;
    mix[i] = (int16_t)mixed;
  }
  return R_NilValue;
}

SEXP rdoom_pump_audio(void) {
  if (!audio_dev || mix_len <= 0) return R_NilValue;
  if (SDL_GetQueuedAudioSize(audio_dev) > 4096) return R_NilValue;
  int n = mix_len;
  if (n > 2048) n = 2048;
  SDL_QueueAudio(audio_dev, mix, (Uint32)n * 2);
  mix_len -= n;
  if (mix_len > 0) memmove(mix, mix + n, (size_t)mix_len * sizeof(int16_t));
  return R_NilValue;
}

SEXP rdoom_mci(SEXP cmd, SEXP want_text) {
  if (TYPEOF(cmd) != STRSXP || Rf_length(cmd) < 1) Rf_error("comando MCI vazio");
  const char *text = CHAR(STRING_ELT(cmd, 0));
  char buf[128];
  buf[0] = '\0';
  unsigned long err = mciSendStringA(text, buf, (unsigned int)sizeof(buf), NULL);
  if (Rf_asLogical(want_text)) return Rf_mkString(buf);
  return Rf_ScalarInteger((int)err);
}

SEXP rdoom_video_set_palette(SEXP pal) {
  if (!pixfmt) Rf_error("SDL ainda nao foi iniciado");
  if (TYPEOF(pal) != RAWSXP || Rf_length(pal) < 768) Rf_error("paleta precisa de 768 bytes RGB");
  const unsigned char *p = RAW(pal);
  for (int i = 0; i < 256; i++) {
    pal_r[i] = p[i * 3];
    pal_g[i] = p[i * 3 + 1];
    pal_b[i] = p[i * 3 + 2];
    palette32[i] = SDL_MapRGBA(pixfmt, pal_r[i], pal_g[i], pal_b[i], 255);
  }
  return R_NilValue;
}

static int crt_build(int dw, int dh) {
  crt_free();
  crt_map = (uint16_t *)malloc((size_t)dw * (size_t)dh * sizeof(uint16_t));
  crt_gain = (uint8_t *)malloc((size_t)dw * (size_t)dh);
  crt_pixels = (uint32_t *)malloc((size_t)dw * (size_t)dh * sizeof(uint32_t));
  crt_blur = (uint8_t *)malloc((size_t)tex_w * (size_t)tex_h * 3);
  crt_texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, dw, dh);
  if (!crt_map || !crt_gain || !crt_pixels || !crt_blur || !crt_texture) {
    crt_free();
    return 0;
  }
  double sl = (double)dh / (double)tex_h - 1.0;
  if (sl < 0) sl = 0;
  if (sl > 1) sl = 1;
  sl *= 0.45;
  for (int y = 0; y < dh; y++) {
    double ny = (2.0 * y) / dh - 1.0;
    double ny2 = (ny * ny) / 32.0;
    for (int x = 0; x < dw; x++) {
      double nx = (2.0 * (x + 0.5)) / dw - 1.0;
      double u = nx * (1.0 + ny2);
      double v = ny * (1.0 + (nx * nx) / 24.0);
      int p = y * dw + x;
      if (u <= -1.0 || u >= 1.0 || v <= -1.0 || v >= 1.0) {
        crt_map[p] = 0xffff;
        continue;
      }
      double sx = (u + 1.0) * 0.5 * tex_w;
      double sy = (v + 1.0) * 0.5 * tex_h;
      int ix = (int)sx;
      int iy = (int)sy;
      if (ix < 0) ix = 0;
      if (iy < 0) iy = 0;
      if (ix > tex_w - 1) ix = tex_w - 1;
      if (iy > tex_h - 1) iy = tex_h - 1;
      double d = sy - iy - 0.5;
      double uu = (u + 1.0) * 0.5;
      double vv = (v + 1.0) * 0.5;
      double g = (1.0 - sl * 4.0 * d * d) * pow(16.0 * uu * vv * (1.0 - uu) * (1.0 - vv), 0.12) * 255.0;
      if (g < 0 || g != g) g = 0;
      if (g > 255) g = 255;
      crt_map[p] = (uint16_t)(iy * tex_w + ix);
      crt_gain[p] = (uint8_t)(g + 0.5);
    }
  }
  int wide = dw >= 2 * tex_w;
  double off = wide ? 0.7 : 1.0;
  double boost = wide ? 1.4 : 1.15;
  for (int m = 0; m < 3; m++) {
    for (int c = 0; c < 3; c++) {
      crt_mask[m * 3 + c] = (int)(256.0 * boost * (m == c ? 1.0 : off));
    }
  }
  crt_dw = dw;
  crt_dh = dh;
  return 1;
}

static void present_crt(const unsigned char *src) {
  int win_w = tex_w;
  int win_h = tex_h;
  SDL_GetWindowSize(window, &win_w, &win_h);
  int fit_w = win_w / tex_w;
  int fit_h = win_h / tex_h;
  int fit = fit_w < fit_h ? fit_w : fit_h;
  if (fit < 1) fit = 1;
  int dw = tex_w * fit;
  int dh = tex_h * fit;
  if ((dw != crt_dw || dh != crt_dh) && !crt_build(dw, dh)) return;
  for (int y = 0; y < tex_h; y++) {
    int row = y * tex_w;
    for (int x = 0; x < tex_w; x++) {
      int i = row + x;
      int xl = x > 0 ? x - 1 : x;
      int xr = x < tex_w - 1 ? x + 1 : x;
      int c = src[i];
      int l = src[row + xl];
      int rgt = src[row + xr];
      int o = i * 3;
      crt_blur[o] = (uint8_t)((pal_r[l] + 2 * pal_r[c] + pal_r[rgt]) >> 2);
      crt_blur[o + 1] = (uint8_t)((pal_g[l] + 2 * pal_g[c] + pal_g[rgt]) >> 2);
      crt_blur[o + 2] = (uint8_t)((pal_b[l] + 2 * pal_b[c] + pal_b[rgt]) >> 2);
    }
  }
  int k = 0;
  for (int p = 0; p < dw * dh; p++) {
    uint16_t idx = crt_map[p];
    uint8_t r = 0, g = 0, b = 0;
    if (idx != 0xffff) {
      int bi = (int)idx * 3;
      int gain = crt_gain[p];
      int mk = k * 3;
      int rr = (crt_blur[bi] * gain * crt_mask[mk]) >> 16;
      int gg = (crt_blur[bi + 1] * gain * crt_mask[mk + 1]) >> 16;
      int bb = (crt_blur[bi + 2] * gain * crt_mask[mk + 2]) >> 16;
      if (rr > 255) rr = 255;
      if (gg > 255) gg = 255;
      if (bb > 255) bb = 255;
      r = (uint8_t)rr;
      g = (uint8_t)gg;
      b = (uint8_t)bb;
    }
    crt_pixels[p] = SDL_MapRGBA(pixfmt, r, g, b, 255);
    if (++k == 3) k = 0;
  }
  SDL_Rect dst;
  dst.x = (win_w - dw) / 2;
  dst.y = (win_h - dh) / 2;
  dst.w = dw;
  dst.h = dh;
  SDL_UpdateTexture(crt_texture, NULL, crt_pixels, dw * (int)sizeof(uint32_t));
  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
  SDL_RenderClear(renderer);
  SDL_RenderCopy(renderer, crt_texture, NULL, &dst);
  SDL_RenderPresent(renderer);
}

SEXP rdoom_video_present(SEXP fb) {
  if (!texture || !pixels) Rf_error("SDL ainda nao foi iniciado");
  if (TYPEOF(fb) != RAWSXP) Rf_error("framebuffer precisa ser raw");
  int npx = tex_w * tex_h;
  if (Rf_length(fb) < npx) Rf_error("framebuffer curto: %d bytes, esperava %d", (int)Rf_length(fb), npx);
  const unsigned char *src = RAW(fb);
  if (crt_on) {
    present_crt(src);
    return R_NilValue;
  }
  for (int i = 0; i < npx; i++) pixels[i] = palette32[src[i]];
  if (SDL_UpdateTexture(texture, NULL, pixels, tex_w * (int)sizeof(uint32_t)) != 0) {
    Rf_error("SDL_UpdateTexture: %s", SDL_GetError());
  }
  SDL_RenderClear(renderer);
  if (SDL_RenderCopy(renderer, texture, NULL, NULL) != 0) {
    Rf_error("SDL_RenderCopy: %s", SDL_GetError());
  }
  SDL_RenderPresent(renderer);
  return R_NilValue;
}

SEXP rdoom_poll_events(void) {
  const int maxn = 64;
  char types[64][8];
  char keys[64][16];
  int n = 0;
  if (window) {
    SDL_Event ev;
    while (SDL_PollEvent(&ev)) {
      if (n >= maxn) continue;
      if (ev.type == SDL_QUIT ||
          (ev.type == SDL_WINDOWEVENT && ev.window.event == SDL_WINDOWEVENT_CLOSE)) {
        snprintf(types[n], 8, "quit");
        keys[n][0] = '\0';
        n++;
        continue;
      }
      if (ev.type != SDL_KEYDOWN && ev.type != SDL_KEYUP) continue;
      int down = ev.type == SDL_KEYDOWN;
      SDL_Keycode sym = ev.key.keysym.sym;
      if (sym == SDLK_LCTRL || sym == SDLK_RCTRL) {
        note_mod(&held_ctrl, sym == SDLK_LCTRL ? 1 : 2, down, ev.key.repeat, types, keys, &n, maxn, "ctrl");
        continue;
      }
      if (sym == SDLK_LSHIFT || sym == SDLK_RSHIFT) {
        note_mod(&held_shift, sym == SDLK_LSHIFT ? 1 : 2, down, ev.key.repeat, types, keys, &n, maxn, "shift");
        continue;
      }
      if (sym == SDLK_LALT || sym == SDLK_RALT) {
        note_mod(&held_alt, sym == SDLK_LALT ? 1 : 2, down, ev.key.repeat, types, keys, &n, maxn, "alt");
        continue;
      }
      if (!name_of(sym, keys[n], 16)) continue;
      snprintf(types[n], 8, "%s", down ? "down" : "up");
      n++;
    }
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, n));
  for (int i = 0; i < n; i++) {
    SEXP pair = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(pair, 0, Rf_mkChar(types[i]));
    SET_STRING_ELT(pair, 1, Rf_mkChar(keys[i]));
    SET_VECTOR_ELT(out, i, pair);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return out;
}

SEXP rdoom_video_shutdown(void) {
  video_teardown();
  return R_NilValue;
}

SEXP rdoom_set_crt(SEXP enabled) {
  crt_on = Rf_asLogical(enabled) == TRUE;
  if (!crt_on) crt_free();
  return Rf_ScalarLogical(crt_on);
}

SEXP rdoom_toggle_fullscreen(void) {
  if (!window) return Rf_ScalarLogical(0);
  Uint32 flags = SDL_GetWindowFlags(window);
  int on = (flags & (SDL_WINDOW_FULLSCREEN | SDL_WINDOW_FULLSCREEN_DESKTOP)) != 0;
  if (SDL_SetWindowFullscreen(window, on ? 0 : SDL_WINDOW_FULLSCREEN_DESKTOP) != 0) {
    return Rf_ScalarLogical(0);
  }
  return Rf_ScalarLogical(1);
}

static const R_CallMethodDef call_methods[] = {
  {"rdoom_video_init", (DL_FUNC)&rdoom_video_init, 3},
  {"rdoom_video_set_palette", (DL_FUNC)&rdoom_video_set_palette, 1},
  {"rdoom_video_present", (DL_FUNC)&rdoom_video_present, 1},
  {"rdoom_play_sfx", (DL_FUNC)&rdoom_play_sfx, 2},
  {"rdoom_pump_audio", (DL_FUNC)&rdoom_pump_audio, 0},
  {"rdoom_mci", (DL_FUNC)&rdoom_mci, 2},
  {"rdoom_poll_events", (DL_FUNC)&rdoom_poll_events, 0},
  {"rdoom_video_shutdown", (DL_FUNC)&rdoom_video_shutdown, 0},
  {"rdoom_toggle_fullscreen", (DL_FUNC)&rdoom_toggle_fullscreen, 0},
  {"rdoom_set_crt", (DL_FUNC)&rdoom_set_crt, 1},
  {NULL, NULL, 0}
};

void R_init_rdoom_sdl(DllInfo *info) {
  R_registerRoutines(info, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}
