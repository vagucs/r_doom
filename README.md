# r_doom

![DOOM running in R with SDL2](screenshot/doom.png)

**Video:** [DOOM running in R](https://youtu.be/hEJqkEXS944)

DOOM generic ported from **[python_doom](https://github.com/vagucs/python_doom)** to **R 4.6 + SDL2**.

By **Wagner Nunes da Silva**

- vagucs@bol.com.br
- vagucs@vagucs.com.br
- vagucs@gmail.com
- [www.vagucs.com.br](https://www.vagucs.com.br)
- [LinkedIn](https://www.linkedin.com/in/wagner-nunes-da-silva-b0a15360)

This tree is that Python engine again, in R. The game loop, the map, and the renderer stay in R. A small C bridge (`src/rdoom_sdl.c`) opens the window, expands PLAYPAL, reads the keyboard, queues sound, and, with `-crt`, draws the tube.

Versão em português: [README.pt.md](README.pt.md)

---

## What this project is

`python_doom` is a condensed, playable DOOM engine in Python. This directory is the **same study piece**, rewritten in R:

- Window, keys, PCM: **SDL2**, reached from R with `.Call`
- Framebuffer: 320×200, one PLAYPAL index per byte (`raw`), stretched to a 640×400 window
- Game tick: 35 Hz (`TICRATE`). Each displayed frame runs up to 4 tics
- Renderer: BSP, visplanes, columns, spans, sprites, the weapon sprite
- Map: VERTEXES, LINEDEFS, SIDEDEFS, SECTORS, SEGS, SSECTORS, NODES, THINGS, BLOCKMAP, REJECT
- Play: walk, doors, lifts, switches, exit, pickups, weapons (fist, chainsaw, pistol, shotgun, chaingun, rocket, plasma, BFG), status bar, Tab automap, DS* sound, MUS→MIDI music, ESC menu, F1 help, intermission tally, melt wipe on a level change, monster look/chase/attack

You need a legal IWAD (shareware `doom1.wad` or commercial `doom.wad` / `doom2.wad`). This repository does not ship commercial WAD data.

It is a **condensed educational port**: the engine is R, the native layer is only the SDL bridge.

Left out of this tree:

- Network, joystick, mouse look
- Demo playback, recording, `-timedemo`
- Palette quantization (`-colors`, `-shades`, `-gray`, `-neogeo`)
- A save file on disk (F2 / F3 open the save and load screens)

---

## Educational purpose

This project is a **study piece**. The Python port already dropped the preprocessor and Harbour's 1-based arrays. The R port asks a different question: **what breaks when the language is a statistics runtime**, with 1-based vectors, 32-bit signed integers, and copy-on-modify lists.

What it is meant to teach:

- **Python, then R.** Open `python_doom/doom/` next to `r_doom/R/`. The names stay close (`thrust`, `fixed_mul`, `line_attack`) so the two files can sit side by side.
- **32-bit wrap, written out.** An R integer stops at 2^31−1. Angles and fixed-point stay numeric doubles. `as_u32` / `as_i32` put the DOOM wrap back.
- **Reference versus copy.** A mobj is an `environment`, so a missile and the blockmap share the same object. A `list` passed into a function is a copy.
- **Where R is enough.** Columns, floors, sprites, and thinkers run in R. SDL2 is the window, the keyboard, the PCM queue, and the CRT present.

Suggested way to study:

1. Run `doom.bat`, then read `main.R` and `R/game.R` — boot, tic, input.
2. Compare `R/compat.R` with `python_doom/doom/compat.py`.
3. Open `R/render.R` next to `python_doom/doom/render.py`.
4. Follow a door from **Space** (`use_lines` in `R/collision.R`) through `R/specials.R`.
5. Follow a shot from **Ctrl** in `R/player.R` to `spawn_player_missile` in `R/enemy.R`.

---

## From Python to R

Python lists are 0-based. R vectors are 1-based. WAD lumps, BSP nodes, menu rows, and clip ranges stay 0-based in the data and are read with `+ 1`.

| Python (`python_doom`) | R (`r_doom`) |
| --- | --- |
| `thing.x` | `thing$x` |
| `None` | `NULL` |
| `items[0]` | `items[[1]]` |
| class / dict | `environment` (shared reference) |
| unlimited `int` | `numeric`; `as_u32` / `as_i32` wrap at 32 bits |
| `&`, `\|`, `^` | `bitwAnd` / `bitwOr` / `bitwXor` under 2^31; `u32_and` above that |
| `x >> n` | `u32_shr` / `shar` |
| `fixed_mul` / `fixed_div` | `fixed_mul` / `fixed_div` |
| `bytearray` framebuffer | `raw` vector, one palette index per pixel |
| pygame | SDL2 through `.Call` |
| `slot[i] = None` | `x[[i]] <- NULL` **deletes** the slot; `x[i] <- list(NULL)` stores `NULL` |
| `a * b // c` | `(a * b) %/% c` — `%/%` binds tighter than `*` |
| a field named `next` | `nxt` — `$next` does not parse |

`identical(5, 5L)` is false. Match a DOOM number with `==`, or with both the double and the integer. Sprite frame letters use `utf8ToInt`, because R string order follows the locale.

### Side-by-side: `P_Thrust`

Python (`doom/player.py`):

```python
def thrust(mo, angle, move):
    mo.momx += fixed_mul(move, fine_cos(angle))
    mo.momy += fixed_mul(move, fine_sin(angle))
```

R (`R/player.R`):

```r
thrust <- function(mo, angle, move) {
  mo$momx <- mo$momx + fixed_mul(move, fine_cos(angle))
  mo$momy <- mo$momy + fixed_mul(move, fine_sin(angle))
}
```

`.` becomes `$`. `mo` is an environment, so the new momentum stays on the mobj the caller already holds.

---

## Technology

| Layer | This port | Python (`python_doom`) |
| --- | --- | --- |
| Language | R 4.6 (the `doom.bat` path is `C:\Program Files\R\R-4.6.1\bin`) | Python 3.10+ |
| Window, keys, mixer | SDL2, `.Call` into `rdoom_sdl.dll` | pygame 2.x |
| Palette blit / CRT | C in `src/rdoom_sdl.c` | numpy |
| IWAD | the same lumps | the same lumps |
| Build | `doom.bat` compiles the bridge, then runs `Rscript` | `pip install -r requirements.txt` |

The renderer is R. Recompiling is only for a change under `src/`.

---

## How to run

From this directory, on Windows:

```
doom.bat
doom.bat -iwad DOOM1.WAD
doom.bat -iwad ..\DOOM1.WAD -fps
doom.bat -crt
doom.bat -file extra.wad
```

`doom.bat` builds `bin/rdoom_sdl.dll`, copies `bin/SDL2.dll`, and starts `main.R`. The window opens at 640×400. Without `-crt`, the 320×200 texture is stretched with nearest-neighbor. There is no VSync.

`make run` does the same build, then `Rscript --vanilla main.R`.

With no `-iwad`, the loader looks for `doom1.wad` / `DOOM1.WAD` / `doom.wad` / `doom2.wad` in the current directory, the parent, the grandparent, `DOOMWADDIR`, and `DOOMWADPATH`.

The bridge expects SDL2 from the Rtools / MSYS layout used by `build_sdl.bat`: headers under the Rtools `include\SDL2` tree, the import library `libSDL2.dll.a`, and `SDL2.dll` copied into `bin/`.

---

## Keys

Classic DOOM controls. Movement is **arrow keys only**, so letter keys stay free for cheat codes.

### Movement and actions

| Key | Action |
| --- | --- |
| Arrow keys | Forward, back, turn |
| **Shift** | Run |
| **Alt** | Strafe (hold) |
| **,** / **.** | Strafe left / right |
| **Ctrl** | Fire |
| **Space** / **E** | Use / open door |
| **1** | Fist / chainsaw |
| **2**–**7** | Pistol, shotgun, chaingun, rocket, plasma, BFG |
| **Enter** | On the title, open the menu |
| **Esc** | Menu |
| **F1** | Help (`HELP2` when the WAD has it) |
| **F2** / **F3** | Save screen / load screen |
| **Tab** | Toggle the automap. **F** follows the player, **G** toggles the grid, arrows pan when follow is off |
| **-** / **=** | Zoom the automap when it is open; otherwise a smaller / larger 3D view |
| **Alt+Enter** | Fullscreen |

**Screen Size** and **Graphic Detail** (HIGH/LOW) in the options menu change the 3D view. LOW draws half the columns and doubles each one. The weapon stays centered.

On the title, **Esc** or **Enter** opens the menu. Arrows and **Enter** choose. **Backspace** goes back. **Y** confirms quit.

### Cheats

Type these during a level, with the menu closed. No Enter. On Nightmare skill only **IDCLEV** and **IDDT** are accepted.

| Code | Effect |
| --- | --- |
| **IDDQD** | God mode |
| **IDKFA** | All weapons, ammo, keys, and armor |
| **IDFA** | Weapons, ammo, and armor |
| **IDCLIP** / **IDSPISPOPD** | No clipping |
| **IDDT** | With the automap open: all walls, then things, then back |
| **IDBEHOLD** | Lists the power-ups; then **V** **S** **I** **R** **A** **L** |
| **IDCHOPPERS** | Chainsaw |
| **IDMYPOS** | Coordinates and angle |
| **IDCLEV** + 2 digits | Warp (`11` = E1M1, or MAP11 on a commercial IWAD) |
| **IDMUS** + 2 digits | Change music |

Messages on screen are ASCII, so the status font can draw them.

---

## Command-line parameters

### IWAD

| Parameter | Description |
| --- | --- |
| `-iwad file.wad` | IWAD to load |
| `file.wad` | Same thing, without `-iwad` |
| `-file wad [wad…]` | Extra PWADs after the IWAD |

### Video

| Parameter | Description |
| --- | --- |
| `-crt` | CRT tube: barrel distortion, vignette, horizontal blur, phosphor mask. Drawn in the C bridge at the window size |
| `-fps` | Frames per second at the top-right. Counts presented views, not the 35 Hz tic |

Skill, episode, and map are chosen in the menu. A level change melts the screen. Starting a new game from the title does not.

---

## Layout

```
main.R               entry: Rscript --vanilla main.R
doom.bat             build the bridge, then run
build_sdl.bat        gcc → bin/rdoom_sdl.dll + bin/SDL2.dll
Makefile             make sdl / make run
src/rdoom_sdl.c      SDL2 bridge
R/                   engine
```

| Path | Python |
| --- | --- |
| `R/compat.R` | `doom/compat.py` |
| `R/wad.R` | `doom/wad.py` |
| `R/video.R` | `doom/video.py` (the R side of the bridge) |
| `R/v_video.R` | `doom/v_video.py` |
| `R/tables.R` | `doom/tables.py` |
| `R/r_data.R` | `doom/r_data.py` |
| `R/render.R` | `doom/render.py` |
| `R/p_setup.R` | `doom/world.py` |
| `R/collision.R` | `doom/collision.py` |
| `R/player.R` | `doom/player.py` |
| `R/specials.R` | `doom/specials.py` |
| `R/info_states.R` | state table from `doom/info.py` |
| `R/sprites.R` | `doom/sprites.py` |
| `R/enemy.R` | `doom/enemy.py` |
| `R/status.R` | `doom/status.py` |
| `R/sound.R` | `doom/sound.py` + `doom/mus2mid.py` |
| `R/menu.R` | `doom/menu.py` |
| `R/wi.R` | `doom/wi_stuff.py` |
| `R/wipe.R` | `doom/wipe.py` |
| `R/cheats.R` | cheat machine in `doom/game.py` |
| `R/am_map.R` | `doom/am_map.py` |
| `R/game.R` | `doom/game.py` |
| `src/rdoom_sdl.c` | pygame window, blit, mixer, and the CRT present |

---

## Lineage

1. **[python_doom](https://github.com/vagucs/python_doom)** — Python + pygame
2. **[r_doom](https://github.com/vagucs/r_doom)** — R + SDL2 (this tree)

---

## Donate

### Ethereum

`0x1b64038A2b1DB73ABd0068d8B9B0d1dC5a90C5F1`

### PIX

Key: `vagucs@bol.com.br`
