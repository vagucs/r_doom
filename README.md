# r_doom

DOOM em R, a partir do `python_doom`. O R usado pelo `doom.bat` e pelo `Makefile` é o instalado em `C:\Program Files\R\R-4.6.1\bin`. A janela é SDL2. O renderer continua em R, num framebuffer de 320×200. O bridge em `src/rdoom_sdl.c` cria a janela, expande a PLAYPAL, lê o teclado, mistura o som e, com `-crt`, desenha o tubo.

```text
doom.bat
doom.bat -iwad DOOM1.WAD
doom.bat -crt
```

`doom.bat` compila `bin/rdoom_sdl.dll` e copia `bin/SDL2.dll` antes de abrir o jogo. A janela nasce em 640×400. Sem `-crt`, a textura de 320×200 é esticada em nearest-neighbor. Não há VSync.

## SDL2 no Windows

1. Dependência: SDL2. A DLL esperada é `C:\msys64\ucrt64\bin\SDL2.dll`. Os headers são os do Rtools, em `C:\rtools45\x86_64-w64-mingw32.static.posix\include\SDL2`.
2. O compilador é o gcc do Rtools 45: `C:\rtools45\x86_64-w64-mingw32.static.posix\bin\gcc.exe`.
3. A lib de importação é `C:\msys64\ucrt64\lib\libSDL2.dll.a`. O link também usa `R.dll`.
4. Compilar: `build_sdl.bat` (ou `make sdl`).
5. Saída: `bin/rdoom_sdl.dll` e `bin/SDL2.dll`.
6. O R faz `dyn.load` das duas. `SDL2.dll` precisa ficar em `bin/`, que entra no `PATH` antes da carga.

No título, `Esc` ou `Enter` abre o menu. Setas e `Enter` escolhem. `Backspace` volta. `F1` é a ajuda. `Y` confirma sair. `-` e `=` mudam o tamanho da tela. `Alt+Enter` põe em tela cheia. Durante a fase valem `iddqd`, `idkfa`, `idfa`, `idclip`, `idspispopd`, `idbehold`, `idchoppers`, `idmypos`, `idclev` e `idmus`. No pesadelo só `idclev`. O laço corre a 35 Hz.

## O que este porte inclui

- Teclado
- Som (efeitos e música do WAD)
- Leitura de WAD (IWAD e `-file`)

## O que fica de fora

- Rede
- Joystick
- Demo, gravação de demo e `-timedemo`
- Quantização de paleta (`-colors`, `-shades`, `-gray`, `-neogeo`)

## Passos

1. **Tela de título.** Feito. WAD, paleta, patch e janela.
2. **Laço e teclado.** Feito. Tic a 35 Hz. `Esc` abre o menu (novo jogo, episódio, skill, opções, load, save, leia isto, sair). Setas, `Ctrl`, `Espaço` e `1`–`7` ficam registrados para a fase. O mapa ainda não entra.
3. **Mapa.** Feito. `VERTEXES`, `LINEDEFS`, `SIDEDEFS`, `SECTORS`, `SEGS`, `SSECTORS`, `NODES`, `THINGS`, `BLOCKMAP` e `REJECT`. Escolher a skill carrega a fase e desenha as linhas vistas de cima. A vista em 3D ainda não entra.
4. **Texturas.** Feito. `PNAMES`, `TEXTURE1`/`TEXTURE2`, patches, flats e `COLORMAP`. A planta pinta cada linha com a cor da textura da parede.
5. **Vista.** Feito. BSP, paredes, chão, teto, céu e sprites no framebuffer de 320×200, a partir do ponto de partida. `-crt` liga o tubo: curvatura, vinheta, scanline e fósforo.
6. **Jogador.** Feito. Andar, correr (`Shift`), gravidade, usar parede (`Espaço` ou `E`), atirar (`Ctrl`) e trocar de arma (`1`–`7`). A arma sobe na frente da vista. A vista redesenha quando a câmera, a arma ou um setor muda.
7. **Setores.** Feito. Portas, elevadores, interruptores e saída da fase, com o movimento no tempo. A porta sobe duas unidades por tic, espera e fecha. A saída abre a intermissão e segue para a fase seguinte.
8. **Inimigos.** Feito. Olham, perseguem e atacam. Zumbi atira, imp lança bola de fogo, demônio morde. O corpo cai no chão. A demo não entra.
9. **Som.** Feito. Efeitos `DS*` no tiro, na porta, no dano e no item. A música da fase é o lump `D_*`, convertido de MUS para MIDI. No título toca a introdução.
10. **Barra e intermissão.** Feito. A barra tem vida, munição, armas, chaves e rosto. Ao sair da fase a tela derrete para a contagem e, depois, para a fase seguinte. A arma e a munição continuam. O fim do episódio volta ao título. Abrir um jogo novo não derrete a tela.

`-` diminui a tela e `=` / `+` aumentam. `Alt+Enter` alterna a tela cheia. No menu de opções, LOW desenha a vista na metade da largura e estica cada coluna, com a arma no centro.

Os passos acima estão no jogo. Cada um depende do anterior para a fase aparecer e dar para andar nela.
