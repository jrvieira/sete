let SessionLoad = 1
let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/sete
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
let s:shortmess_save = &shortmess
if &shortmess =~ 'A'
  set shortmess=aoOA
else
  set shortmess=aoO
endif
badd +1 app/Main.hs
badd +1 app/Verse/Verse.hs
badd +1 app/Verse/Sim.hs
badd +1 app/Verse/Art.hs
argglobal
%argdel
$argadd app/Main.hs
edit app/Verse/Verse.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
3wincmd h
wincmd w
wincmd w
wincmd w
let &splitbelow = s:save_splitbelow
let &splitright = s:save_splitright
wincmd t
let s:save_winminheight = &winminheight
let s:save_winminwidth = &winminwidth
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 103 + 208) / 417)
exe 'vert 2resize ' . ((&columns * 104 + 208) / 417)
exe 'vert 3resize ' . ((&columns * 104 + 208) / 417)
exe 'vert 4resize ' . ((&columns * 103 + 208) / 417)
argglobal
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
let s:l = 140 - ((75 * winheight(0) + 52) / 104)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 140
normal! 010|
wincmd w
argglobal
if bufexists(fnamemodify("app/Main.hs", ":p")) | buffer app/Main.hs | else | edit app/Main.hs | endif
if &buftype ==# 'terminal'
  silent file app/Main.hs
endif
balt app/Verse/Verse.hs
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 52) / 104)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("app/Verse/Sim.hs", ":p")) | buffer app/Verse/Sim.hs | else | edit app/Verse/Sim.hs | endif
if &buftype ==# 'terminal'
  silent file app/Verse/Sim.hs
endif
balt app/Verse/Verse.hs
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 52) / 104)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
wincmd w
argglobal
if bufexists(fnamemodify("app/Verse/Art.hs", ":p")) | buffer app/Verse/Art.hs | else | edit app/Verse/Art.hs | endif
if &buftype ==# 'terminal'
  silent file app/Verse/Art.hs
endif
balt app/Verse/Verse.hs
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 52) / 104)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
lcd ~/sete
wincmd w
exe 'vert 1resize ' . ((&columns * 103 + 208) / 417)
exe 'vert 2resize ' . ((&columns * 104 + 208) / 417)
exe 'vert 3resize ' . ((&columns * 104 + 208) / 417)
exe 'vert 4resize ' . ((&columns * 103 + 208) / 417)
tabnext 1
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0 && getbufvar(s:wipebuf, '&buftype') isnot# 'terminal'
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20
let &shortmess = s:shortmess_save
let &winminheight = s:save_winminheight
let &winminwidth = s:save_winminwidth
let s:sx = expand("<sfile>:p:r")."x.vim"
if filereadable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
