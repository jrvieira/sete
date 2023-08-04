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
badd +1 term://~/sete//39502:vifm\ \ \ /home/zero/sete/app\ \ \'--choose-files\'\ \'/tmp/nvim.zero/PRBOtj/2\'\ \'--on-choose\'\ \'echo\ \$VIFM_OPEN_TYPE\ >/tmp/nvim.zero/PRBOtj/3\'\ \'+command\ EditVim\ \ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'edit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ VsplitVim\ :let\ \$VIFM_OPEN_TYPE=\'/\'\'vsplit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ SplitVim\ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'split\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ DiffVim\ \ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'vert\ diffsplit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ PeditVim\ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'pedit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ TabVim\ \ \ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'tablast\ \|\ tab\ drop\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'
badd +0 app/Main.hs
badd +0 term://~/sete//39879:/usr/bin/fish
argglobal
%argdel
$argadd app
edit app/Main.hs
let s:save_splitbelow = &splitbelow
let s:save_splitright = &splitright
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd _ | wincmd |
split
1wincmd k
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
exe '1resize ' . ((&lines * 47 + 48) / 97)
exe 'vert 1resize ' . ((&columns * 138 + 208) / 417)
exe '2resize ' . ((&lines * 47 + 48) / 97)
exe 'vert 2resize ' . ((&columns * 138 + 208) / 417)
exe 'vert 3resize ' . ((&columns * 138 + 208) / 417)
exe 'vert 4resize ' . ((&columns * 139 + 208) / 417)
argglobal
if bufexists(fnamemodify("term://~/sete//39879:/usr/bin/fish", ":p")) | buffer term://~/sete//39879:/usr/bin/fish | else | edit term://~/sete//39879:/usr/bin/fish | endif
if &buftype ==# 'terminal'
  silent file term://~/sete//39879:/usr/bin/fish
endif
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 23) / 47)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 014|
wincmd w
argglobal
enew
balt app/Main.hs
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
wincmd w
argglobal
balt term://~/sete//39502:vifm\ \ \ /home/zero/sete/app\ \ \'--choose-files\'\ \'/tmp/nvim.zero/PRBOtj/2\'\ \'--on-choose\'\ \'echo\ \$VIFM_OPEN_TYPE\ >/tmp/nvim.zero/PRBOtj/3\'\ \'+command\ EditVim\ \ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'edit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ VsplitVim\ :let\ \$VIFM_OPEN_TYPE=\'/\'\'vsplit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ SplitVim\ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'split\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ DiffVim\ \ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'vert\ diffsplit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ PeditVim\ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'pedit\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'\ \'+command\ TabVim\ \ \ \ :let\ \$VIFM_OPEN_TYPE=\'/\'\'tablast\ \|\ tab\ drop\'/\'\'\ \|\ execute\ \'/\'\'cnoremap\ j\ \<cr>\'/\'\'\ \|\ normal\ gs:editj\'
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
let s:l = 1 - ((0 * winheight(0) + 47) / 95)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 1
normal! 0
wincmd w
argglobal
enew
balt term://~/sete//39879:/usr/bin/fish
setlocal fdm=indent
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=1
setlocal fml=1
setlocal fdn=1
setlocal fen
wincmd w
3wincmd w
exe '1resize ' . ((&lines * 47 + 48) / 97)
exe 'vert 1resize ' . ((&columns * 138 + 208) / 417)
exe '2resize ' . ((&lines * 47 + 48) / 97)
exe 'vert 2resize ' . ((&columns * 138 + 208) / 417)
exe 'vert 3resize ' . ((&columns * 138 + 208) / 417)
exe 'vert 4resize ' . ((&columns * 139 + 208) / 417)
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
