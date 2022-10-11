let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
silent tabonly
cd ~/clones/phone_service
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
argglobal
%argdel
set stal=2
tabnew
tabnew
tabnew
tabrewind
edit rebar.config
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 91 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 90 + 136) / 273)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 3 - ((2 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
3
normal! 0
lcd ~/clones/phone_service
wincmd w
argglobal
terminal ++curwin ++cols=91 ++rows=73 
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 2847 - ((65 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
2847
normal! 0
lcd ~/clones/phone_service
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/content_hub/rebar.config") | buffer ~/clones/phone_service/apps/content_hub/rebar.config | else | edit ~/clones/phone_service/apps/content_hub/rebar.config | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 5 - ((4 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
5
normal! 0
lcd ~/clones/phone_service/apps/content_hub
wincmd w
exe 'vert 1resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 91 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 90 + 136) / 273)
tabnext
edit ~/clones/phone_service/apps/phone_service/src/ivr_sup.erl
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 91 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 90 + 136) / 273)
argglobal
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 9 - ((8 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
9
normal! 0
lcd ~/clones/phone_service/apps/phone_service/src
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/phone_service/src/phone_service_app.erl") | buffer ~/clones/phone_service/apps/phone_service/src/phone_service_app.erl | else | edit ~/clones/phone_service/apps/phone_service/src/phone_service_app.erl | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 5 - ((4 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
5
normal! 0
lcd ~/clones/phone_service/apps/phone_service/src
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/phone_service/src/phone_service_sup.erl") | buffer ~/clones/phone_service/apps/phone_service/src/phone_service_sup.erl | else | edit ~/clones/phone_service/apps/phone_service/src/phone_service_sup.erl | endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
let s:l = 55 - ((54 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
55
normal! 041|
lcd ~/clones/phone_service/apps/phone_service/src
wincmd w
exe 'vert 1resize ' . ((&columns * 91 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 90 + 136) / 273)
tabnext
edit ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 91 + 136) / 273)
argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{-,}}-
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
643
normal! zo
661
normal! zo
664
normal! zo
713
normal! zo
718
normal! zo
723
normal! zo
839
normal! zo
900
normal! zo
923
normal! zo
928
normal! zo
1466
normal! zo
let s:l = 238 - ((23 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
238
normal! 0
lcd ~/clones/phone_service/apps/content_hub/apps
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl") | buffer ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | else | edit ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | endif
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{-,}}-
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
643
normal! zo
661
normal! zo
664
normal! zo
713
normal! zo
718
normal! zo
723
normal! zo
839
normal! zo
900
normal! zo
923
normal! zo
928
normal! zo
1466
normal! zo
let s:l = 1007 - ((34 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1007
normal! 022|
lcd ~/clones/phone_service/apps/content_hub/apps
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl") | buffer ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | else | edit ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | endif
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{-,}}-
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
643
normal! zo
661
normal! zo
664
normal! zo
713
normal! zo
718
normal! zo
723
normal! zo
839
normal! zo
900
normal! zo
923
normal! zo
928
normal! zo
1466
normal! zo
let s:l = 1718 - ((30 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1718
normal! 0
lcd ~/clones/phone_service/apps/content_hub/apps
wincmd w
3wincmd w
exe 'vert 1resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 91 + 136) / 273)
tabnext
edit ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
2wincmd h
wincmd w
wincmd w
set nosplitbelow
set nosplitright
wincmd t
set winminheight=0
set winheight=1
set winminwidth=0
set winwidth=1
exe 'vert 1resize ' . ((&columns * 91 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 90 + 136) / 273)
argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{-,}}-
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
643
normal! zo
661
normal! zo
664
normal! zo
713
normal! zo
718
normal! zo
723
normal! zo
839
normal! zo
900
normal! zo
923
normal! zo
928
normal! zo
1466
normal! zo
let s:l = 231 - ((51 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
231
normal! 026|
lcd ~/clones/phone_service/apps/phone_service/src
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl") | buffer ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | else | edit ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | endif
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{-,}}-
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
300
normal! zc
318
normal! zc
336
normal! zc
354
normal! zc
440
normal! zc
461
normal! zc
643
normal! zo
661
normal! zo
664
normal! zo
713
normal! zo
718
normal! zo
723
normal! zo
839
normal! zo
900
normal! zo
923
normal! zo
928
normal! zo
1466
normal! zo
let s:l = 373 - ((37 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
373
normal! 0
lcd ~/clones/phone_service/apps/phone_service/src
wincmd w
argglobal
if bufexists("~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl") | buffer ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | else | edit ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl | endif
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{-,}}-
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
643
normal! zo
661
normal! zo
664
normal! zo
713
normal! zo
718
normal! zo
723
normal! zo
839
normal! zo
900
normal! zo
923
normal! zo
928
normal! zo
1466
normal! zo
let s:l = 1636 - ((24 * winheight(0) + 36) / 73)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1636
normal! 026|
lcd ~/clones/phone_service/apps/phone_service/src
wincmd w
exe 'vert 1resize ' . ((&columns * 91 + 136) / 273)
exe 'vert 2resize ' . ((&columns * 90 + 136) / 273)
exe 'vert 3resize ' . ((&columns * 90 + 136) / 273)
tabnext 3
set stal=1
badd +4 ~/clones/phone_service/apps/content_hub/rebar.config
badd +2 ~/clones/phone_service/rebar.config
badd +1 ~/clones/phone_service/apps/content_hub
badd +2 ~/clones/phone_service/apps/phone_service/src/phone_service_sup.erl
badd +2 ~/clones/phone_service/apps/phone_service/src/phone_service_app.erl
badd +0 ~/clones/phone_service/apps/phone_service/src/phone_service.app.src
badd +4 ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content_hub_app.erl
badd +1 ~/clones/phone_service/apps/content_hub/apps/content_hub/src
badd +35 ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content_hub_sup.erl
badd +1 ~/clones/phone_service/apps/content_hub/apps/content_hub/src/content.erl
badd +5 ~/clones/phone_service/apps/phone_service/src/ivr_sup.erl
badd +1 ~/clones/phone_service/apps/phone_service/src/ivr.erl
badd +99 ~/clones/phone_service/apps/phone_service/src/fs.erl
badd +2 ~/clones/phone_service/apps/phone_service/src/futil.erl
if exists('s:wipebuf') && len(win_findbuf(s:wipebuf)) == 0
  silent exe 'bwipe ' . s:wipebuf
endif
unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToOS
set winminheight=1 winminwidth=1
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save
let g:this_session = v:this_session
let g:this_obsession = v:this_session
doautoall SessionLoadPost
unlet SessionLoad
" vim: set ft=vim :
