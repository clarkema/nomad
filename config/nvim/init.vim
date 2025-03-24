set runtimepath^=~/.vim runtimepath+=~/.vim/after
"let &packpath = &runtimepath
set packpath=~/.nomad/nvim
source ~/.vimrc

let mapleader = " "
set undodir=~/.nvim/backups

"call plug#begin('~/.nomad/nvim-vimplug')
"Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} 
"Plug 'neovim/nvim-lspconfig'
""Plug 'neoclide/coc.nvim', {'branch': 'release'}
"Plug 'nvim-lua/plenary.nvim'
"Plug 'nvim-telescope/telescope.nvim', { 'branch': '0.1.x' }
"Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' }
"Plug 'L3MON4D3/LuaSnip', {'tag': 'v2.*', 'do': 'make install_jsregexp'}
"call plug#end()

" LuaSnip {{{
" press <Tab> to expand or jump in a snippet. These can also be mapped separately
" via <Plug>luasnip-expand-snippet and <Plug>luasnip-jump-next.
"imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>' 
"" -1 for jumping backwards.
"inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>
"
"snoremap <silent> <Tab> <cmd>lua require('luasnip').jump(1)<Cr>
"snoremap <silent> <S-Tab> <cmd>lua require('luasnip').jump(-1)<Cr>
"
"" For changing choices in choiceNodes (not strictly necessary for a basic setup).
"imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
"smap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'
" }}}


lua require('config')
lua require('clarkema.core')
lua require('clarkema.lazy')
