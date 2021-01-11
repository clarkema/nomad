.PHONY: all
all: vim-command-t

.PHONY: vim-command-t
vim-command-t:
	cd vim/bundle/command-t/ruby/command-t/ext/command-t && \
		ruby extconf.rb && \
		make
