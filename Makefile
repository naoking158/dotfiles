DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES := $(wildcard .??*)
EXCLUSIONS := .DS_Store .git .gitmodules .travis.yml .ssh
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

.DEFAULT_GOAL := help

all:

list: ## Show dot files in this repo
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)

deploy: ## Create symlink to home directory
	@echo ''
	@echo '==> Start to deploy dotfiles to home directory.'
	@echo ''
	@$(foreach val, $(DOTFILES), ln -sfnv $(abspath $(val)) $(HOME)/$(val);)

init: ## Initialize
	@echo ''
	@echo 'etc/init/*.sh will be conducted.'
	@make .confirm_prompt
	@echo ''
	@echo '==> Start to initialize.'
	@echo ''
	@$(foreach val, $(wildcard ./etc/init/*.sh), bash $(val);)

clean: ## Remove the dot files and this repo
	@echo 'Remove dot files in your home directory.'
	@echo ''
	@make .confirm_prompt
	@echo ''
	@-$(foreach val, $(DOTFILES), rm -vrf $(HOME)/$(val);)
	-rm -rf $(DOTPATH)

update:
	git pull origin main

.confirm_prompt:
	@read -p "Are you sure? [y/N] " yn && [ $${yn:-N} = y ]
