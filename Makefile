DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES := $(wildcard .??*)
EXCLUSIONS := .DS_Store .git .gitmodules .travis.yml .ssh
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

all:


list: ## Show dot files in this repo
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)


deploy: ## Create symlink to home directory
	./etc/deploy run

nix:
	./etc/nix-installer
  
init: ## Initialize
	@echo 'Execute shell scripts located in ./etc/init/ manually.'
	# @echo ''
	# @echo 'etc/init/*.sh will be conducted.'
	# @make .confirm_prompt
	# @echo ''
	# @echo '==> Start to initialize.'
	# @echo ''
	# @$(foreach val, $(wildcard ./etc/init/*.sh), bash $(val);)


clean: ## Remove the dot files
	@echo 'Remove the dot files in your home directory.'
	@echo ''
	@make .confirm_prompt
	@echo ''
	./etc/clean

update:
	git pull origin main


.confirm_prompt:
	@read -p "Are you sure? [y/N] " yn && [ $${yn:-N} = y ]
