DOTPATH    := $(realpath $(dir $(lastword $(MAKEFILE_LIST))))
CANDIDATES := $(wildcard .??*)
EXCLUSIONS := .DS_Store .git .gitmodules .travis.yml .ssh
DOTFILES   := $(filter-out $(EXCLUSIONS), $(CANDIDATES))

all:


list: ## Show dot files in this repo
	@$(foreach val, $(DOTFILES), /bin/ls -dF $(val);)


deploy: ## Create symlink to home directory
	@echo ''
	@echo '==> Start to deploy dotfiles to home directory.'
	@echo ''
	@$(foreach val, $(DOTFILES), make create-symlink-safely ARG=$(val);)
	@echo ''
	@echo 'All done, except for `.ssh/config`.'
ifneq ("$(wildcard $(HOME)/bak_dotfiles)","")
	@echo ''
	@echo 'Original dotfiles have been moved to $(HOME)/bak_dotfiles/.'
	@echo 'Relocate them manually if necessary.'
endif


create-symlink-safely:
ifneq ("$(wildcard $(HOME)/$(ARG))","")
	@echo "\nBackup: from $(HOME)/$(ARG) to $(HOME)/bak_dotfiles/"
	@test -d "$(HOME)/bak_dotfiles/" || mkdir $(HOME)/bak_dotfiles \
		&& mv -f $(HOME)/$(ARG) $(HOME)/bak_dotfiles/
endif
	@echo "Create Symlink:"
	@ln -sfnv $(abspath $(ARG)) $(HOME)/$(ARG)


init: ## Initialize
	@echo ''
	@echo 'etc/init/*.sh will be conducted.'
	@make .confirm_prompt
	@echo ''
	@echo '==> Start to initialize.'
	@echo ''
	@$(foreach val, $(wildcard ./etc/init/*.sh), bash $(val);)


clean: ## Remove the dot files
	@echo 'Remove the dot files in your home directory.'
	@echo ''
	@make .confirm_prompt
	@echo ''
	@-$(foreach val, $(DOTFILES), rm -vrf $(HOME)/$(val);)


update:
	git pull origin main


.confirm_prompt:
	@read -p "Are you sure? [y/N] " yn && [ $${yn:-N} = y ]
