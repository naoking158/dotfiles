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
	@test -d "$(HOME)/tmp_naoking_dotfiles/" || mkdir $(HOME)/tmp_naoking_dotfiles
	@$(foreach val, $(DOTFILES), make create-symlink-safely ARG=$(val);)
	@rm -vrf $(HOME)/tmp_naoking_dotfiles
	@echo 'All done, except for `.ssh`.'

create-symlink-safely:
ifneq ("$(wildcard $(HOME)/$(ARG))","")
	@echo "\nBackup: from $(HOME)/$(ARG) to $(HOME)/bak_dotfiles/"
	@echo "Create Symlink:"
	@test -d "$(HOME)/bak_dotfiles/" || mkdir $(HOME)/bak_dotfiles \
		&& cp -r $(HOME)/$(ARG) $(HOME)/bak_dotfiles/
	@mv -f $(HOME)/$(ARG) $(HOME)/tmp_naoking_dotfiles/$(ARG)
	@rsync -a ./$(ARG) $(HOME)/tmp_naoking_dotfiles/
	@ln -sfnv $(abspath $(ARG)) $(HOME)/$(ARG)
	@rsync -a $(HOME)/tmp_naoking_dotfiles/$(ARG) ./
else
	@echo "Create Symlink:"
	@ln -sfnv $(abspath $(ARG)) $(HOME)/$(ARG)
endif

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
