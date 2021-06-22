# Dotfiles

This is a repository with my [configuration files](http://en.wikipedia.org/wiki/Configuration_file), those that in Linux/macOS normally are these files under the `$HOME` directory that are hidden and preceded by a dot, AKA *dotfiles*.

## 🗺️ Overview

The primary goal is to increase CLI productivity on macOS, though many scripts run just fine on any POSIX implementation and it is easy to build environment again by running just [installation command](#oneliner) of one liner.

My primary OS is macOS (10.15.x) and some of these configurations are tuned to work on that platform. The bash files are more generic and friendly toward other Unix-based operating systems.

## Status

As of writing (2021-06-22) it works for me on my machine.
- **macOS** Catallina 10.15.7
- **[fish](https://github.com/fish-shell/fish-shell)** 3.2.2
- **[Tmux](https://github.com/tmux/tmux/wiki)** 3.1c
- **Emacs** 28.0.05
- **[iTerm2](https://iterm2.com)** 3.4.8

Note: You can clone or fork them freely, but I don't guarantee that they fit you.

## 🛠️ Installation

> ⚠️ **DO NOT** run the following snippet if you do not fully understand [what it does](#proc).

Run the installation command below in your terminal.

| Tools | <a name="oneliner">The installation command</a> |
|:-:|:-:|
| cURL | bash -c "$(curl -fsSL raw.githubusercontent.com/naoking158/dotfiles/main/etc/install)" |
| Wget | bash -c "$(wget -qO - raw.githubusercontent.com/naoking158/dotfiles/main/etc/install)" |

- :warning: NOTE

	It is almost the same as the command below except for executing through a Web site directly.
	```console
	make deploy
	```

	It is not necessary to perform `make deploy` at all if this repository was installed by the [installation command](#oneliner).

**<a name="proc">What does it do?</a>**

1. Downloads this repository
2. Deploy (i.e., *copy* or *create symlink*) dot files to your home directory; `make deploy`
> If dot files exist, they are copied to `$HOME/bak_dotfiles/` directory and this repository's dot files are merged to original one.
3. **Optional**: Run all programs for setup in `./etc/init/` directory; `make init`, when running the [installation command](#oneliner) specify `-s init` as an argument

When the [installation command](#oneliner) format is not `curl -L URL | sh` but `sh -c "$(curl -L URL)"`, shell will be restart automatically. If this is not the case, it is necessary to restart your shell manually.

## 🔃 Updating

To update later on, just run this command.
```console
make update
```

In addition, there are several git submodules included in this configuration. On a new installation these submodules need to be initialized and updated.

## ⚙️ Setup

All configuration files for setup is stored within the `etc/init/` directory. By running this command, you can interactively setup all preferences.
```console
make init
```

To run `make init` immediately after running the [installation command](#oneliner):
```console
bash -c "$(curl -fsSL raw.githubusercontent.com/naoking158/dotfiles/main/etc/install)" -s init 
```

> Initialization of [homebrew](https://brew.sh) and [fisher](https://github.com/jorgebucaran/fisher) does not complete properly. This will be resolved in the future when [homebrew.sh](etc/init/homebrew.sh) and [fisher.sh](etc/init/fisher.sh) have the ability to set the path automatically.

## Components

- **bin/**: Anything in `bin/` will get added to your `$PATH` and be made available everywhere.
- **etc/init/**: Configuration file storage to be executed initially for setup.

## 💡 Inspiration

- <https://github.com/b4b4r07/dotfiles>
- [最強の dotfiles 駆動開発と GitHub で管理する運用方法](https://qiita.com/b4b4r07/items/b70178e021bef12cd4a2#fnref2)

## License

[MIT](LICENSE)
