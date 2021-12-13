# Dotfiles

## Status

As of writing (2021-11-04) it works for me on my machine.
- **macOS** Monterey 12.0.1 (M1)
- **Ubuntu** 18.04.5 LTS
- **[fish](https://github.com/fish-shell/fish-shell)** 3.3.1
- **[Tmux](https://github.com/tmux/tmux/wiki)** 3.2a
- **Emacs** 28.0.60
- **[iTerm2](https://iterm2.com)** 3.4.8

Note: You can clone or fork them freely, but I don't guarantee that they fit you.


## 🛠️ Installation

1. Downloads this repository
2. Deploy (i.e., *copy* or *create symlink*) dot files to your home directory; `make deploy`
> If dot files exist, they are moved to `$HOME/bak_dotfiles/`.
3. **Optional**: Run all programs for setup in `./etc/init/` directory; `make init`, when running the [installation command](#oneliner) specify `-s init` as an argument

Also, you can do that by running the installation command below.

| Tools | <a name="oneliner">The installation command</a> |
|:-:|:-:|
| cURL | bash -c "$(curl -fsSL raw.githubusercontent.com/naoking158/dotfiles/main/etc/install)" |
| Wget | bash -c "$(wget -qO - raw.githubusercontent.com/naoking158/dotfiles/main/etc/install)" |

When the [installation command](#oneliner) format is not `curl -L URL | sh` but `sh -c "$(curl -L URL)"`, shell will be restart automatically. If this is not the case, it is necessary to restart your shell manually.


## 🔃 Updating

```console
make update
```

It just run this command.
```console
git pull origin main
```


## ⚙️ Setup

By running this command, all configuration files stored within the `etc/init/` directory are executed.
```console
make init
```

To run `make init` immediately after running the [installation command](#oneliner):
```console
bash -c "$(curl -fsSL raw.githubusercontent.com/naoking158/dotfiles/main/etc/install)" -s init 
```

> Initialization of [homebrew](https://brew.sh) and [fisher](https://github.com/jorgebucaran/fisher) does not complete properly. This will be resolved in the future when [homebrew.sh](etc/init/homebrew.sh) and [fisher.sh](etc/init/fisher.sh) have the ability to set the path automatically.


## Components

- **etc/init/**: Configuration file storage to be executed initially for setup.


## 💡 Inspiration

- <https://github.com/b4b4r07/dotfiles>
- [最強の dotfiles 駆動開発と GitHub で管理する運用方法](https://qiita.com/b4b4r07/items/b70178e021bef12cd4a2#fnref2)

## 📸 Screen shots


### Emacs
#### GUI
![ 2021-12-13 at 10 50 40](https://user-images.githubusercontent.com/29372455/145740341-4789e63b-a39f-46c5-aaff-a36942b50c38.jpg)


#### CUI
![ 2021-12-13 at 10 49 22](https://user-images.githubusercontent.com/29372455/145740371-dae99e43-09bf-4a9a-bfde-2ef0e13e6f36.jpg)



## License

[MIT](LICENSE)
