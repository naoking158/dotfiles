# Dotfiles

## Status

As of writing (2021-06-22) it works for me on my machine.
- **macOS** Big Sur 11.5.1 (intel)
- **Ubuntu** 18.04.5 LTS
- **[fish](https://github.com/fish-shell/fish-shell)** 3.3.1
- **[Tmux](https://github.com/tmux/tmux/wiki)** 3.2a
- **Emacs** 28.0.50
- **[iTerm2](https://iterm2.com)** 3.4.8
- **[Alacritty](https://github.com/alacritty/alacritty)** 0.9.0 (fed349a)

Note: You can clone or fork them freely, but I don't guarantee that they fit you.


## 🛠️ Installation

1. Downloads this repository
2. Deploy (i.e., *copy* or *create symlink*) dot files to your home directory; `make deploy`
> If dot files exist, they are copied to `$HOME/bak_dotfiles/` directory and this repository's dot files are merged to original one.
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

### terminal appearance (fish + tmux + tmux-powerline)
- Simple
- Show hostname, workspace
![ 2021-06-22 at 16 10 35](https://user-images.githubusercontent.com/29372455/122880004-7951c080-d374-11eb-90a1-6d97cfe6a9c2.png)


### Emacs
#### GUI
![ 2021-06-22 at 16 00 28](https://user-images.githubusercontent.com/29372455/122879165-8f12b600-d373-11eb-9a8e-3b7a1a004477.png)

#### CLI
![ 2021-06-22 at 15 55 39](https://user-images.githubusercontent.com/29372455/122879197-99cd4b00-d373-11eb-9eaa-e92e79fb76fb.png)


## License

[MIT](LICENSE)
