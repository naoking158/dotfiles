# Dotfiles

## Status

As of writing (2021-08-11) it works for me on my machine.
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

- <https://github.com/daviwil/dotfiles>
- <https://github.com/b4b4r07/dotfiles>
- [最強の dotfiles 駆動開発と GitHub で管理する運用方法](https://qiita.com/b4b4r07/items/b70178e021bef12cd4a2#fnref2)

## 📸 Screen shots

### terminal appearance (alacritty + tmux + fish)
- Simple
- Show hostname, workspace
<img width="1478" alt=" 2021-08-11 at 18 29 26" src="https://user-images.githubusercontent.com/29372455/129005501-8ce85155-41b4-400b-9487-9cf138ab5521.png">



### Emacs

- My Emacs is configured with `init.el` and `early-init.el`.
- `init.el` is exported by `org-babel-tangle` from [`~/.emacs.d/Emacs.org`](.emacs.d/Emacs.org).

#### GUI
<img width="1136" alt=" 2021-08-11 at 18 25 05" src="https://user-images.githubusercontent.com/29372455/129005605-1d7b59b9-5f1b-49cc-af0e-d11c6c25efb0.png">
<img width="1136" alt=" 2021-08-11 at 17 45 48" src="https://user-images.githubusercontent.com/29372455/129005645-1ba4f05e-c9f7-4907-a5ca-f7e7650187a5.png">
<img width="1136" alt=" 2021-08-11 at 17 48 52" src="https://user-images.githubusercontent.com/29372455/129005679-5f1de101-d995-4606-9a2e-2a6c1927769c.png">


#### CLI
Depending on your environment, you may need to start Emacs as follows to display the colors correctly:
```console
TERM=xterm-direct emacs -nw
```
- reference: https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html

<img width="1136" alt=" 2021-08-11 at 18 22 21" src="https://user-images.githubusercontent.com/29372455/129005753-078085ac-7cc5-4fc8-a5eb-162514ddf9b6.png">
<img width="1136" alt=" 2021-08-11 at 17 51 14" src="https://user-images.githubusercontent.com/29372455/129005791-83ba219c-7d78-43ba-8fe6-a9075b9b46a7.png">
<img width="1136" alt=" 2021-08-11 at 17 50 52" src="https://user-images.githubusercontent.com/29372455/129005841-b28c39e9-35de-474d-b9e7-5cad3e449e27.png">


## License

[MIT](LICENSE)
