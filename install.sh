#!/bin/bash
trap "exit" INT

# echo "Updating packages"
# sudo dnf upgrade

if dnf repolist | grep -q brave-browser; then
  echo "Brave browser is installed - skipping adding repo for install"
else
  echo "Adding repos for installing brave browser"
  sudo dnf install dnf-plugins-core
  sudo dnf config-manager --add-repo https://brave-browser-rpm-release.s3.brave.com/x86_64/
  sudo rpm --import https://brave-browser-rpm-release.s3.brave.com/brave-core.asc
fi

if dnf repolist | grep -q lazygit; then
  echo "Lazygit is installed - skipping adding repo for install"
else
  echo "Adding repos for installing lazygit"
  sudo dnf copr enable atim/lazygit -y
fi

if dnf repolist | grep -q docker-ce; then
  echo "Docker is installed - skipping adding repos for install"
else
  echo "Adding repos for installing docker"
  sudo dnf config-manager \
    --add-repo \
    https://download.docker.com/linux/fedora/docker-ce.repo
fi

echo "Installing packages"
sudo dnf install alacritty neovim nitrogen thunar polybar stow zsh ripgrep xclip lazygit brave-browser stack solaar docker-ce docker-ce-cli containerd.io docker-compose-plugin syncthing keepassxc qutebrowser mpv ranger w3m kitty

if grep -q docker /etc/group; then
  echo "User already added to docker group"
else
  echo "Adding user to docker group"
  sudo groupadd docker && sudo usermod -aG docker $USER
fi

if [ -f "$HOME/.local/share/pnpm/pnpm" ]; then
  echo "Pnpm is installed - skipping install"
else
  echo "Installing pnpm"
  curl -fsSL https://get.pnpm.io/install.sh | sh -
fi

if [ -d "$HOME/.nvm/.git" ];
then
  echo "Nvm is installed - skipping install"
else
  echo "Installing nvm"
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash
fi

if [ -d "$HOME/.oh-my-zsh/.git" ];
then
  echo "Omz is installed - skipping install"
else
  echo "Installing omz"
  $(sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)")
fi

echo "Creating symlinks"
for dir in */; do
  echo "$dir"
  stow $dir
done
