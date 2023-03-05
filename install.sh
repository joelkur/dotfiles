#!/bin/bash

function section() {
  echo
  echo
  echo $1
}

function conditional() {
  ($1 && echo "${3:Already installed}") || ($2)
}

function main() {
  section "Updating"
  sudo dnf upgrade
  
  section "Adding package repositories"

  conditional "which brave-browser" $(
    # Brave browser repos
    sudo dnf install dnf-plugins-core
    sudo dnf config-manager --add-repo https://brave-browser-rpm-release.s3.brave.com/x86_64/
    sudo rpm --import https://brave-browser-rpm-release.s3.brave.com/brave-core.asc
  ) "Brave browser has already been installed"
  
  conditional "which lazygit" "sudo dnf copr enable atim/lazygit -y" "Lazygit has already been installed"
  
  conditional "which docker" $(
      sudo dnf config-manager \
        --add-repo \
        https://download.docker.com/linux/fedora/docker-ce.repo
    ) "Docker has already been installed"
  
  section "Installing packages"
  # Install packages
  sudo dnf install alacritty neovim nitrogen thunar polybar stow zsh ripgrep xclip lazygit brave-browser stack ghc solaar docker-ce docker-ce-cli containerd.io docker-compose-plugin syncthing keepassxc
  
  section "Adding user to docker group"
  conditional "grep docker /etc/group" "sudo groupadd docker && sudo usermod -aG docker $USER"
  
  section "Installing pnpm"
  conditional "pnpm --version" "curl -fsSL https://get.pnpm.io/install.sh | sh -"
  
  # Create symlinks
  echo "Creating symlinks..."
  for dir in */; do
    echo "$dir"
    stow $dir
  done
  
  # Programming languages
  section "Installing nvm"
  conditional "nvm --version" "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.2/install.sh | bash"
  
  section "Installing rust"
  conditinal "cargo --version" "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
  
  # Install oh-my-zsh
  section "Installing omz"
  conditional "which omz" $(sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)")
}

main
