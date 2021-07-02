#!/bin/bash

if [ "$(uname)" = "Darwin" ]; then

  printf "Disable automatic spelling correction\n"
  defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false

  printf "Set dns servers\n"
  networksetup -setdnsservers Wi-Fi 2001:4860:4860::8844 2001:4860:4860::8888 8.8.4.4 8.8.8.8

  printf "System - Disable software updates\n"
  sudo softwareupdate --schedule off

  printf "Finder - Show hidden files"
  defaults write com.apple.finder AppleShowAllFiles -bool true

  printf "Finder - Show all extensions"
  defaults write -g AppleShowAllExtensions -bool true

  printf "Finder - Show filename extensions\n"
  defaults write NSGlobalDomain AppleShowAllExtensions -bool true

  printf "Finder - Show path bar\n"
  defaults write com.apple.finder ShowPathbar -bool true

  printf "Finder - Show status bar\n"
  defaults write com.apple.finder ShowStatusBar -bool true

  printf "Finder - Show fullpath"
  defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

  printf "Increase key repeat rate\n"
  defaults write -g InitialKeyRepeat -int 12
  defaults write -g KeyRepeat -float 1.2

  killall Finder

  printf "Enable Trackpad Three Finger Drag\n"
  defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerDrag -bool true && \
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerDrag -bool true

  printf "delete Japanese file name of screen shot\n"
  defaults write com.apple.screencapture name ""

  printf "Don't make .DS_Store on network volume\n"
  defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

  printf "Disable window animation when you open/close window\n"
  defaults write -g NSAutomaticWindowAnimationsEnabled -bool false

  printf "Adjust window resize time\n"
  defaults write -g NSWindowResizeTime -float 0.001

  printf "Disable animations corresponding to finder\n"
  defaults write com.apple.finder DisableAllAnimations -bool true

  printf "Disable Quick Look animation\n"
  defaults write -g QLPanelAnimationDuration -float 0

  printf "Disable animation when launch app from dock\n"
  defaults write com.apple.dock launchanim -bool false

  printf "Disable shadow of screencapture\n"
  defaults write com.apple.screencapture disable-shadow -bool TRUE
  defaults write com.apple.screencapture type JPG
  killall SystemUIServer

  printf "Restart mac for applying modifications."

fi
