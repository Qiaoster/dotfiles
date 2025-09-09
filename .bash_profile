#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#if [ -z "$WAYLAND_DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
#    export XDG_CURRENT_SESSION=Hyprland
#    export XDG_SESSION_TYPE=wayland
#    export XDG_SESSION_DESKTOP=Hyprland
#    export GDK_BACKEND=wayland
#    export QT_QPA_PLATFORM=wayland
#    export SDL_VIDEODRIVER=wayland
#    export CLUTTER_BACKEND=wayland
#    export XCURSOR_SIZE=40
#    export QT_QPA_PLATFORMTHEME=qt6ct
#    exec Hyprland
#fi
exec Hyprland

# Created by `pipx` on 2025-08-08 12:33:09
export PATH="$PATH:/home/qiao/.local/bin"
