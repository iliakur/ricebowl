set -e

# Use both shifts to cycle through layouts.
# Swap left control, alt, super for better emacs experience.
# Capslock key is swapped with Esc for better vim experience.
setxkbmap \
    -layout us,ru,us \
    -variant colemak,, \
    -option grp:shifts_toggle,ctrl:swap_lalt_lctl_lwin,caps:swapescape
