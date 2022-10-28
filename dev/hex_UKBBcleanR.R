# ------------------------------------------------------------------------------ #
# Hexsticker for the GitHub Repository machiela-lab/UKBBcleanR
# ------------------------------------------------------------------------------ #
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: August 02, 2022
#
# Recently modified by: @idblr
# Recently modified on: October 28, 2022
#
# Notes:
# A) Uses the "hexSticker" package
# B) Subplot is one image designed by Iyi Kon via Vecteezy (THANK YOU TO THE CREATOR!) See below for attribution
# C) Hexsticker for the GitHub Repository https://github.com/machiela-lab/UKBBcleanR
# ------------------------------------------------------------------------------ #

# Packages
library(hexSticker)

# Image file
## Adaptation of one image with free license from Iyi Kon via Vecteezy 
### 1) "Sprayer Vector" under a free license via Vecteezy
#### Adaptations: Removed background color, added label, and custom characterization of the United Kingdom Union Jack Flag
#### Image: https://www.vecteezy.com/free-vector/spray-bottle-icon
#### Creator: https://www.vecteezy.com/members/iyikon

path_image <- "dev/hex_subplot.png"

# Create hexSticker
s <- hexSticker::sticker(subplot = path_image,
                         package = "",
                         p_size = 30, p_x = 1, p_y = 1.25, p_color = "black", # title
                         s_x = 1.05, s_y = 0.975, s_width = 0.85, s_height = 0.85, # symbol
                         h_fill = "#efefff", # inside
                         h_color = "#010066", # outline
                         dpi = 1000, # resolution
                         filename = "man/figures/UKBBcleanR.png",
                         white_around_sticker = FALSE)

# -------------------------------- END OF CODE --------------------------------- #
