# ------------------------------------------------------------------------------ #
# Hexsticker for the GitHub Repository machiela-lab/UKBBcleanR
# ------------------------------------------------------------------------------ #
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: August 02, 2022
#
# Recently modified by: 
# Recently modified on: 
#
# Notes:
# A) Uses the "hexSticker" package
# B) Subplot is ...
# C) Hexsticker for the GitHub Repository https://github.com/machiela-lab/UKBBcleanR
# ------------------------------------------------------------------------------ #

# Packages
library(hexSticker)

# Image file
## [INSERT NOTES ABOUT SUBPLOT, ANY ATTRIBUTION OR ALTERNATIONS]

path_image <- "man/figures/hex_subplot.png"

# Create hexSticker
s <- hexSticker::sticker(subplot = path_image,
                         package = "UKBBcleanR",
                         p_size = 30, p_x = 1, p_y = 1.25, p_color = "black", # title
                         s_x = 1, s_y = 1, s_width = 1, s_height = 1, # symbol
                         h_fill = "white", # inside
                         h_color = "black", # outline
                         dpi = 1000, # resolution
                         filename = "man/figures/UKBBcleanR.png",
                         white_around_sticker = F)s

# -------------------------------- END OF CODE --------------------------------- #
