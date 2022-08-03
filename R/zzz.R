.onAttach <- function(...) {
  packageStartupMessage(paste("\nWelcome to {UKBBcleanR} version ", utils::packageDescription("UKBBcleanR")$Version, "\n> help(\"UKBBcleanR\") # for documentation\n> citation(\"UKBBcleanR\") # for how to cite\n", sep = ""), appendLF = TRUE)
}
