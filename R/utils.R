# function to remove ".0" from end of columns and remove "." from columns to elongate
colchange <- function(x) {
  colnames(x) <- gsub(".0$", "", colnames(x));x
  colnames(x) <- gsub("^f.4", "f4", colnames(x));x
}
