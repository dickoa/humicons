#' @importFrom utils tail
get_humiconList <- function(path) {
    l <- readLines(path)
    l <- l[grep("^.hi", l)]
    r <- gsub("\\:before \\{|\\.hi\\-", "", l)
    r[!grepl("\\{", r)]
}


icon_system_file <- function(file) {
  system.file(file, package = "humicon")
}
