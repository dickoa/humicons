#' @importFrom utils tail
get_humiconList <- function(path) {
    l <- readLines(path)
    l <- l[grep("^.hi", l)]
    r <- gsub("\\:before \\{|\\.hi\\-", "", l)
    r[!grepl("\\{", r)]
}

icon_system_file <- function(file) {
  system.file(file, package = "humicons")
}

humicon <- function(name) {
    df <- humicons_data
    icon_list <- df$name
    bool <- tolower(name) %in% tolower(icon_list)
    if(!bool) {
        stop(paste0("Icon '", name, "' not found in OCHA Humaniarian Icons v02. Did you mean '", icon_list[which.min(adist(name, icon_list))], "'?"))
    }
    code <- df$code[tolower(df$name) == tolower(name)]
    intToUtf8(strtoi(code, 16L))
}


#' Get OCHA Humanitarian icons v02 unicode
#' 
#' Get you the unicode to use in base graphic or ggplot2
#' @param names character name of the icon see humicons_data
#' @examples
#' humicons(c("Urban", "Car"))
#' 
#' @export
humicons <- function(names) {
    vapply(names, humicon, character(1))
}
