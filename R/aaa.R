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

#' Get OCHA Humanitarian icons v02 unicode
#' 
#' Get you the unicode to use in base graphic or ggplot2
#' @param name character name of the icon see humicons_data
#' @examples
#' get_humicon("Urban")
#' 
#' @export
#'
get_humicon <- function(name) {
    df <- humicons_data
    icon_list <- df$name
    if(!(name %in% icon_list)) {
        stop(paste0("Icon '", name, "' not found in OCHA Humaniarian Icons v02. Did you mean '", icon_list[which.min(adist(name, icon_list))], "'?"))
    }
    intToUtf8(strtoi(df$code[df$name == name], 16L))
}
