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


check_icon_name <- function(name) {
    df <- humicons_data
    icon_list <- df$name
    bool <- tolower(name) %in% tolower(icon_list)
    if(!bool) {
        stop(paste0("Icon '", name, "' not found in OCHA Humaniarian Icons v02. Did you mean '", icon_list[which.min(adist(name, icon_list))], "'?"))
    }
}

html_dependency_humicons <- function() {
  htmltools::htmlDependency("humanitarian-icons", "2.0.0", src = icon_system_file("fonts/humanitarian-icons-2.0.0"),
      stylesheet = "css/humicons.min.css")
}

## Generate all functions for all icons
#' @rdname humicon_rmd
#' @export
hi_iconList <- get_humiconList(with(html_dependency_humicons(), paste0(src$file, "/", stylesheet)))

#' Humicons alias
#'
#' @rdname humicon_rmd-alias
#' @name humicon_rmd-alias
#' @usage NULL
NULL
