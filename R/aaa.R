#' @importFrom utils tail
get_humicon_list <- function(path) {
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
hi_icon_list <- get_humicon_list(with(html_dependency_humicons(), paste0(src$file, "/", stylesheet)))

#' @noRd
font_style <- function(x) {
  out <- NULL
  if(!is.null(x$options$colour)) {
    out <- paste0(out, "color:", x$options$colour, ";")
  }
  out
}
