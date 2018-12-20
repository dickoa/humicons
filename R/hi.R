html_dependency_humicons <- function() {
  htmltools::htmlDependency("humanitarian-icons", "2.0.0", src = icon_system_file("fonts/humanitarian-icons-2.0.0"),
      stylesheet = "css/humicons.min.css")
}

## Generate all functions for all icons
#' @rdname hi
#' @export
hi_iconList <- get_humiconList(with(html_dependency_humicons(), paste0(src$file, "/", stylesheet)))
#' Humicons alias
#'
#' @rdname hi-alias
#' @name hi-alias
#' @usage NULL
NULL

#' @evalRd paste("\\keyword{internal}", paste0('\\alias{hi_', gsub('-', '_', hi_iconList), '}'), collapse = '\n')
#' @name hi-alias
#' @rdname hi-alias
#' @exportPattern ^hi_
hi_constructor <- function(...) hi(name = name, ...)
for (icon in hi_iconList) {
  formals(hi_constructor)$name <- icon
  assign(paste0("hi_", gsub("-", "_", icon)), hi_constructor)
}
rm(hi_constructor)


#' Insert icon from OCHA humanitarian icons v2.0.0
#'
#' @param name A string indicating the icon name.
#' @param size Size of the icon relative to font size. Options are 1, lg (33%
#' increase), 2, 3, 4, or 5.
#' @param fixed_width If TRUE, the icon is set to a fixed width
#' @param animate 'still', 'spin', or 'pulse'.
#' @param rotate Rotate degree: 0, 90, 180, or 270.
#' @param flip 'none', 'horizontal', 'vertical'.
#' @param border If TRUE, draws a border around the icon.
#' @param pull Pulls icon to either 'left' or 'right' and wraps proceeding text
#' around it.
#' @param other Character vector of other parameters directly added to the icon classes
#' @param color,colour Hex code for a colour to be given to the icon
#'
#' @details `hi_*` is equivalent to `hi(name = *)`, which utilises the auto completion.
#' @references [Humantarian icons](https://thenounproject.com/ochavisual/collection/ocha-humanitarian-icons-v02/)
#' @export
#' @importFrom utils adist
hi <- function(name = "Livestock", size = 1, fixed_width = FALSE, animate = "still",
               rotate = 0, flip = "none", border = FALSE, pull = NULL, color = NULL, colour = color, other = NULL) {
    if(!(name %in% hi_iconList)) {
        stop(paste0("Icon '", name, "' not found in Humanitarian icons. Did you mean '", hi_iconList[which.min(adist(name, hi_iconList))], "'?"))
    }
    result <- structure(list(name = name, options = list(size = size, fixed_width = fixed_width,
                                                         animate = animate, rotate = rotate, flip = flip,
                                                         border = border, pull = pull, colour = colour,
                                                         other = other)), class = c("icon_hi", "icon"))
    out <- knitr::knit_print(result)
    class(out) <- c(class(out), "knit_icon")
    out
}


