#' Insert icon from OCHA humanitarian icons v2.0.0 in a Shiny app
#'
#' Create an icon for use within a page in a Shiny app. Icons can appear on their own, inside
#' of a button, or as an icon for a \code{\link{tabPanel}} within a
#' \code{\link{navbarPage}}.
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
#' @return An Shiny icon element
#'
#' @examples
#' # add an icon to a submit button
#' submitButton("Update View", icon = icon("Fund"))
#'
#' navbarPage("App Title",
#'   tabPanel("Plot", icon = humicon("Food-Security")),
#'   tabPanel("Summary", icon = humicon("Agriculture")),
#'   tabPanel("Table", icon = humicon("Livestock"))
#' )
#' @export
humicon <- function(name, size = 1, fixed_width = FALSE,
                          animate = "still", rotate = 0, flip = "none",
                          border = FALSE, pull = NULL, color = NULL,
                          colour = color, other = NULL, class = NULL) {
    check_icon_name(name)
    name <- humicons_data$name[tolower(humicons_data$name) == tolower(name)]
    x <- structure(list(name = name, options = list(size = size, fixed_width = fixed_width,
                                                    animate = animate, rotate = rotate, flip = flip,
                                                    border = border, pull = pull, colour = colour,
                                                    other = other)), class = c("icon_hi", "icon"))
    icon_class <- cat_icon(x)
    if (!is.null(class))
        icon_class <- paste(icon_class, class)
    icon <- htmltools::tags$i(class = cat_icon(x), style = font_style(x))
    htmltools::htmlDependencies(icon) <- html_dependency_humicons()
    htmltools::browsable(icon)
}

#' Insert icon from OCHA humanitarian icons v2.0.0 in an Rmd document
#
#' @inheritParams humicon
#'
#' @details `humicon_rmd`
#' @references [Humantarian icons](https://thenounproject.com/ochavisual/collection/ocha-humanitarian-icons-v02/)
#' @rdname humicon_rmd
#' @export
#' @importFrom utils adist
humicon_rmd <- function(name = "Livestock", size = 1, fixed_width = FALSE, animate = "still",
                        rotate = 0, flip = "none", border = FALSE, pull = NULL, color = NULL, colour = color, other = NULL) {
    check_icon_name(name)
    name <- humicons_data$name[tolower(humicons_data$name) == tolower(name)]
    result <- structure(list(name = name, options = list(size = size, fixed_width = fixed_width,
                                                         animate = animate, rotate = rotate, flip = flip,
                                                         border = border, pull = pull, colour = colour,
                                                         other = other)), class = c("icon_hi", "icon"))
    out <- knitr::knit_print(result)
    class(out) <- c(class(out), "knit_icon")
    out
}

#' @noRd
humicon_chr_ <- function(name) {
    check_icon_name(name)
    code <- humicons_data$code[tolower(humicons_data$name) == tolower(name)]
    intToUtf8(strtoi(code, 16L))
}

#' Get OCHA Humanitarian icons v02 unicode in character
#'
#' Get you the unicode to use in base graphic or ggplot2
#' @param names character name of the icon see humicons_data
#' @examples
#' humicon_chr(c("Urban", "Car"))
#'
#' @export
humicon_chr <- function(names) {
    vapply(names, humicon_chr_, character(1))
}
