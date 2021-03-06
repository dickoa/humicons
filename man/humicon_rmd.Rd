% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa.R, R/humicon.R
\docType{data}
\name{hi_icon_list}
\alias{hi_icon_list}
\alias{humicon_rmd}
\title{Insert icon from OCHA humanitarian icons v2.0.0 in an Rmd document}
\format{An object of class \code{character} of length 295.}
\usage{
hi_icon_list

humicon_rmd(name = "Livestock", size = 1, fixed_width = FALSE,
  animate = "still", rotate = 0, flip = "none", border = FALSE,
  pull = NULL, color = NULL, colour = color, other = NULL)
}
\arguments{
\item{name}{A string indicating the icon name.}

\item{size}{Size of the icon relative to font size. Options are 1, lg (33%
increase), 2, 3, 4, or 5.}

\item{fixed_width}{If TRUE, the icon is set to a fixed width}

\item{animate}{'still', 'spin', or 'pulse'.}

\item{rotate}{Rotate degree: 0, 90, 180, or 270.}

\item{flip}{'none', 'horizontal', 'vertical'.}

\item{border}{If TRUE, draws a border around the icon.}

\item{pull}{Pulls icon to either 'left' or 'right' and wraps proceeding text
around it.}

\item{color}{Hex code for a colour to be given to the icon}

\item{colour}{Hex code for a colour to be given to the icon}

\item{other}{Character vector of other parameters directly added to the icon classes}
}
\description{
Insert icon from OCHA humanitarian icons v2.0.0 in an Rmd document
}
\details{
\code{humicon_rmd}
}
\references{
\href{https://thenounproject.com/ochavisual/collection/ocha-humanitarian-icons-v02/}{Humantarian icons}
}
\keyword{datasets}
