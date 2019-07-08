\name{getBBox}
\alias{getBBox}
\title{Get the bounding box of a collection of nodes}
\description{
  This function computes the top-left location and the width and height
  of each node to give its locations.
}
\usage{
getBBox(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE)
}
\arguments{
  \item{nodes}{}
  \item{asDataFrame}{}
  \item{color}{}
  \item{diffs}{}
  \item{dropCropMarks}{}
}
\details{
}
\value{
  A matrix or a data.frame depending on \code{asDataFrame}.
  The difference between the matrix and the data.frame is that
  the matrix does not contain the text in a column, but as row names;
  the data.frame contains a column named \code{text} giving the text.
  (Adding a column of text to the matrix would cause all columns to be
  character strings.)

  If \code{diffs} is \code{TRUE}, the standard/always-present columns are left, top, width and height.
  Recall that the values increase from top down as the origin of a PDF
  page is the top-left corner.

  If \code{diffs} is \code{FALSE}, the columns give the top-left and
  bottom right locations as x0, y0, x1, y1. Here bottom means down the
  page  so a larger y value. So y1 is greater than y0.

  If \code{color} is \code{TRUE}, the color of each node is added as a
  column in the data.frame (not if returning a matrix).
}
\author{
  Duncan Temple Lang
}
\note{
}


\seealso{
}
\examples{
}
\keyword{IO}

