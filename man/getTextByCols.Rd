\name{getTextByCols}
\alias{getTextByCols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get the text arranged by each column.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getTextByCols(p, threshold = 0.1, asNodes = FALSE, txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)), bbox = getBBox2(txtNodes, TRUE), breaks = getColPositions(if (perPage) p else as(p, "XMLInternalDocument"), threshold = threshold, bbox = bbox, perPage = perPage, docFont = docFont, ...), perPage = FALSE, docFont = FALSE, order = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{asNodes}{
%%     ~~Describe \code{asNodes} here~~
}
  \item{txtNodes}{
%%     ~~Describe \code{txtNodes} here~~
}
  \item{bbox}{
%%     ~~Describe \code{bbox} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{perPage}{
%%     ~~Describe \code{perPage} here~~
}
  \item{docFont}{
%%     ~~Describe \code{docFont} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
}
\keyword{IO}
