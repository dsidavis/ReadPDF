\name{isCentered}
\alias{isCentered}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Is the node centered within the text on the page or column}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
isCentered(node, cols = getTextByCols(xmlParent(node), asNodes = TRUE), threshold = 0.2, colNum = inColumn(node, cols))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
%%     ~~Describe \code{node} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
}
  \item{colNum}{
%%     ~~Describe \code{colNum} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (node, cols = getTextByCols(xmlParent(node), asNodes = TRUE), 
    threshold = 0.2, colNum = inColumn(node, cols)) 
{
    if (length(cols) == 0) 
        return(FALSE)
    bb = getBBox2(cols[[colNum[1]]])
    byLine = by(bb, bb[, "top"], function(x) c(min(x[, "left"]), 
        max(x[, "left"] + x[, "width"])))
    byLine2 = do.call(rbind, byLine)
    pos = apply(byLine2, 2, median)
    mid = median(pos)
    top = xmlGetAttr(node, "top")
    lw = byLine[[top]]
    if (length(lw) && ((lw[1] - pos[1] < 5) || diff(pos) - diff(lw) < 
        40)) 
        return(FALSE)
    textPos = as.numeric(xmlAttrs(node)[c("left", "width")])
    textMid = textPos[1] + textPos[2]/2
    textPos[1] - pos[1] > 0.1 * diff(pos) & abs(textMid - mid) < 
        threshold * median(byLine2[, 2])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
