\name{showPage}
\alias{showPage}
\alias{plot}
\title{
Plot the contents of a PDF page
}
\description{
  This displays the elements of a PDF page where they appear on the page and mimic
  the elementary display of a PDF page. This is helpful for 
  visualizing where specific elements are.
}
\usage{
showPage(f, pageNum = 1, doc = xmlParse(f), page = getNodeSet(doc, "//page")[[pageNum]], ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
    the name of the file containing the PDF document or the converted
    PDF to XML,  
    or alternatively a PDF page extracted from an XML representation.
}
  \item{pageNum}{the index of the page in the file if \code{f} is a file name.
}
  \item{doc}{
%%     ~~Describe \code{doc} here~~
}
  \item{page}{
%%     ~~Describe \code{page} here~~
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (f, pageNum = 1, doc = xmlParse(f), page = getNodeSet(doc, 
    "//page")[[pageNum]], ...) 
{
    if (is(f, "XMLInternalDocument") && missing(doc)) 
        doc = f
    else if (missing(pageNum) && is(f, "XMLInternalElementNode") && 
        xmlName(f) == "page") 
        page = f
    renderPage(page, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
