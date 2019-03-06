\name{isScanned}
\alias{isScanned}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Is the document scanned or a contain real PDF elements}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
isScanned(doc, checkFuns = NULL, textNodeThreshold = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{
%%     ~~Describe \code{doc} here~~
}
  \item{checkFuns}{
%%     ~~Describe \code{checkFuns} here~~
}
  \item{textNodeThreshold}{
%%     ~~Describe \code{textNodeThreshold} here~~
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
function (doc, checkFuns = NULL, textNodeThreshold = 10) 
{
    if (is.character(doc)) 
        doc = xmlParse(doc)
    nodes = getNodeSet(doc, "//page/*[not(local-name(.) = 'img') and not(local-name(.) = 'fontspec') and not(local-name(.) = 'rect') and not(local-name(.) = 'line')]")
    if (length(nodes) == 0) 
        return(c(NoText = TRUE))
    isTropicalMedHyg = length(getNodeSet(doc, "//text[ contains(., 'The American Society of Tropical Medicine and Hygiene')]")) > 
        0
    if (isTropicalMedHyg && sum(sapply(nodes, xmlName) == "text") > 
        30) 
        return(FALSE)
    pg = getNodeSet(doc, "//page")
    textWords = sapply(pg, isScannedPage, textNodeThreshold)
    if (!any(textWords)) 
        return(FALSE)
    pageHasImg = sapply(pg, function(x) "img" \%in\% names(x))
    if (all(textWords) && all(pageHasImg)) 
        return(TRUE)
    txt = lapply(pg, function(x) unique(getPageText(x)))
    if (length(unique(unlist(txt))) == 1) 
        return(SameTextOnAllPages = TRUE)
    if (!is.null(checkFuns)) {
        if (is.function(checkFuns)) 
            ans = checkFuns(doc)
        else for (f in checkFuns) {
            ans = f(doc)
            if (ans) 
                break
        }
        if (ans) 
            return(ans)
    }
    if (length(pg) > 2 && length(unique(txt[-1])) == 1) 
        return(SameTextOnAllPagesExceptFirst = TRUE)
    img = getNodeSet(doc, "//page/img")
    names(img) = xpathSApply(doc, "//page/img", function(x) xmlGetAttr(xmlParent(x), 
        "number"))
    if (length(pg) > 0 && (all(pageHasImg) || (length(pg) > 2) && 
        all(pageHasImg[-c(1, length(pg))]))) {
        y = sapply(img, imgSpansPage)
        if (all(y)) 
            return(ImagesSpanAllPages = TRUE)
        byPage = tapply(y, names(img), any)
        if (length(byPage) > 2 && all(byPage[-c(1, length(byPage))])) 
            return(c(TwoCoverPages = TRUE))
        if (length(pg) == 2 && any(grepl("ResearchGate|Downloaded|JSTOR", 
            txt[[1]])) && textWords[2] & byPage[2]) 
            return(OneCoverPage = TRUE)
    }
    FALSE
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
