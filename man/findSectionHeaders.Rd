\name{findSectionHeaders}
\alias{findSectionHeaders}
\title{
  Find the XML nodes corresponding to the section titles
  of the article/document.
}
\description{
This uses a heuristic approach to find the
}
\usage{
findSectionHeaders(doc, sectionName = c("introduction", "background", "conclusions", "discussion", "materials and methods", "literature cited", "references cited", "the study"), otherSectionNames = c("references", "acknowledgements", "acknowledgments", "results", "methods"), checkCentered = TRUE, discardAfterReferences = TRUE, allowRotated = FALSE, onlyFirst = FALSE, order = TRUE, groupByLine = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{doc}{
%%     ~~Describe \code{doc} here~~
}
  \item{sectionName}{
%%     ~~Describe \code{sectionName} here~~
}
  \item{otherSectionNames}{
%%     ~~Describe \code{otherSectionNames} here~~
}
  \item{checkCentered}{a logical value. If the nodes we identify as section
       using the "expected" names  are centered, then by default
       when we look for other text with the same font, we only
       include centered text.  However, if checkCentered = FALSE
       we include all text with the same section header font.
       Checking for centered is currently expensive.%%     ~~Describe \code{checkCentered} here~~
}
  \item{discardAfterReferences}{
%%     ~~Describe \code{discardAfterReferences} here~~
}
  \item{allowRotated}{
%%     ~~Describe \code{allowRotated} here~~
}
  \item{onlyFirst}{
%%     ~~Describe \code{onlyFirst} here~~
}
  \item{order}{
%%     ~~Describe \code{order} here~~
}
  \item{groupByLine}{
%%     ~~Describe \code{groupByLine} here~~
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
function (doc, sectionName = c("introduction", "background", 
    "conclusions", "discussion", "materials and methods", "literature cited", 
    "references cited", "the study"), otherSectionNames = c("references", 
    "acknowledgements", "acknowledgments", "results", "methods"), 
    checkCentered = TRUE, discardAfterReferences = TRUE, allowRotated = FALSE, 
    onlyFirst = FALSE, order = TRUE, groupByLine = FALSE) 
{
    if (is.character(doc)) 
        doc = readPDFXML(doc)
    hasNum = FALSE
    filter = paste(sprintf("(contains(lower-case(normalize-space(.)), '\%s') and isNum(normalize-space(.)))", 
        sectionName), collapse = " or ")
    xp = sprintf("//text[\%s]", filter)
    intro = getNodeSet(doc, xp, xpathFuns = list(isNum = isSectionNum))
    hasNum = length(intro) > 0
    if (!hasNum) {
        filter = paste(sprintf("lower-case(normalize-space(.)) = '\%s'", 
            sectionName), collapse = " or ")
        xp = sprintf("//text[\%s]", filter)
        intro = getNodeSet(doc, xp)
    }
    if (onlyFirst) 
        return(intro)
    if (!length(intro)) {
        filter = paste(sprintf("lower-case(normalize-space(.)) = '\%s'", 
            otherSectionNames), collapse = " or ")
        xp = sprintf("//text[\%s]", filter)
        intro = getNodeSet(doc, xp)
    }
    if (length(intro)) 
        intro = intro[!isLowerCase(sapply(intro, xmlValue))]
    if (length(intro)) {
        if (hasNum) 
            return(getNodeSet(doc, "//text[isNum(normalize-space(.))]", 
                xpathFuns = list(isNum = isSectionNum)))
        i = sapply(intro, function(x) length(getNodeSet(x, "./preceding::text[ . = 'Supporting Online Material']"))) > 
            0
        intro = intro[!i]
        if (length(intro) == 0) 
            return(NULL)
        fontID = unique(sapply(intro, xmlGetAttr, "font"))
        secs = getNodesWithFont(doc, fontID = fontID)
        secs = secs[!grepl("\\.{5,}[[:space:]]*[0-9]+$", xmlValue(secs))]
        if (!allowRotated) 
            secs = secs[as.numeric(sapply(secs, xmlGetAttr, "rotation")) == 
                0]
        secs = secs[!grepl("^[0-9]+$", xmlValue(secs))]
        preRefs = sapply(secs, function(x) getNodeSet(x, ".//preceding::text[ lower-case(normalize-space(.)) = 'references' or lower-case(normalize-space(.)) = 'references cited' or lower-case(normalize-space(.)) = 'supporting online material']"))
        w = sapply(preRefs, length) > 0
        if (any(w) && hasCoverPage(doc)) 
            w[w] = sapply(preRefs[w], function(x) pageOf(x) != 
                1)
        secs = secs[!w]
        if (all(isUpperCase(sapply(intro, xmlValue)))) {
            txt = sapply(secs, xmlValue)
            secs = secs[i <- isUpperCase(txt)]
            secs = secs[!grepl("^[[:punct:]]+$", txt[i])]
        }
        if (checkCentered) 
            secs = secs[sapply(secs, isCentered) == isCentered(intro[[1]])]
        if (isOnLineBySelf(intro[[1]])) {
            i = sapply(secs, isOnLineBySelf)
            secs = secs[i]
        }
        if (order) {
            o = order(sapply(secs, pageOf), sapply(secs, inColumn))
            secs = secs[o]
        }
        if (groupByLine) {
            secs = nodesByLine(secs)
        }
        secs
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
