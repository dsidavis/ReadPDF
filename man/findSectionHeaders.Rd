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
\arguments{
  \item{doc}{
}
  \item{sectionName}{
}
  \item{otherSectionNames}{
}
  \item{checkCentered}{a logical value. If the nodes we identify as section
       using the "expected" names  are centered, then by default
       when we look for other text with the same font, we only
       include centered text.  However, if checkCentered = FALSE
       we include all text with the same section header font.
       Checking for centered is currently expensive.
}
  \item{discardAfterReferences}{
}
  \item{allowRotated}{
}
  \item{onlyFirst}{
}
  \item{order}{
}
  \item{groupByLine}{
}
}
\value{
}
\references{
}
\author{
Duncan Temple Lang
}
\seealso{
}
\examples{
}
\keyword{IO}% use one of  RShowDoc("KEYWORDS")

