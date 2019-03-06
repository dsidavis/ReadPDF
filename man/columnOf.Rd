\name{columnOf}
\alias{columnOf}
\title{
Determine in which column in a page a node is located.
}
\description{

}
\usage{
columnOf(node, cols = getColPositions(xmlParent(node)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{the XML node whose column is to be identified}
\item{cols}{the positions of the columns on the page, i.e., where each
  column starts. These are calculated
  from the node's page. However it can be useful to specify the values
  either from contextual knowledge or by precomputing them to avoid
  repeating this calculation each time, or to customize the calculations
  of these positions.
 }
}
\value{
 An integer vector.
}
%\references{}
\author{
Duncan Temple Lang
}


\seealso{}
\examples{

doc = readPDFXML(system.file("samples/3Column.xml", package = "ReadPDF"))
library(XML)
getNodeSet(doc, "//text[contains(., 'Column')]")
nodes = getNodeSet(doc, "//text[contains(., 'Column')]")

sapply(nodes, columnOf)
}
\keyword{IO}% use one of  RShowDoc("KEYWORDS")

