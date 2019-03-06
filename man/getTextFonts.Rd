\name{getTextFonts}
\alias{getTextFonts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get information about the fonts used for each text node.
}
\description{
}
\usage{
getTextFonts(page, fontInfo = getFontInfo(page), txtNodes = xpathSApply(page, xpathQ("//text", page)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{page}{an XML node corresponding to a page.}
  \item{fontInfo}{font information from the document}
  \item{txtNodes}{the text nodes for which we want the font information.}
}
\details{
}
\value{
  A data.frame with elements
  \item{id}{}
  \item{size}{}
  \item{family}{}
  \item{color}{}
  \item{isItalic}{}
  \item{isBold}{}
  \item{isOblique}{}
  \item{name}{}
  \item{text}{}  
}
\references{
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
f = system.file("samples/2Column.xml", package = "ReadPDF")
doc = readPDFXML(f)
f = getTextFonts(doc[[1]])
}
\keyword{IO}
