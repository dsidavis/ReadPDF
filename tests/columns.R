library(ReadPDF)
f = system.file("samples","2Column.xml", package = "ReadPDF")
doc = xmlParsePDFTOHTML(f)

# Deal with the second page.
p = doc[[2]]
renderPage(p, cex = .5)

cols = getTextByCols(p)
nchar(cols)
