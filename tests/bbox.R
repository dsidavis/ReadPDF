library(ReadPDF)

doc = readPDFXML(system.file("samples", "Lahm-2007-Morbidity and mortality of wild anim.xml", package = "ReadPDF"))
bb = getTextBBox(doc[[2]])
stopifnot(all(c("pageNumber", "pageDimensions", "document") %in% names(attributes(bb))))
getPageHeight(bb)
getPageWidth(bb)


