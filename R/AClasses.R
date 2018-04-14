setOldClass(c("PDFToXMLDoc", "ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("PDFToXMLPage", "ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))


setAs("character", "PDFToXMLDoc", function(from) readPDFXML(from))
