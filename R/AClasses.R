
#setOldClass(c("PDFToXMLDoc", "ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument", "Document"))
#setOldClass(c("PDFToXMLPage", "ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))


if(TRUE) {
setOldClass(c("ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))    
setClass("PDFToXMLDoc", contains = c("ConvertedPDFDoc", "Document"))
setClass("PDFToXMLPage", contains = c("ConvertedPDFPage", "DocumentPage"))


setOldClass(c("PDFTextBoundingBox", "TextBoundingBox", "BoundingBox"))
} 

setAs("character", "PDFToXMLDoc", function(from) readPDFXML(from))


setAs("XMLInternalNode", "PDFToXMLPage",
      function(from) {
          if(xmlName(from) != 'page') 
              stop("XML node is not a <page>")

          class(from) = c("PDFToXMLPage", "ConvertedPDFPage", "DocumentPage", class(from))
          from
      })
