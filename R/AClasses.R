setOldClass(c("PDFToXMLDoc", "ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("PDFToXMLPage", "ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))


setAs("character", "PDFToXMLDoc", function(from) readPDFXML(from))


setAs("XMLInternalNode", "PDFToXMLPage",
      function(from) {
          if(xmlName(from) != 'page') 
              stop("XML node is not a <page>")

          class(from) = c("PDFToXMLPage", "ConvertedPDFPage", class(from))
          from
      })
