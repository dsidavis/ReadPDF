
convertPDF2XML =
function(file, pdftohtml = getOption("PDFTOHTML", Sys.getenv("PDFTOHTML", 'pdftohtml')))
{
      # -q - quiet
      # -xml - convert to xml
      # No -c with -stdout!!!
    cmd = sprintf("%s -q -xml -stdout %s", pdftohtml, file)

    out = system(cmd, intern = TRUE)
    
    doc = xmlParsePDFTOTHML(out, asText = TRUE)
    docName(doc) = file
    
    doc
}

xmlParsePDFTOTHML =
function(file, ...)
{
    doc = xmlParse(file, ...)
    class(doc) = c("PDFToHTMLDoc", "ConvertedPDFDoc", class(doc))
    doc
}
