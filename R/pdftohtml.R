
convertPDF2XML =
    # Convert a PDF document to XML using stdout
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
    # Parse the pdftohtml document
function(file, ...)
{
    doc = xmlParse(file, ...)
    class(doc) = c("PDFToHTMLDoc", "ConvertedPDFDoc", class(doc))
    doc
}
