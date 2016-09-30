pdfDoc =
function()
{

}


convertPDF2XML =
    # Convert a PDF document to XML using stdout
function(file, pdftohtml = getOption("PDFTOHTML", Sys.getenv("PDFTOHTML", 'pdftohtml')))
{
      # -q - quiet
      # -xml - convert to xml
      # No -c with -stdout!!!
    cmd = sprintf("%s -q -xml -stdout '%s'", pdftohtml, path.expand(file))

    out = system(cmd, intern = TRUE)
    
    doc = xmlParsePDFTOHTML(out, asText = TRUE)
    docName(doc) = file
    
    doc
}

readPDFXML = xmlParsePDFTOHTML =
    # Parse the pdftohtml document
function(file, asText = FALSE, ...)
{
    if(!asText && grepl("\\.pdf$", file))
       return( convertPDF2XML(file, ...))
    
    doc = xmlParse(file, ...)
    class(doc) = c("PDFToHTMLDoc", "ConvertedPDFDoc", class(doc))
    doc
}
