#pdfDoc =function(){}

QQuote =
function(x)
    sprintf("'%s'", x)

convertPDF2XML =
    # Convert a PDF document to XML using stdout
function(file, out = character(), args = c("-q", "-xml"), pdftohtml = getOption("PDFTOHTML", Sys.getenv("PDFTOHTML", 'pdftohtml')))
{
      # -q - quiet
      # -xml - convert to xml
    # No -c with -stdout!!!
    if(length(out) == 0)
        args = c(args, "-stdout")

    if(grepl("\\.xml$", out))
        out = gsub("\\.xml$", "", out)
    
    cmd = sprintf("%s %s '%s' %s", pdftohtml, paste(args, collapse = " "), path.expand(file), if(length(out)) QQuote(out) else "")

    ans = system(cmd, intern = TRUE)
    if(length(out))
        return(paste0(out, ".xml"))

    doc = xmlParsePDFTOHTML(ans, asText = TRUE)
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
    class(doc) = c("PDFToXMLDoc", "ConvertedPDFDoc", class(doc))
    doc
}
