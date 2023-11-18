#pdfDoc =function(){}

QQuote =
function(x)
    sprintf("'%s'", x)

convertPDF2XML =
    # Convert a PDF document to XML using stdout
function(file, out = character(), args = c("-q", "-xml"),
             pdftohtml = getOption("PDFTOHTML", Sys.getenv("PDFTOHTML", 'pdftohtml')))
{
      # -q - quiet
      # -xml - convert to xml
    # No -c with -stdout!!!
    if(length(out) == 0)
        args = c(args, "-stdout")
    else if(grepl("\\.xml$", out))
        out = gsub("\\.xml$", "", out)
    
#    cmd = sprintf("%s %s '%s' %s", pdftohtml, paste(args, collapse = " "), path.expand(file), if(length(out)) QQuote(out) else "")
    #    ans = system(cmd, intern = TRUE)
#    ans = system2(pdftohtml, c(args, shQuote(path.expand(file)), if(length(out)) shQuote(out) else character()))
    
    args2 = c(args, shQuote(file))
    if(length(out))
        args2 = c(args2, shQuote(out) )
    status = system2(pdftohtml, args2, stdout = TRUE)

    if(length(out))
        return(paste0(out, ".xml"))

    doc = xmlParsePDFTOHTML(status, asText = TRUE)
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
