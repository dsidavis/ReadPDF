getItalics =
    #
    # get the text that are in italics.
    #
function(doc = xmlParsePDFTOHTML("../../pdftohtml/examples/Italics.xml"), useHeuristics = TRUE)
{
    if(is.character(doc))
       doc = xmlParsePDFTOHTML(doc)
    
    italics = getNodeSet(doc, "//fontspec[@isItalic = '1']")
    if(length(italics) == 0 && !useHeuristics)
        return(NULL)

    if(length(italics) == 0) {
        # So using heuristics, i.e. the name of the font rather than the flags in the PDF for the font.
       fonts = getNodeSet(doc, "//fontspec")
       names = sapply(fonts, xmlGetAttr, "name")
       w = grepl("I$|Italic|Itl", names)
       italics = fonts[w]
    }

    if(length(italics) == 0)
        return(NULL)
    
    fid = sapply(italics, xmlGetAttr, "id")

    xpq = paste("//text[", paste( sprintf("@font = '%s'", fid), collapse = " or "), "]")
    getNodeSet(doc, xpq)
}


f = 
function(p, bbox = getBBox2(p), cols = getColPositions(p))
{
    kols = split(bbox, cut(bb[, "left"], c(0, cols, Inf) - 2))
#    lapply(kols, function(x))
}
