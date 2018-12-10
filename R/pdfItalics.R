getItalics =
    #
    # get the text that are in italics.
    #
function(doc = xmlParsePDFTOHTML("../../pdftohtml/examples/Italics.xml"), useHeuristics = TRUE, isItalicOnly = FALSE)
{
    if(is.character(doc))
       doc = xmlParsePDFTOHTML(doc)
    
    italics = getNodeSet(doc, "//fontspec[@isItalic = '1']")
    
    if(length(italics) == 0 && !useHeuristics)
        return(NULL)

    if(length(italics) == 0 || !isItalicOnly) {
        # So using heuristics, i.e. the name of the font rather than the flags in the PDF for the font.
       fonts = getNodeSet(doc, "//fontspec")
       names = sapply(fonts, xmlGetAttr, "name")
       w = grepl("IT?$|Italic|Itl|-i$|\\.I", names)
       italics = c(italics, fonts[w])
    }

    if(length(italics) == 0)
        return(NULL)

     # Go get the text for the nodes with these fonts.
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
