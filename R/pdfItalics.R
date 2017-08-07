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

fontInfo =
function(doc)
{
   df = as.data.frame(t(xpathSApply(doc, "//fontspec", xmlAttrs)), stringsAsFactors = FALSE)
   iids = c("size" = 'integer',  isItalic = 'logical', isBold = 'logical', isOblique = 'logical')

   df[names(iids)] = mapply(function(var, to)
                              as(as.integer(df[[var]]), to),
                            names(iids), iids, SIMPLIFY = FALSE)
   df
}



textByFont =
    # Get all the text nodes for a single font identifier
function(doc, font)
{
   getNodeSet(doc, sprintf("//text[@font = '%s']", font))
}


textByFonts =
    # Get all the text strings for each font in the document.
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    fontIds = unlist(getNodeSet(doc, "//fontspec/@id"))
    txt = lapply(fontIds, function(id) sapply(textByFont(doc, id), xmlValue))
    names(txt) = fontIds
    txt
}

getDocFont = getTextFont =
    #
    # Get the font information for the most commonly used font in the document,
    # which is assumed to be that of the text.
    # There are cases in which the most common font may not be that of the text.
    # 
function(doc)
{
    txt = textByFonts(doc)
    ctr = sapply(txt, function(x) sum(nchar(x)))
    info = fontInfo(doc)
    id = names(ctr)[which.max(ctr)]
    info[id, ]
} 


f = 
function(p, bbox = getBBox2(p), cols = getColPositions(p))
{
    kols = split(bbox, cut(bb[, "left"], c(0, cols, Inf) - 2))
#    lapply(kols, function(x))
}
