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
function(doc, font)
{
   getNodeSet(doc, sprintf("//text[@font = '%s']", font))
}
