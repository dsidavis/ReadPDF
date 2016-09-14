
getItalics =
function(doc = xmlParsePDFTOTHML("../../pdftohtml/examples/Italics.xml"))
{    
    italics = getNodeSet(doc, "//fontspec[@isItalic = '1']")
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
