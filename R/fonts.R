getTextFonts =
#
# Given a page or a document, get the font information for each text node.
#
function(page, fontInfo = getFontInfo(page),
          txtNodes = xpathSApply(page, xpathQ("//text", page)))
{
   
   fid = sapply(txtNodes, xmlGetAttr, "font")

   ans = fontInfo[fid, ]
   ans$text = sapply(txtNodes, xmlValue)
   ans
}

getFontInfo =
function(page,
         fonts = getNodeSet(page, xpathQ("//fontspec", page)))
{
   fids = sapply(fonts, xmlGetAttr, "id")
   df = do.call(rbind, lapply(fonts, function(x) xmlAttrs(x)[c("size", "family", "color")]))
   rownames(df) = fids
   df = as.data.frame(df, stringsAsFactors = FALSE)
   df$size = as.integer(df$size)
   df
}

xpathQ =
function(xpath, obj)
{
    if(inherits(obj, "XMLInternalElementNode"))
       paste0(".", xpath)
    else
       xpath
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
      # If we don't take into account the number of characters, but just the number of text nodes:
      #   sort(table(unlist(getNodeSet(doc, "//text/@font"))))
    txt = textByFonts(doc)
    ctr = sapply(txt, function(x) sum(nchar(x)))
    info = fontInfo(doc)
    id = names(ctr)[which.max(ctr)]
    info[info[,"id"] == id, ]
} 

