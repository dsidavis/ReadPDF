
getHeader =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    p1 = doc[[1]]
}

getPageFooter =
function(p,  bbox = getBBox2(getNodeSet(p, ".//text")), ignorePageNumber = TRUE)
{
    mx = max(bbox[, "top"], na.rm = TRUE)
    w = bbox[, "top"] == mx
    ans = rownames(bbox)[w][order(bbox[w, "left"])]

       # We have some docs with E57 as a page number (de la Torre-2009)
    if(length(ans) == 1 && (grepl("^[0-9]+$", ans) || grepl("Downloaded from", ans) || grepl("For +personal +use", ans) || (length(strsplit(ans, " +")[[1]]) == 1)))
        getPageFooter(, bbox[!w,])
    else
        paste(trim(ans), collapse = " ")
    
#    foot = bbox[ w , ]
     # Now combine the elements that are close
#    foot
}

getPageHeader =
function(p,  bbox = getBBox2(getNodeSet(p, ".//text")))
{
    mx = min(bbox[, "top"], na.rm = TRUE)
    w = bbox[, "top"] == mx
    rownames(bbox)[w]
}


lineSpacing =
function(doc)
{
   textNodes = getNodeSet(doc, "//text")
   bb = getBBox2(textNodes)
   
}


getFooterPos =
function(page, docFont = getDocFont(page), fontInfo = getFontInfo(page))
{
    ll = getNodeSet(page, ".//rect | .//line")
    bb = getBBox(ll)
    bottom = max(bb[, "y0"])
    # look for a line with all the text below it being smaller than the the document font.
    nodes = getNodeSet(page, sprintf(".//text[ @top > %f]", bottom))
    if(length(nodes)) {
        fontId = sapply(nodes, xmlGetAttr, "font")
        if(all(fontInfo[fontId, "size"] < docFont$size))
            return(bottom)
    }
    
    NA
}
