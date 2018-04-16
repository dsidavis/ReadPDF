
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
    #' @param p an XML page node from the document
    #' @param bbox the
    #' @param nodes the list of XML nodes of interest in which to find possible header nodes. This allows us to filter the nodes, e.g., by font type/name/size, position.
    #' @lineThreshold integer vertical distance within which nodes are considered on the same line.
    #' interlineThreshold
function(p, bbox = getBBox2(nodes),
         nodes = getNodeSet(p, ".//text"),
         lineThreshold = 4, asNodes = FALSE,
         interlineThreshold = min(getDocFont(p)$size) * 2)
{
    mn = min(bbox[, "top"], na.rm = TRUE)
    w = bbox[, "top"] - mn <= lineThreshold
    #XXX Now check it is actually a header.
    # Find how far the nodes are from the other nodes not within the threshold
    # If this is sufficiently large (relative to the size of the text), then this is
    # a header.
    delta = min(bbox[!w, "top"] - mn)
    if(delta < interlineThreshold)
       return(if(asNodes) list() else character())
        
    if(asNodes)
       nodes[w]
    else
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
    if(length(ll)) {
        bb = getBBox(ll)
        bottom = max(bb[, "y0"])
        # look for a line with all the text below it being smaller than the the document font.
        nodes = getNodeSet(page, sprintf(".//text[ @top > %f]", bottom))
        if(length(nodes)) {
            fontId = sapply(nodes, xmlGetAttr, "font")
            if(all(fontInfo[fontId, "size"] < docFont$size))
                return(bottom)
        }
    }
    
    NA
}
