getXPathDocFontQuery =
function(node, docFont = TRUE, fontId = getDocFont(node)$id)
{
    xp = if(is.logical(docFont) && docFont)
            sprintf("//text[@font = '%s']", fontId)
         else if(is.data.frame(docFont) && nrow(docFont) > 0)
            sprintf("//text[@font = '%s']", docFont$id)
         else
            "//text"
    
    xpathQ(xp, node)
}

##########

getColPositions =
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE, ...)
    UseMethod("getColPositions")


getColPositions.character = 
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE, ...)    
   getColPositions(readPDFXML(p), threshold, docFont = docFont, ...)


getColPositions.PDFToXMLDoc = getColPositions.XMLInternalDocument =
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE,
         perPage = TRUE, ...
        )    
{

       # getPages() call here since could be called with an XMLInternalDocument, not a PDFToXMLDoc.
    ans = lapply(getPages(p), getColPositions, threshold, docFont = docFont, ...)
    if(is.logical(perPage) && perPage)
        return(ans)

    if(is.logical(perPage))
       perPage = .66

    tt = table(unlist(ans))/getNumPages(p)
    as.numeric(names(tt))[ tt >= perPage ]
}


getColPositions.PDFToXMLPage = getColPositions.XMLInternalNode =
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE, ...)    
{
    bbox = as.data.frame(bbox)

    tt = table(bbox$left)
    # Subtract 2 so that we start slightly to the left of the second column to include those starting points
    # or change cut to be include.lowest = TRUE
    ans = as.numeric(names(tt [ tt > nrow(bbox)*threshold])) - 2

    w = which(diff(ans) < 5)
    if(length(w))
       ans = ans[ - (w+1)]
    
    ans
}

#########

getNumCols =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
    UseMethod("getNumCols")

getNumCols.character =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
  getNumCols(  readPDFXML(p), threshold, txtNodes, bbox)


getNumCols.PDFToXMLPage = getNumCols.XMLInternalNode =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
    length(getColPositions(p, threshold, txtNodes, bbox))


getNumCols.PDFToXMLDoc =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
{    
    ncols = sapply(getPages(p), getNumCols, threshold, txtNodes, bbox)
    tt = table(ncols)
    as.integer(names(tt)[which.max(tt)])
}



hardCols =
function(page, nodes = getNodeSet(nodes, ".//text"),
         bbox = getBBox2(nodes, TRUE))
{

}

interNodeDist =
function(nodes, bbox = getBBox2(nodes, TRUE), asNA = TRUE)
{
    if(nrow(bbox) == 1)
       if(asNA) NA else 0
    else
      max(bbox[-1, 1] - bbox[-nrow(bbox),2])
}

if(FALSE)
getHorizExtremes =
function(node)
{
  
}



