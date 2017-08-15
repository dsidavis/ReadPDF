getXPathDocFontQuery =
function(node, docFont = TRUE, fontId = getDocFont(node)$id)
{
    xp = if(docFont)
            sprintf("//text[@font = '%s']", fontId)
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
   getColPositions(readPDFXML(p), threshold)


getColPositions.PDFToXMLDoc = getColPositions.XMLInternalDocument =
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE,
         perPage = TRUE, ...
        )    
{
       # getPages() call here since could be called with an XMLInternalDocument, not a PDFToXMLDoc.
    ans = lapply(getPages(p), getColPositions, threshold)
    if(perPage)
        return(ans)

    tt = table(unlist(ans))/6
    as.integer(names(tt))[ tt >= .666]
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





getHorizExtremes =
function(node)
{
  
}
