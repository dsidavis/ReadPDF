getColPositions =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
    UseMethod("getColPositions")

getColPositions.character = 
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))    
    getColPositions(readPDFXML(p), threshold)

getColPositions.PDFToXMLDoc = 
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))    
{
  lapply(p, getColPositions, threshold)
}


getColPositions.PDFToXMLPage = getColPositions.XMLInternalNode =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))    
{
    bbox = as.data.frame(bbox)

    tt = table(bbox$left)
    # Subtract 2 so that we start slightly to the left of the second column to include those starting points
    # or change cut to be include.lowest = TRUE
    as.numeric(names(tt [ tt > nrow(bbox)*threshold])) - 2
}

getNumCols =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
    UseMethod("getNumCols")

getNumCols.character =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
  getNumCols(  readPDFXML(p), threshold)

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
