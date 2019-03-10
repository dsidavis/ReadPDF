getXPathDocFontQuery =
function(node, docFont = TRUE, fontId = getDocFont(node, local = local)$id, local = FALSE)
{
    xp = if(is.logical(docFont) && !is.na(docFont) && docFont)
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
    #
    # For a file name
    #
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE, ...)    
   getColPositions(readPDFXML(p), threshold, docFont = docFont, ...)


getColPositions.PDFToXMLDoc = getColPositions.XMLInternalDocument =
    #
    # For a document of probably multiple pages.
    #
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont)),
         bbox = getBBox2(txtNodes), docFont = TRUE,
         perPage = TRUE,
         acrossPages = FALSE, ...
        )    
{
    # getPages() call here since could be called with an XMLInternalDocument, not a PDFToXMLDoc.
    pages = getPages(p)
    ans = lapply(pages, getColPositions, threshold, docFont = docFont, ...)
    if(is.logical(perPage) && perPage && !acrossPages)
        return(ans)

    if(acrossPages)
       return(collapsePageCols(ans, length(pages)))

# The following is the same intent as collapsePageCols    
    if(is.logical(perPage))
       perPage = .66

    tt = table(unlist(ans))/length(pages)
    as.numeric(names(tt))[ tt >= perPage ]
}


getColPositions.PDFToXMLPage = getColPositions.XMLInternalNode =  getColPositions.XMLInternalElementNode =
    # For a single page
function(p, threshold = .1,
         txtNodes = getNodeSet(p, getXPathDocFontQuery(p, docFont, local = local)),
         bbox = getBBox2(txtNodes), docFont = TRUE, align = "left", local = FALSE, ...)    
{
    bbox = as.data.frame(bbox)
    p = as(p, "PDFToXMLPage")

    vals = switch(align,
                  left = bbox$left,
                  right = bbox$left + bbox$right,
                  center = (bbox$left + bbox$right)/2
                 )
    tt = table(vals)
#browser()    
    if(missing(txtNodes) && nrow(bbox) == 0 && !local) {
        # Use the page-specific font count
        return(getColPositions(p, threshold, docFont = docFont, align = align, local = TRUE, ...))
    }

    # Subtract 2 so that we start slightly to the left of the second column to include those starting points
    # or change cut to be include.lowest = TRUE
    ans = as.numeric(names(tt [ tt > nrow(bbox)*threshold])) - 2

    minDiff = 5
    if(length(ans) > 2 && any(delta <- (diff(ans) <= 20))) {
        # See Forrester-2008
        tt = split(sapply(txtNodes, xmlValue), cut(bbox[,1], c(ans, Inf)))
        w = sapply(tt, function(x) any(grepl("References", x)))
        if(any(w)) {
              # Need to check it is the References column
           minDiff = 20
        }

    }
    
    w = which(diff(ans) < minDiff)
    if(length(w))
        ans = ans[ - (w+1)]


    if(length(ans) == 1 && ans[1] > .4*dim(p)["width"]) {
        ## So only one column and it starts close to the middle of the page. Probably means
        ## there is one to the left that we didn't find. This may be because the text is in a different font.
        ##
        ans = c(margins(p)[1], ans)
     }
    
    ans
}


collapsePageCols =
function(vals, numPages)
{
    v2 = table(unlist(vals))

    ans = names(v2)[ v2 >  numPages * .5]
    as.integer(ans)
}

#########

getNumCols =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
    UseMethod("getNumCols")

getNumCols.character =
function(p, threshold = .1, txtNodes = getNodeSet(p, ".//text"), bbox = getBBox2(txtNodes))
{
    p = readPDFXML(p)
    getNumCols(p , threshold, txtNodes, bbox)
}


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



