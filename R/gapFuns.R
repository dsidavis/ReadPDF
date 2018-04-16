if(FALSE) {
    doc3 = readPDFXML("LatestDocs/PDF/3475635737/Nakgoi-2014-Dengue, Japanese Encephalitis and.xml" )
    ll3 = pageNodesByLine(doc3[[1]])
    xx3 = groupLines(ll3, 13)
    sapply(xx3, names)
    spansColumn(unlist(xx[[4]]), 450)
}    

groupLines =
    ##
    ##
    ## "LatestDocs/PDF/3475635737/Nakgoi-2014-Dengue,%20Japanese%20Encephalitis%20and.xml" - lineskip 13
function(ll, lineskip = 16) # 18 didn't separate the abstract from columns in Mackenzie-2001.pdf
{
    bbs = lapply(ll, getBBox2, asDataFrame = TRUE)
    tops = sapply(bbs, function(x) median(x$top))
    d = diff(tops)

    if(is.na(lineskip)) {
        tt = table(d)
        lineskip = as.integer(names(tt)[which.max(tt)])
    }
    
    grps = cumsum(c(0, d) > 2*lineskip)
    blocks = split(ll, grps)
}


pageNodesByLine =
function(page)
{        
#doc = readPDFXML("NewPDFs/Dobrava Virus/Scharninghausen-1999.xml")
#    bb = getBBox(doc[[1]])
    tt = getNodeSet(page, ".//text")
    ll = nodesByLine(tt)
}


spansColumn =
function(nodes, gap, bbox = getBBox2(nodes, asDataFrame = TRUE), threshold = 20)
{
   start = bbox$left
   end = bbox$left +  bbox$width

   w = start < gap & end > gap
   
#   w = abs(gap - start) < threshold | abs(end - gap) < threshold
   any(w) # table(w)
}


getNodePos =
function(nodes, bbox = getBBox2(nodes, asDataFrame = TRUE))
{
   data.frame(start = bbox$left, end = bbox$left + bbox$width)
}





hasGap = 
function(nodes, col = getColPositions(xmlParent(node[[1]])), bbox = getBBox2(nodes, asDataFrame = TRUE))
{
   start = bbox$left
   end = bbox$left +  bbox$width
   n = length(nodes)
   d = start[-1] - end[-n]
   browser()
}


getPageGroups =
function(page, lineskip = 20)
{
    ll = pageNodesByLine(page)
    g = groupLines(ll, lineskip)
}

