
getPageWidth =
function(page)
{
   as.integer( xmlGetAttr(page, "width") )
}


flattenPages =
function(doc)
{
  pgs = getNodeSet(doc, "//page")
  lapply(pgs, replaceNodeWithChildren)
  doc
}


getVerticalRects =
function(nodes, threshold = 5)
{
   bb = getBBox(nodes)
   w = (bb[,3] - bb[,1]) < threshold
   bb[w,]
}


getHorizRects =
function(nodes, threshold = 5)
{
   bb = getBBox(nodes)
   w = (bb[,4] - bb[,2]) < threshold
   bb[w,]
}

getCrossPageLines =
function(rects, horiz = getHorizRects(rects), width = getPageWidth(xmlParent(rects[[1]])),
          threshold = .85)
{
  w = (horiz[,3] - horiz[,1])/width > threshold
  horiz[w,]
}


findNearestVerticalLine =
function(txtPos, linePos, threshold = 10)
{
     abs(txtPos[2] - linePos[,2]) < threashold
}
    
