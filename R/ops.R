
getPageWidth =
function(page)
{
   as.integer( xmlGetAttr(page, "width") )
}


flattenPages =
    #
    # remove the <page> nodes and elevate the children of each page to the same level.
    # This helps when we don't want to have to deal separately with content that flows across pages
    #  However, the y coordinates are messed up in successive pages as they start from 0 again.
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
    
