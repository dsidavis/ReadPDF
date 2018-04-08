getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE)
    UseMethod("getBBox2")

getBBox2.PDFToXMLPage =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE)
    getBBox2(getNodeSet(nodes, ".//text"), asDataFrame, color)


getBBox2.XMLInternalNode =
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE)
    getBBox2(list(nodes), asDataFrame, attrs, pages, rotation)

getBBox2.XMLNodeSet = getBBox2.list = 
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE)
{
    if(is(nodes, "XMLInternalElementNode"))
        if(xmlName(nodes) == "text")
            nodes = list(nodes)
        else
            nodes = getNodeSet(nodes, ".//text")

   ats = c(attrs, "width", "height")    
   if(length(nodes) == 0)
       return(matrix(0, 0, length(ats) + pages, dimnames = list(NULL, c(ats, if(pages) "page"))))
    
   m = do.call(rbind, lapply(nodes, function(x) as.integer(xmlAttrs(x)[ats])))
   colnames(m) = ats

   if(pages)
      pageNums = sapply(nodes, pageOf)
    
   txt = sapply(nodes, xmlValue)
   if(asDataFrame) {
     m = as.data.frame(m)
     m$text = txt
     if(pages)
        m$page = pageNums
   } else {
       if(pages)
          m = cbind(m, page = pageNums)
       rownames(m) = txt
   }
   m
}



getBBox =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE)
    UseMethod("getBBox")

getBBox.XMLInternalNode =
function(nodes, asDataFrame = FALSE, color = FALSE)    
    getBBox(list(nodes), asDataFrame, color)

getBBox.XMLNodeSet = getBBox.list =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE)    
{
    if(length(nodes) == 0) {
        if(asDataFrame)
            return(data.frame(x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric()))
        else
            return(matrix(0, nrow = 0, ncol = 4 + if(color) 2 else 0, dimnames = list(NULL, c("x0", "y0", "x1", "y1", if(color) c("fill", "stroke")))))
    }
    
    tmp = sapply(nodes, xmlGetAttr, "bbox")
    els = strsplit(tmp, ",")
    bb = matrix(as.numeric(unlist(els)), , 4, byrow = TRUE)
    colnames(bb) = c("x0", "y0", "x1", "y1")
    ty = sapply(nodes, xmlName)
    if(asDataFrame) {
        ans = as.data.frame(bb)
        ans$nodeType = ty
        ans
    } else {
        rownames(bb) = sapply(nodes, xmlName)
        bb
    }
}


getBBox.PDFToXMLPage =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE)
{
  getBBox(getNodeSet(nodes, ".//line| .//rect"), asDataFrame, color)
}




