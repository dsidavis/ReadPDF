getTextBBox =
function(nodes, asDataFrame = TRUE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
    getBBox2(nodes, asDataFrame, attrs, pages, rotation, color)

getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
    UseMethod("getBBox2")

getBBox2.PDFToXMLPage =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
    getBBox2(getNodeSet(nodes, ".//text"), asDataFrame, attrs = attrs, color = color)


getBBox2.XMLInternalNode =
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
{
    if(xmlName(nodes) == "page")
        return(getBBox2.PDFToXMLPage(nodes, asDataFrame, attrs, pages, rotation, color))

    getBBox2(list(nodes), asDataFrame, attrs, pages, rotation, color)
}

getBBox2.XMLNodeSet = getBBox2.list = 
    # For text, not rect or line nodes.
    #XXX Add support for color.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE, font = FALSE)
{
    if(is(nodes, "XMLInternalElementNode")) {
        if(xmlName(nodes) == "text")
            nodes = list(nodes)
        else
            nodes = getNodeSet(nodes, ".//text")
    }

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

    if(color) {
       cols = getTextNodeColors(nodes) # , m$font)
       if(asDataFrame)
           m$color = cols
       else
           m = cbind(m, color = cols)
   }

   if(font) {
       if(asDataFrame)
           m$font = as.integer(sapply(nodes, xmlGetAttr, "font"))
   }

   class(m) = c("TextElementsLocations", class(m))
   m
}



getBBox =
    #
    # This bbox function expects an attribute named bbox
    # This is for rect and line nodes, not <text> nodes. Use getBBox2() for that.
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, ...)
    UseMethod("getBBox")

getBBox.XMLInternalNode =
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, ...)    
{
    if(xmlName(nodes) == "page")
        return(getBBox.PDFToXMLPage(nodes, asDataFrame, color, diffs, dropCropMarks, ...))
    
    getBBox(list(nodes), asDataFrame, color, diffs, dropCropMarks)
}

getBBox.XMLNodeSet = getBBox.list =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, sort = TRUE, ...)    
{
    if(length(nodes) == 0) {
        ans = if(asDataFrame)
                data.frame(x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric())
              else
                 matrix(0, nrow = 0, ncol = 4 + if(color) 2 else 0, dimnames = list(NULL, c("x0", "y0", "x1", "y1", if(color) c("fill", "stroke"))))
        if(diffs)
            names(ans)[3:4] = c("width", "height")

        return(ans)
    }
    
    tmp = sapply(nodes, xmlGetAttr, "bbox")
    els = strsplit(tmp, ",")
    bb = matrix(as.numeric(unlist(els)), , 4, byrow = TRUE)
    colnames(bb) = c("x0", "y0", "x1", "y1")

    if(dropCropMarks) {
        ok = apply(bb, 1, function(x) all(x != 0))
        bb = bb[ ok, , drop = FALSE]
        nodes = nodes[ok]
    }
    
    ty = sapply(nodes, xmlName)
    ans = if(asDataFrame) {
        ans = as.data.frame(bb)
        ans$nodeType = ty
        ans
    } else {
        rownames(bb) = sapply(nodes, xmlName)
        bb
    }

    if(color) 
        ans = addBBoxColors(nodes, ans)

    if(diffs) {
        ans[3:4] = ans[3:4] - ans[1:2]
        names(ans)[3:4] = c("width", "height")        
    }

    if(sort & asDataFrame)
        ans = ans[order(ans[, "nodeType"]), ]

    class(ans) = c("LineRectLocations", class(ans))
    ans
}

addBBoxColors =
function(nodes, ans)    
{
        cols = lapply(c("fill.color", "stroke.color"), function(at) sapply(nodes, xmlGetAttr, at, ""))
        if(is.data.frame(ans)) {
            ans$fill = cols[[1]]
            ans$stroke = cols[[2]]            
        } else 
            ans = cbind(ans, fill = cols[[1]], stroke = cols[[2]])

        ans
}

getTextNodeColors =
function(nodes, fontIds = sapply(nodes, xmlGetAttr, "font"), fontInfo = getFontInfo(doc), doc = as(nodes[[1]], "XMLInternalDocument"))
{
    fontInfo[fontIds, "color"]
}

getBBox.PDFToXMLPage =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, ...)
{
  getBBox(getNodeSet(nodes, ".//line| .//rect"), asDataFrame, color)
}






getCoords =
function(x, lwd = TRUE, color = TRUE, ...)
    UseMethod("getCoords")

getCoords.PDFToXMLPage =    
function(x, lwd = TRUE, color = TRUE, ...)    
{
    getCoords(getNodeSet(x, ".//coords"), lwd, color, ...)
}

getCoords.XMLNodeSet = getCoords.XMLNodeList = getCoords.list =
function(x, lwd = TRUE, color = TRUE, ...)    
  lapply(x, parseCoord, lwd = lwd, color = color, ...)



parseCoord =
function(node, lwd = TRUE, color = TRUE)
{
  ans = as.data.frame(matrix(scan(textConnection(xmlSApply(node, xmlValue)), sep = " ", quiet = TRUE), , 2, byrow = TRUE))
  names(ans) = c("x", "y")
  class(ans) = c("PDFCoord", class(ans))
  if(lwd)
      ans$lineWidth = xmlGetAttr(node, "lineWidth", NA, as.numeric)
  if(color) {
      ans$stroke = xmlGetAttr(node, "stroke.color", NA, mkColor)
      ans$fill = xmlGetAttr(node, "fill.color", NA, mkColor)
  }

  ans
}
