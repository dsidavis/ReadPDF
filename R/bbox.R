getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
    UseMethod("getBBox2")

getBBox2.PDFToXMLPage =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
    getBBox2(getNodeSet(nodes, ".//text"), asDataFrame, color = color)


getBBox2.XMLInternalNode =
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
    getBBox2(list(nodes), asDataFrame, attrs, pages, rotation, color)

getBBox2.XMLNodeSet = getBBox2.list = 
    # For text, not rect or line nodes.
    #XXX Add support for color.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE)
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
       cols = getTextNodeColors(nodes, m)
       if(asDataFrame)
           m$color = cols
       else
           m = cbind(m, color = cols)
   }
   m
}



getBBox =
    #
    # This bbox function expects an attribute named bbox
    # This is for rect and line nodes, not <text> nodes. Use getBBox2() for that.
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE)
    UseMethod("getBBox")

getBBox.XMLInternalNode =
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE)    
    getBBox(list(nodes), asDataFrame, color, diffs)

getBBox.XMLNodeSet = getBBox.list =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE)    
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

    ans
}

addBBoxColors =
function(nodes, ans)    
{
        cols = lapply(c("fill.color", "stroke.color"), function(at) sapply(nodes, xmlGetAttr, at))
        if(is.data.frame(ans)) {
            ans$fill = cols[[1]]
            ans$stroke = cols[[2]]            
        } else 
            ans = cbind(ans, cols[[1]], cols[[2]])

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
function(nodes, asDataFrame = FALSE, color = FALSE)
{
  getBBox(getNodeSet(nodes, ".//line| .//rect"), asDataFrame, color)
}




