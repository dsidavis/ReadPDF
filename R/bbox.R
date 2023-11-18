getTextBBox.PDFToXMLPage =
function(obj, asDataFrame = TRUE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = TRUE, color = TRUE, ...)
     getBBox2(obj, asDataFrame, attrs, pages, rotation, color, ...)

getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE, ...)
    UseMethod("getBBox2")

getBBox2.PDFToXMLPage = # XXX getTextBBox.PDFToXMLPage =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE, ...)
{
    ans = getBBox2(getNodeSet(nodes, ".//text"), asDataFrame, attrs = attrs, color = color, ...)
    attributes(ans) = append(attributes(ans), list(pageNumber = as.integer(xmlGetAttr(nodes, "number")), filename = docName(nodes), pageDimensions = dim(nodes)))
    ans
}

getPageHeight.PDFTextBoundingBox =
function(page)
{
   attr(page, "pageDimensions")[1]
}

getPageWidth.PDFTextBoundingBox =
function(page)
{
   attr(page, "pageDimensions")[2]
}


getBBox2.PDFToXMLDoc =  #XXX getTextBBox.PDFToXMLDoc =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE, font = FALSE, ...)
{
   Dociface:::bboxForDoc(getTextBBox, nodes, asDataFrame, attrs, pages, rotation, color)
}


getBBox2.XMLInternalNode = getTextBBox.XMLInternalNode =
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE, ...)
    # Should check if xmlName(nodes) is "page"
    getBBox2(list(nodes), asDataFrame, attrs, pages, rotation, color)

getBBox2.XMLNodeSet = getBBox2.list =
  getTextBBox.XMLNodeSet = getTextBBox.list = 
    # For text, not rect or line nodes.
    #XXX Add support for color.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top", if(rotation) "rotation"), pages = FALSE, rotation = FALSE, color = FALSE, font = TRUE, ...)
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
       if(asDataFrame) {
           info = getFontInfo(xmlParent(xmlParent(nodes[[1]])))
           fid = sapply(nodes, xmlGetAttr, "font")
           i = match(fid, info$id)
           vars = c("size", "family", "isItalic", "isBold", "isOblique", "name")
           # we could keep these with the same partial matching, but unique, name with isBold.font
           m[, paste0("font", capitalize(vars))] = info[i, vars]

       } else
           warning("for now, font = TRUE and asDataFrame = FALSE is not supported for TextBoundingBox for PDF/XML documents")
       
   }

   class(m) = c("PDFTextBoundingBox", "PDFBoundingBox", "TextBoundingBox", class(m))
    
   m
}

capitalize =
function(x)
{
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

getBBox =
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, ...)
    UseMethod("getBBox")    

getBBox.XMLInternalNode = getShapesBBox.XMLInternalNode =
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, ...)    
    getBBox(list(nodes), asDataFrame, color, diffs, dropCropMarks)


getBBox.XMLNodeSet = getBBox.list =
  getShapesBBox.XMLNodeSet = getShapesBBox.list =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, ...)    
{
    if(length(nodes) == 0) {
        ans = if(asDataFrame)
                data.frame(x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric(), nodeType = character())
            
              else
                matrix(0, nrow = 0, ncol = 4 + if(color) 2 else 0, dimnames = list(NULL, c("x0", "y0", "x1", "y1", if(color) c("fill", "stroke"))))
        if(diffs)
            names(ans)[3:4] = c("width", "height")

        class(ans) = c("PDFShapesBoundingBox", "PDFBoundingBox", "ShapeBoundingBox", class(ans))
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


    ans$lineWidth = as.numeric(sapply(nodes, xmlGetAttr, "lineWidth", NA))
    
    class(ans) = c("PDFShapesBoundingBox", "PDFBoundingBox", "ShapeBoundingBox", class(ans))

    ans
}

addBBoxColors =
function(nodes, ans)    
{
        cols = lapply(c("fill.color", "stroke.color"), function(at) mkColor(sapply(nodes, xmlGetAttr, at)))
        if(is.data.frame(ans)) {
            ans$fill = cols[[1]]
            ans$stroke = cols[[2]]            
        } else 
            ans = cbind(ans, fill = cols[[1]], stroke = cols[[2]])

#XXX Add a class to identify has colors        
        ans
}


getTextNodeColors =
function(nodes, fontIds = sapply(nodes, xmlGetAttr, "font"), fontInfo = getFontInfo(doc), doc = as(nodes[[1]], "XMLInternalDocument"))
{
    fontInfo[fontIds, "color"]
}

getShapesBBox.PDFToXMLPage = getBBox.PDFToXMLPage =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = TRUE, color = TRUE, diffs = FALSE, dropCropMarks = TRUE, images = TRUE, ...)
{
    ans = getBBox(getNodeSet(nodes, ".//line| .//rect"), asDataFrame, color, ...)
    if(images) {
        imgs = getImages(nodes, attrs = c("x", "y", "width", "height"))
        if(nrow(imgs) > 0) {
            imgs$x1 = imgs$x + imgs$width
            imgs$y1 = imgs$y + imgs$height
            imgs$nodeType = "img"
            imgs = imgs[, c("x", "y", "x1", "y1", "nodeType")]
            imgs$fill = imgs$stroke = imgs$lineWidth = NA
            names(imgs)[1:2] = c("x0", "y0")
            ans = structure(rbind(ans, imgs), class = class(ans))
        }
    }
    ans
}



# Method for Document in Dociface now.
#    getShapesBBox.PDFToXMLDoc =
getBBox.PDFToXMLDoc =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE, color = FALSE, diffs = FALSE, dropCropMarks = TRUE, ...)
{
    bboxForDoc(getShapesBBox, nodes, asDataFrame, color, diffs, dropCropMarks, ...)
}






