getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE, attrs = c("left", "top"))
{
   if(is(nodes, "XMLInternalElementNode"))
       nodes = getNodeSet(nodes, ".//text")

   ats = c(attrs, "width", "height")    
   if(length(nodes) == 0)
       return(matrix(0, 0, 4, dimnames = list(NULL, ats)))
    
   m = do.call(rbind, lapply(nodes, function(x) as.integer(xmlAttrs(x)[ats])))
   colnames(m) = ats

   txt = sapply(nodes, xmlValue)
   if(asDataFrame) {
     m = as.data.frame(m)
     m$text = txt
   } else
     rownames(m) = txt
   m
}



getBBox =
    #
    # This bbox function expects an attribute named bbox
    #
function(nodes, asDataFrame = FALSE)
{
    if(length(nodes) == 0) {
        if(asDataFrame)
            return(data.frame(x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric()))
        else
            return(matrix(0, nrow = 0, ncol = 4))
    }
    
    tmp = sapply(nodes, xmlGetAttr, "bbox")
    els = strsplit(tmp, ",")
    bb = matrix(as.numeric(unlist(els)), , 4, byrow = TRUE)
    colnames(bb) = c("x0", "y0", "x1", "y1")
    if(asDataFrame) 
       as.data.frame(bb)
    else
       bb
}
