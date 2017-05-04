getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE)
{
   if(is(nodes, "XMLInternalElementNode"))
       nodes = getNodeSet(nodes, ".//text")

   ats = c("left", "top", "width", "height")    
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

