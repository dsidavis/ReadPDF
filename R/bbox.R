getBBox2 =
    # For text, not rect or line nodes.
function(nodes, asDataFrame = FALSE)
{
   ats = c("left", "top", "width", "height")
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

