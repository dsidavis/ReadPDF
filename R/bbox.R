getBBox2 =
    # For text, not rect or line nodes.
function(nodes)
{
   ats = c("left", "top", "width", "height")
   m = do.call(rbind, lapply(nodes, function(x) as.integer(xmlAttrs(x)[ats])))
   colnames(m) = ats
   m
}

