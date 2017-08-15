removeRotated =
function(doc, ...)
{
    if(is.character(doc))
       doc = readPDFXML(doc)
    
    nodes = getRotatedText(doc, ...)
    removeNodes(nodes)
    nodes
}

getRotatedText =
function(doc, xpath = xpathQ("//text[not(@rotation = 0)]", doc))
{
    if(is.character(doc))
       doc = readPDFXML(doc)
    
    nodes = getNodeSet(doc, xpath)
}
