
getTables =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    getNodeSet(doc, "//img")
}


getCaption =
function(node)
{

}
