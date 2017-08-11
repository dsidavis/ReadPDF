getTables =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

      # Some docs have T able as two separate text elements
    getNodeSet(doc, "//text[contains(., 'Table') or contains(., 'TABLE')]")
}


