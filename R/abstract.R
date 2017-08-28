#
#
#
#
#
#

margins =
function(page, bbox = getBBox2(getNodeSet(page, ".//text")))
{
   c(min(bbox[, 1]), max(bbox[,1] + bbox[,3]))
}

findAbstract =
function(doc, asNodes = TRUE)
{
    if(is.character(doc))
        doc = readPDFXML(doc)
    
    a = getNodeSet(doc[[1]], "//text[lower-case(normalize-space(.)) = 'abstract' or lower-case(normalize-space(.)) = 'summary']")

    if(length(a) == 0) {

    }
    page = xmlParent(a[[1]])
    mar = leftMargin(xmlParent(a[[1]]))
    bb = getBBox2(a)
    if(bb[1,1] > mar[1] * 1.1) {
        # So not flush with left margin or even very close
        # e.g. 1.s2.0-S1090
        #
        # Can find the end of the abstract, i.e. vertical position below which
        #  content is no longer in abstract
        # Then find all nodes to the right of bb[1,1] and above this position.
        # OR find all lines flush with this bb[1,1] and the lowest of these 
        # defines the end. Then go back and find all the text to right
        # 
        nodes = getNodeSet(page, sprintf(".//text[abs(@left - %d) < 4]", bb[1,1]))
        bb2 = getBBox2(nodes)
        bot = max(bb2[, 2] + bb[,4])
        nodes = getNodeSet(page, sprintf(".//text[@left > %d - 4 and @top > %d and @top + @height <= %f + 9]", bb[1,1], bb[1,2], bot))       
    }
        
    browser()

}


getAbstractBySpan=
function(doc, col = getColPositions(doc[[1]]))
{
    if(length(col) == 1)
        return(NULL)

    
}




