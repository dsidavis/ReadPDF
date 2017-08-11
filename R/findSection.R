#
#
# Find lines that
#  is one of the regular section header text
#  have a larger font than others on the page
#  shorter than most in the same column
#  have a larger font than most text nodes in the column
#  larger vertical distance between it and the next line
#

# Includes names
#    "LatestDocs/PDF/0629421650/Padula-2002-Andes virus and first case report1.xml"
# also doesn't get the subheadings for Patient 1 and Patient 2
#

findSectionHeaders =
    #
    # Given a node, find the node identifying the section in the paper
    # for this node.
    #
    #  Looking for text on its own line,  in bold or larger font.
    #  "../LatestDocs/PDF/2157652992/Calisher-2006-Bats_ Important Reservoir Hosts.xml"
    #  ""../LatestDocs/PDF/0000708828/Li-2009-Sensitive, qualitative detection of hu.xml"
    #  "../LatestDocs/PDF/2636860725/Růžek-2010-Omsk haemorrhagic fever.xml"
    #  "../LatestDocs/PDF/3757461900/Matsuzaki-2002-Antigenic and genetic characte1.xml"
    #
function(doc, sectionName = c('introduction', 'conclusions',
                  'results', 'methods'),
            # For, e.g., Lahm-2007-Morbidity, with these 2 extra section names, we
            # match References and Ackno..  and these don't have
            # numbers.
            # Maybe go for numbered section titles first?         
          otherSectionNames = c('references', 'acknowledgements')
         )
{
    if(is.character(doc))
        doc = readPDFXML(doc)

       # Find section titles with numbers
    hasNum = FALSE    
    filter = paste(sprintf("(contains(lower-case(normalize-space(.)), '%s') and isNum(normalize-space(.)))", sectionName), collapse = " or ")
    xp = sprintf("//text[%s]", filter)
    intro = getNodeSet(doc, xp, xpathFuns = list(isNum = isSectionNum))
    hasNum = length(intro) > 0

    if(!hasNum) {
       filter = paste(sprintf("lower-case(normalize-space(.)) = '%s'", sectionName), collapse = " or ")
       xp = sprintf("//text[%s]", filter)
       intro = getNodeSet(doc, xp)
    }
    
    #    cols = getColPositions(doc[[1]])

    # Check if these are centered on a column or on the page. If so,
    # other nodes we think are titles also better be centered.

    if(length(intro)) {
        if(hasNum)
           return(getNodeSet(doc, "//text[isNum(normalize-space(.))]", xpathFuns = list(isNum = isSectionNum)))
        
        fontID = xmlGetAttr(intro[[1]], "font")
         # Check if on line by itself and not just a word.        
        return(getNodesWithFont(doc, fontID = fontID))
    }
}

isSectionNum =
    #
    # For use in XPath test.
    #
function(x)
    grepl("^[0-9](\\.[0-9](\\.[0-9])?)?\\. ", x)


getNodesWithFont =
function(doc, fontID)
{
  getNodeSet(doc, sprintf("//text[@font = '%s']", fontID))        
}

isOnLineBySelf =
function(node, pos = getColPositions(doc),
         textNodes = getNodeSet(xmlParent(node), ".//text"),
         bbox = getBBox2(textNodes, TRUE))
    # doc = as(node, "XMLInternalDocument"),
{
    colNodes = getTextByCols(pageOf(node, TRUE), asNodes = TRUE)
    # determine which column this is in
    colNum = which(sapply(colNodes, function(x) any(sapply(x, identical, node))))
    col = colNodes[[colNum]]
#    lines = split(col, as.integer(sapply(col, xmlGetAttr, "top")))
    h = as.integer(xmlGetAttr(node, "top"))
    npos = as.integer(sapply(col, xmlGetAttr, "top"))
    sum(npos == h) == 1
}

#XXX give proper name.
f = 
function(page, nodes = getNodeSet(page, ".//text"), bb = getBBox2(nodes, TRUE),
          cols = getColPositions(page))
{    
    b = split(bb, cut(bb$left, c(0, cols[-1], Inf) -2))
    k = lapply(b, function(x) x[order(x$top),])
}



getNodesBetween =
function(x, y)
{
    s = pageOf(x)
    e = pageOf(y)
    if(e > s) {
        # get all the nodes on each page up to e
        p1 = getTextAfter(x)
    }

}

getTextAfter =
function(x, to = NULL, before = FALSE)
{
    cols = getTextByCols(xmlParent(x), asNodes = TRUE)
      # find the column and the index of the node matching x
    i = lapply(cols, function(n) which(sapply(n, identical,  x)))
    colNum = which(sapply(i, length) > 0)

    if(!is.null(to)) {
        j = lapply(cols, function(n) which(sapply(n, identical,  to)))
        to.colNum = which(sapply(j, length) > 0)        
    }

    if(is.null(to)) {
        nodes = cols[[colNum]][ - (1:(i[[colNum]]-1)) ]
        if(colNum < length(cols))
            nodes = c(nodes, cols[(colNum+1):length(cols)])
    } else {
        if(colNum == to.colNum) {
           nn = cols[[ colNum ]]
           nodes = nn[  seq(i[[colNum]], j[[to.colNum]] - 1) ] 
        } else {
              # nodes in x's column
            nodes = cols[[colNum]][ - (1:(i[[colNum]]-1)) ]
            btwn = seq(colNum + 1, length = to.colNum  - colNum - 1)
            nodes =  c(nodes, cols[btwn],
                        cols[[to.colNum]][ seq(1, length = j[[to.colNum]] - 1) ])
        }
    }

    nodes
}
