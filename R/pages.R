`[[.ConvertedPDFDoc` =  #`[[.PDFMinerDoc` =
function(x, i, j, ...)
{
   getPages(x)[[i]]
}

`[.ConvertedPDFDoc` =   # `[.PDFMinerDoc` =
function(x, i, j, ...)
{
    getPages(x)[i, ...]
}

setGeneric("lapply")

lapply.ConvertedPDFDoc  =  lapply.PDFToXMLDoc = # PDFMinerDoc =
function(X, FUN, ...)
    lapply(getPages(X), FUN, ...)
setMethod("lapply", "ConvertedPDFDoc", lapply.ConvertedPDFDoc)

sapply.ConvertedPDFDoc = sapply.PDFToXMLDoc = # PDFMinerDoc =
function(X, FUN, ...)
  sapply(getPages(X), FUN, ...)

setMethod("sapply", "ConvertedPDFDoc", sapply.ConvertedPDFDoc)

getNumPages =
function(doc)
{
    length(getNodeSet(doc, "//page"))
}

setGeneric("length")
setMethod("length", "ConvertedPDFDoc",
          function(x)
           getNumPages(x))


dim.PDFToXMLPage =
function(x)
{
   structure( as.numeric(xmlAttrs(x)[c("width", "height")]), names = c("width", "height"))
}
getPages = 
function(doc)
{
  p = getNodeSet(doc, "//page")
    # Change the class of the page nodes so that we know they are PDF page nodes.
  lapply(p, function(x, newClass) {
                   class(x) = c(newClass, class(x))
                   x
                }, gsub("Doc$", "Page", grep("PDF", class(doc), value = TRUE)))
}



pageOf =
function(node, asNode = FALSE)
    UseMethod("pageOf")

pageOf.list = 
function(node, asNode = FALSE)
{
    sapply(node, pageOf, asNode)
}

pageOf.XMLInternalNode = pageOf.XMLInternalElementNode =
function(node, asNode = FALSE)
{    
    p = getNodeSet(node, ".//ancestor::page")[[1]]
    if(asNode)
        p
    else
        xmlGetAttr(p, "number", converter = as.integer)
}


columnOf =
function(node, cols = getColPositions(xmlParent(node)))
{
    bb = getBBox2(list(node))
    which(bb[1, "left"] < cols)[1] - 1
}
