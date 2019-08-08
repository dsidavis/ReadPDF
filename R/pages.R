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



setMethod("getNumPages", "ConvertedPDFDoc",  # getNumPages =
  function(doc, ...) {
    length(getNodeSet(doc, "//page"))
  })


#setGeneric("length")
setMethod("length", "ConvertedPDFDoc",
    length.ConvertedPDFDoc <-  function(x)  getNumPages(x))

if(FALSE) {
    # See ops.R
dim.PDFToXMLPage =
function(x)
{
   structure( as.numeric(xmlAttrs(x)[c("width", "height")]), names = c("width", "height"))
}
}

setMethod("getPages",  "ConvertedPDFDoc",
function(doc, ...)
{
  p = getNodeSet(doc, "//page")
    # Change the class of the page nodes so that we know they are PDF page nodes.
  lapply(p, function(x, newClass) {
      #!!! orig class(x) = c(newClass, class(x))
                   class(x) = c("PDFToXMLPage", "ConvertedPDFPage", "DocumentPage", class(x))      
                   x
                }, gsub("Doc$", "Page", grep("PDF", class(doc), value = TRUE)))
})



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
        structure(p, class = c("PDFToXMLPage", class(p)))
    else
        xmlGetAttr(p, "number", converter = as.integer)
}


setGeneric("columnOf",
function(node, cols = getColPositions(xmlParent(node)), ...)
           standardGeneric("columnOf"))

setMethod("columnOf", "list",
function(node, cols = getColPositions(xmlParent(node)), ...)
          {
              sapply(node, columnOf, cols, ...)
          })

setMethod("columnOf",
          "XMLInternalElementNode",
function(node, cols = getColPositions(xmlParent(node)), ...)
{          
    bb = getBBox2(list(node))
    before = bb[1, "left"] < cols
    if(!any(before))
        length(cols)
    else
       which(before)[1] - 1
}
)
