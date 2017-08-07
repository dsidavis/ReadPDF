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



