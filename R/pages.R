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


lapply.ConvertedPDFDoc =  # PDFMinerDoc =
function(X, FUN, ...)
  lapply(getPages(X), FUN, ...)

sapply.ConvertedPDFDoc = # PDFMinerDoc =
function(X, FUN, ...)
  sapply(getPages(X), FUN, ...)


getNumPages =
function(doc)
{
    length(getNodeSet(doc, "//page"))
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



