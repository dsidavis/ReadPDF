# Move to PDF -  and change to PDFTextBoundingBox rather than TextBoundingBox.
setMethod("right", "PDFTextBoundingBox", function(x, ...) x$left + x$width)
setMethod("bottom", "PDFTextBoundingBox", function(x, ...) x$top + x$height)

setMethod("width", "TextBoundingBox", function(x, ...) x$width)
setMethod("height", "TextBoundingBox", function(x, ...) x$height)


# See getTextNodeColors
setMethod("getTextColors", "PDFToXMLPage",
          function(obj, ...) {
            getTextNodeColors(getNodeSet(obj, ".//text"))
          })


setAs("PDFToXMLPage", "TextBoundingBox",
      function(from) {
          getTextBBox(from, asDataFrame = TRUE)
      })



setMethod("show", "PDFToXMLDoc",
   print.PDFToXMLDoc <- function(object) {

    np = getNumPages(object)
    show(paste(docName(object), ";",  np, paste0("page", if(np == 1) "" else "s"), "XML representation of PDF document"))
 })
