# Move to PDF -  and change to PDFTextBoundingBox rather than TextBoundingBox.
setMethod("right", "PDFTextBoundingBox", function(x, ...) x$left + x$width)
setMethod("bottom", "PDFTextBoundingBox", function(x, ...) x$top + x$height)

setMethod("width", "TextBoundingBox", function(x, ...) x$width)
setMethod("height", "TextBoundingBox", function(x, ...) x$height)
