library(XML)

# See MarkLundy/readPDF2HTML.R
setOldClass(c("PDFToHTMLDoc", "ConvertedPDFDoc", "XMLInternalDocument", "XMLAbstractDocument"))
setOldClass(c("PDFToHTMLPage", "ConvertedPDFPage", "XMLInternalElement", "XMLInternalNode", "XMLAbstractNode"))

pdftohtmlDoc =
function(file)
{
   if(grepl("\\.pdf$", file))
      convertPDF2XML(file)
   else {
      doc = xmlParse(file)
      class(doc) = c("PDFToHTMLDoc", "ConvertedPDFDoc", class(doc))
      doc
   }
}

showPage =
    # toplevel function.  Provide a file and a page number, and we render that page.
function(f, pageNum = 1, doc = xmlParse(f), page = getNodeSet(doc, "//page")[[pageNum]])
{
  renderPage(page)
}

renderPage =
    #
    # Give us the page.
    #
function(page)
{    
    p = page
    psize = as.integer(xmlAttrs(p)[c("height", "width")])
    h = psize[1]

    plot(0, type = "n", xlab = "", ylab = "", xlim = c(0, psize[2]), ylim = c(0, psize[1]))
    title(sprintf("%s, page %s", docName(page), xmlGetAttr(page, "number")))
    
    rr = getNodeSet(page, ".//rect ")
    if(length(rr)) {
        bb = getBBox(rr)
        col = sapply(rr, function(x) mkColor(xmlGetAttr(x, "fill.color")))
        lwd = as.numeric(sapply(rr, function(x) xmlGetAttr(x, "lineWidth")))
        rect(bb[,1], h - bb[,2], bb[,3], h-bb[,4], border = "green", col = col, lwd = 3)
    }

    if(length ( lines <- getNodeSet(page, ".//line "))) {
        bb = getBBox(lines)
        sapply(1:nrow(bb),
                function(i) {
                     at = xmlAttrs(lines[[i]])
                     lines(bb[i, c(1,3)], h - bb[i, c(2, 4)], col = mkColor(at["stroke.color"]), lwd = as.numeric(at["lineWidth"]))
                })
        # lines(bb[,1], h - bb[,2], bb[,3], h-bb[,4], col = "red")
    }

    
    txt = getNodeSet(page, ".//text")
    if(length(txt)) {
        bb = t(sapply(txt, xmlAttrs))
        storage.mode(bb) = "double"
    
        text(bb[,2], h - bb[,1], sapply(txt, xmlValue), cex = 2, adj = c(0, 1))
    }


    imgs = getNodeSet(page, ".//img")
    if(length(imgs)) {
        bb = getBBox2(imgs)
        rect(bb[,1], bb[,2], bb[,3], bb[,4], col = "blue", lty = 3)
    }

    TRUE
}

mkColor =
function(x)
{
   els = as.integer(strsplit(x, ",")[[1]])
   rgb(els[1], els[2], els[3], maxColorValue = 65536)
}

getBBox =
function(nodes)
{   
    tmp = sapply(nodes, xmlGetAttr, "bbox")
    els = strsplit(tmp, ",")
    bb = matrix(as.numeric(unlist(els)), , 4, byrow = TRUE)
    colnames(bb) = c("x0", "y0", "x1", "y1")
    bb
}






