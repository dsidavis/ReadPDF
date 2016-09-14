library(ReadPDF)

if(FALSE) {
doc = xmlParsePDFTOTHML("2ColPaper.xml")

# Deal with the second page.
p = doc[[2]]
renderPage(p, cex = .5)

cols = getTextByCols(p)
nchar(cols)
}


##
# Todo
#[Done]  Deal with pages that have some text in a column that spans the entire width of the page
#   and then others in 2 or more columns, e.g., an abstract and then the 2 column text.
#
#
# Fix the fonts so that they are unique. Do this in pdftohtml

#
# Headers and footers on the pages.


pdfText = pdf_text =
function(doc)
{
   if(is.character(doc))
      doc = xmlParsePDFTOTHML(doc)
   
   lapply(getPages(doc), getTextByCols)
}



getTextByCols =
    #
    # Have to remove headers and footers first!
    #
    #  The nodes that are a little further to the right of the majority are indenations of the
    # first line in a paragraph, like this sentence!
    #
function(p, threshold = .1)
{
    txtNodes = getNodeSet(p, ".//text")
    bb = getBBox2(txtNodes)
    bb = as.data.frame(bb)
    bb$text = sapply(txtNodes, xmlValue)

    tt = table(bb$left)
    # Subtract 2 so that we start slightly to the left of the second column to include those starting points
    # or change cut to be include.lowest = TRUE
    breaks = as.numeric(names(tt [ tt > nrow(bb)*threshold])) - 2

    cols = split(bb, cut(bb$left, c(0, breaks[-1], Inf)))
    cols = sapply(cols, function(x) paste(x$text[ order(x$top) ], collapse = "\n"))
}



getTextFonts =
function(page, fontInfo = getFontInfo(page))
{
   txtNodes = xpathSApply(page, ".//text")
   fid = sapply(txtNodes, xmlGetAttr, "font")

   ans = fontInfo[fid, ]
   ans$text = sapply(txtNodes, xmlValue)
   ans
}

getFontInfo =
function(page)
{
   fonts = getNodeSet(page, ".//fontspec")
   fids = sapply(fonts, xmlGetAttr, "id")
   df = do.call(rbind, lapply(fonts, function(x) xmlAttrs(x)[c("size", "family", "color")]))
   rownames(df) = fids
   df = as.data.frame(df, stringsAsFactors = FALSE)
   df$size = as.integer(df$size)
   df
}


bodyLine =
function(doc)
{
   ll = getNodeSet(doc, "//line")
   bb = getBBox(ll, asDataFrame = TRUE)
     # get the page number for each line
   bb$pageNum = sapply(ll, function(x) xmlGetAttr(xmlParent(x), "number"))

      # now get all of the text nodes
   txtBB = getBBox2(getNodeSet(doc, "//text"))
      # get the extremes for the text, i.e., the left and right margins
   txtExt = c(left = min(txtBB[, "left"]), right = max(txtBB[, "left"] + txtBB[, "width"]))

      # All the lines that are greater than 95% of the 
   hll = bb[ ((bb[, "x1"] - bb[, "x0"])/ diff(txtExt)) > .95,  ]

   # is there the same y0 (or y1) on each page for these lines
   table(hll[, "y1"])
}
