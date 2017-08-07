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

