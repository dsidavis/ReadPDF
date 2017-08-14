findBibCites =
    #
    # Find text that identifies citations in the bibliography
    # in the form of numbers, ranges of numbers, etc.
    # that are superscripts.
    #
function(doc, supThreshold = .66, fonts = getFontInfo(doc),
         textFont = getDocFont(doc))
{
    w = fonts$size < supThreshold*textFont$size
    if(any(w))
        fontq = paste(sprintf("@font = '%s'", fonts$id[w]), collapse = " or ")
    else
        fontq = "true"
    xp = sprintf("//text[(%s) and isBibSup(normalize-space(.))]", fontq)
  tt = getNodeSet(doc, xp,  xpathFuns = list(isBibSup = isBibSup))
}

isBibSup =
function(str)
{
  grepl("^[0-9]+((,[0-9]+)*|­[0-9]+)$", str)
}
