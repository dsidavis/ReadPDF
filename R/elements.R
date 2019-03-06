
getImages =
function(doc, addPageNum = FALSE)
{
    if(is.character(doc))
       doc = readPDFXML(doc)
    
    tmp = t(xpathSApply(doc, "//img", xmlAttrs))
    imgInfo = as.data.frame(matrix(as.numeric(tmp), nrow(tmp), ncol(tmp),
                                    dimnames = dimnames(tmp)))
    
    if(addPageNum)
        imgInfo$pageNum = as.integer(xpathSApply(doc, "//img", function(x) xmlGetAttr(xmlParent(x), "number")))
    
    imgInfo
}

extRegexp = "\\.[a-z]{3,4}"

getLinks =
function(doc, locations = FALSE, internal = TRUE)
{
    if(is.character(doc))
       doc = readPDFXML(doc)
    
    if(!locations) {
       u = unname(unlist(getNodeSet(doc, xpathQ("//ulink/@url", doc))))
       if(!internal) {
           b = removeExtension(basename(URLdecode(docName(doc))))
           i = grepl(sprintf("^%s%s#", b, extRegexp), u)
           u = u[!i]
#           u = grep(paste0(b,"#"), u, fixed = TRUE, invert = TRUE, value = TRUE)
       }
       return(u)
    }
    
    tmp = t(xpathSApply(doc, xpathQ("//ulink", doc), xmlAttrs))
    df = as.data.frame(tmp[, c("x1", "y1", "x2", "y2")])
    df[] = lapply(df, as.numeric)
    df$url = tmp[,"url"]
        
    df
}

removeExtension =
function(x)    
{
  gsub(extRegexp, "", x)
}

attrsToDataFrame =
function(x)
{

}



getMetaData =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    m = getNodeSet(doc, "//META")
    meta = structure(sapply(m, xmlGetAttr, "content"), names = sapply(m, xmlGetAttr, "name"))
    o = getNodeSet(doc, "//docinfo/*[local-name(.) != 'META']")
    meta[ paste0("pdf.", sapply(o, xmlName))] =  sapply(o, xmlValue, trim = TRUE)
    meta
}
