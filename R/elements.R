
getImages =
function(doc, addPageNum = TRUE,
         attrs = c("x", "y", "width", "height", "originalWidth", "originalHeight", "filename", "Title", "Creator", "Producer", "CreationDate"),
         query = xpathQ("//img", doc))
{
    if(is.character(doc))
       doc = readPDFXML(doc)

    if(length(attrs)) {
        ans = xpathSApply(doc, query, function(x) xmlAttrs(x)[attrs])
        tmp = if(length(ans) == 0)
                 as.data.frame(structure(replicate(length(attrs), character(), simplify = FALSE), names = attrs), stringsAsFactors = FALSE)
              else
                 as.data.frame(t(ans), stringsAsFactors = FALSE)
        names(tmp) = attrs
        ans = type.convert(tmp)
    } else
        ans = xpathSApply(doc, query, xmlAttrs)
    
    if(addPageNum) {
        pages = as.integer(xpathSApply(doc, query, function(x) xmlGetAttr(xmlParent(x), "number")))
        if(is.data.frame(ans))
            ans$pageNum = pages
        else
            mapply(function(x, p) { x["pageNum"] = p; x}, ans, pages)
    }
    
    ans
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
