#  See  ~/DSIProjects/Zoonotics-shared/PublicationDate.R for a better version.
getDatePublished =
function(doc)
{    
   nodes = getNodeSet(doc, "//text[contains(., 'Received:')] | //text[contains(., 'Published:')] | //text[contains(., 'Accepted:')]")
   if(length(nodes) == 0)
       return(NULL)

   txt = sapply(nodes, xmlValue)
   e = strsplit(unlist(strsplit(txt, " / ")), ":")
   structure(XML:::trim(sapply(e, `[[`, 2)), names = XML:::trim(sapply(e, `[[`, 1)))
}


getSubmissionDateInfo =
function(doc, phrases = c("Received", "Accepted", "Available online", "Published at", "Published online", "received for review"))
{
  cond = sprintf("starts-with(normalize-space(.), '%s')", t(cbind(phrases, paste0("(", phrases))))
  getNodeSet(doc, sprintf("//text[%s]", paste(cond, collapse = " or ")))
}
