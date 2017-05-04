#  See  ~/DSIProjects/Zoonotics/PublicationDate.R for a better version.
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

