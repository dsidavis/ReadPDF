
Fixing the author affiliation will help to find fewer sections.

Failing in 4 of the NewPDF documents.

[1] "./Australian Bat Lyssavirus/Xi-2012.xml"                         
[2] "./Dobrava Virus/Golovljova-2002.xml"                             
[3] "./Dobrava Virus/Nemirov-2003.xml"                                
[4] "./Seoul Virus/Smith-2002-Prevalence study of antibody to rat.xml"

The problem seems to be in getNodesBetween and getTextAfter, and
caused by the x and to nodes are in reverse order, i.e., the to is actually
in an earlier column/page than the to.
So we need to order these before we call getNodesBetween, i.e. in findSectionHeaders().
Add an option to findSectionHeaders()

Are they actually out of order in the XML/PDF?
No. It is that we compute them separately and just concatenate them together without checking the order.


columnOf() doesn't give the correct value.
getColPositions() is giving back just one value.

Use inColumn() 
