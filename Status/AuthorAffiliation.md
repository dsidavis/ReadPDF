# Author Affiliation

We are working from the PDF documents, not the docx files (yet).

25% of the new PDF documents are scanned.

## Emerging Infectious Diseases 

22% of the papers mentioned in species.csv are from the Emerging Infectious Diseases journal.
Only 7% of the new PDFs are from EID.

Emerging Infectious Disease papers have particular characteristic of
```
Title
author names
...
___________
author affiliations
```
all in column 1.

+ isEID() identifies whether a paper is an Emerging Infections Diseases article.
+ getEIDAuthors() gives the nodes containing the author names. (May need to be made more robust, but
  probably okay)
+ getBelowLine() will get the nodes giving the author affiliation content in an EID paper.



# General

getAuthorAffil() is a start to getting the author nams and affiliations generally.


+ Get the text below a line either in first column or spans both columns, or in each column
  (Lu-2008)
+ Find line before main text after title (Yang-2010)
+ Title for Lu-2008
+ Title in between two lines, then author names and affiliations, then start of text - Haglund
  


# 

Running getAuthorAffil() (08:27 April 14) on all of the 406 documents in sp.xml.
We raise an error if there is no title, or no abstract.

We get 237 list elements and 169 errors.

```
affil = lapply(sp.xml, function(x) try(getAuthorAffil(x)))
table(unlist(affil[sapply(affil, is, 'try-error')]))
```
```

                                                                         Error : 1: PCDATA invalid Char value 12\n\n 
                                                                                                                   2 
                                                           Error in bbox[, "top"] : incorrect number of dimensions\n 
                                                                                                                   1 
                               Error in dimnames(x) <- dn : \n  length of 'dimnames' [2] not equal to array extent\n 
                                                                                                                   1 
                                                             Error in findAbstract(doc) : try within single column\n 
                                                                                                                   6 
                                                                          Error in getAuthorAffil(x) : no abstract\n 
                                                                                                                  20 
                                                                             Error in getAuthorAffil(x) : No title\n 
                                                                                                                 107 
                                                           Error in lines[, "y0"] : incorrect number of dimensions\n 
                                                                                                                   5 
Error in seq.default(colNum + 1, length = to.colNum - colNum - 1) : \n  'length.out' must be a non-negative number\n 
                                                                                                                   1 
                                                              Error in strsplit(tmp, ",") : non-character argument\n 
                                                                                                                  19 
   Error in tapply(nodes, pgnum, nodesByLine, asNodes, baseFont = baseFont,  : \n  arguments must have same length\n 
                                                                                                                   1 
                                                          Error in tlines[, "y0"] : incorrect number of dimensions\n 
                                                                                                                   3 
                                                                    Error in tlines[1, ] : subscript out of bounds\n 
                                                                                                                   3 
```

So 20 no abstract, and 107 no title. 2 invalid XML docs.   40 other errors.

The "no title" could be just that it is returned as a character from getDocTitle().
We want the node.


## Docs for which we get an answer
table(sapply(affil[sapply(affil, is.list)], length))

  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  17 
 10  10  68   3   8   8   2   8   3   6   2   5   4   2   5   5   4 
 18  19  21  23  24  25  26  29  30  31  32  34  37  38  39  40  42 
  1   2   6   4   4   1   6   6   1   1   1   1   1   2   2   1   1 
 43  44  45  46  47  49  51  53  54  59  60  62  64  65  67  71  81 
  3   1   1   2   4   2   2   1   1   1   2   1   1   1   2   1   2 
 84  85  89  99 100 103 115 123 160 202 218 285 330 910 
  2   1   1   1   1   1   1   1   1   1   1   1   1   1 



# Mahalingam-2012
Author affiliations are in narrow column on right.


# Fails
"./1456230004/Haglund-2003-Characterisation of human tick-bo.xml" - doesn't get column positions.
"./2461911560/Curtis-2007-A spatial variant of the Basic Rep.xml" - cover page. So not doing the right thing. Identify and ignore the cover page.

"./1899620778/Arroyo-2011-Characterization of the temporal a.xml"
   Fails to get correctly abstract.
   gets the bit after the title. but eats the "abstract" which is Objective Animals ... between 2
   dark lines. 


 "./0244522529/Llopis-2015-Further circulation of West Nile 1.xml"  - cover page
    even after removing first page, can't get title. In regular font.
	 Cover page - "Accepted Author Manuscript...."
	 getNodeSet(d, "//page[1]//text[contains(., 'Accepted Author')]")
	 
"./3873003146/Izadi-2004-Crimean-Congo hemorrhagic fever in1.xml"  - no title. But regular Elsevier journal.	 

"./2775139994/Yang-2010-Probable variant Creutzfeldt-Jakob d.xml" - doesn't get abstract
  curious first 2 nodes - pcn_2151 and 652...658
  Abstract is between 2 lines.
  "Key words"  within these lines.

"./3681646993/Randolph-2008-Tick-borne encephalitis incidenc.xml" - fail to get title.  Gets abstract correctly.

"./0848476047/Ellis-2012-Yellow fever virus susceptibility o.xml" - elsevier doc. No title.


"./2996829660/Lu-2008-Tick-borne encephalitis in mainland Ch.xml" - wrong   Includes title and only
one author
   Abstract correct.
   Title is Review.  Need to ignore this.


"3178445298/Nolen-Walston-2007-Eastern equine encephalitis.xml"
  wrong abstract.
  get text below line

# Working with getAuthorAffil()

"./3901467270/Medina-2009-Ecology, genetic diversity, and p1.xml"
"./1641380441/Nidom-2012-Serological evidence of Ebola virus.xml"
"./0096914037/Stiasny-2009-Molecular mechanisms of flaviviru.xml"
"./0096914037/Stiasny-2009-Molecular mechanisms of flaviviru.xml"
"./0381298807/Mores-2009-Phylogenetic relationships among O1.xml"
"./0859336341/Kuno-2006-Characterization of Sepik and Entebb.xml"
"./1477898070/Dilcher-2012-Genetic characterization of Bhan1.xml"
"./3759643942/Drexler-2010-Genomic characterization of sever.xml"


# Scanned. (and isScanned() says TRUE)
"./3052373796/Foster-1972-Human monkeypox.xml"
"./0138003548/St George-1977.xml"
"./0144716285/543.full.xml"





# getBelowLine()
"./0381298807/Mores-2009-Phylogenetic relationships among O1.xml" - fix - line goes all the way across 2 columns.
