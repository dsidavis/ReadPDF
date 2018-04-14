
We want

```
titles = structure(lapply(sp.xml, function(x) try(getDocTitle(x, asNode = TRUE))), names = sp.xml)
```

```
err = sapply(titles, is, 'try-error')
table(err)
```

```
[1] "LatestDocs/PDF/2828631744/art%253A10.1023%252FA%253A1008199800011.xml"       
[2] "LatestDocs/PDF/3529243761/Pattnaik-2006-Kyasanur Forest disease_ an epid.xml"
[3] "LatestDocs/PDF/0148058638/Wong_et_al-2007-Reviews_in_Medical_Virology.xml"   
[4] "LatestDocs/PDF/3246714993/bok%253A978-3-540-70962-6.xml"                     
```

```
titles = titles[!err]
```


```
table(sapply(titles, class))
```
```
character      list    matrix 
       60       337         7 
```

We should have list(). So 67 need some attention.
is.list() will identify the matrix elements as they are not simple matrices of characters, but
matrices of XML nodes. So they will appear to be a list.
```
isM = sapply(titles, is.matrix)
isChar = sapply(titles, is.character)
isList = !isM & !isChar
att = names(titles)[!isList]
```
We want to fix getDocTitle() to return the nodes for these.

Some of these are NA; some are matrices.

## Matrices

```
names(titles)[isM]
```
Running the code on the first identifies mkLines() as calling sapply() and we change this to lapply.
Then all of these become lists.


## Characters

Debugging the first of these, we get back a list. So fixing mkLines() may have fixed some or all of
these.  So we re-run on these:
```
tmp = lapply(names(titles)[isChar], getDocTitle, asNode = TRUE)
```
We get 2 XMLNodeSet, 30 list ites, and 28 character.
These are all NA values from getDocTitle().
```
w = sapply(tmp, is.character)
b = names(titles)[isChar][w]
```
27 of these are scanned. So we change getDocTitle(, asNode = TRUE) to return an empty list.

"LatestDocs/PDF/2999137579/Wong et al 2007 supplement.xml" is the one that is not scanned.
But getDocTitle() used isScanned2(), not isScanned(), to determine if it is scanned. 
If we change this to isScanned(), then this returns a list with one node.
It is the wrong node, but at least a list() of nodes.
The page is mostly rotated. So it is ambiguous  as to what the title  should be.


So now we rerun getDocTitle on all of these that were not lists.
```
tmp = lapply(sp.xml[isM | isChar], getDocTitle, asNode = TRUE)
table(sapply(tmp, class))
table(sapply(tmp, is.list))
```
And we get all of them being lists.
11 are empty and we will have to explore these when looking at the lists.

We add these back into titles.

```
titles[isM | isChar] = tmp
```


# List Results

```
len = sapply(titles, length)
table(len)
```
59 have length 0.
Hopefully these are all scanned.

```
checkScanned = names(titles)[len == 0]
w = sapply(checkScanned, isScanned)
table(w)
```
```
FALSE  TRUE 
    6    53 
```
```
b = checkScanned[!w]
```
```
[1] "LatestDocs/PDF/3625557333/Jacobsen-2010-Borna disease in an adult alpaca.xml"
[2] "LatestDocs/PDF/4081573285/Morikawa-2007-Current knowledge on lower virul.xml"
[3] "LatestDocs/PDF/2406305536/Zacks-2010-Encephalitic alphaviruses.xml"          
[4] "LatestDocs/PDF/2853897527/978-1-4020-6106-6.xml"                             
[5] "LatestDocs/PDF/3637917410/Meister-2008-Serological evidence of continui1.xml"
[6] "LatestDocs/PDF/1050442495/Perez-2010-Spatial and phylogenetic analysis 1.xml"
```

"LatestDocs/PDF/2853897527/978-1-4020-6106-6.xml" is a 334 page book and the first page is a cover.
So this is legitimate.

```
b = b[-4]
```

If we run getDocTitle(, asNode = TRUE) on these, they all return a list with elements.
So the changes we made to correct the matrices and character issues appear to have changed these
results.
So we run getDocTitle on all of our sp.xml files again.

```
titles = structure(lapply(sp.xml[!err], function(x) try(getDocTitle(x, asNode = TRUE))), names = sp.xml[!err])
```
We get 3 warning messages which we should address:
```
Warning messages:
1: In fonts$size < mx :
  longer object length is not a multiple of shorter object length
2: In max(fonts$size[fonts$size < mx]) :
  no non-missing arguments to max; returning -Inf
3: In max(fonts$size[fonts$size < mx]) :
  no non-missing arguments to max; returning -Inf
```
The first of these resonates as being a problem we noticed in one paper.
Specifically, if we have two fonts with the same size and these are the biggest fonts,
do we just look at the first? or do we try both in succession as we check isTitleBad().

We look at the lengths again:
```
len = sapply(titles, length)
table(len)
```
```
  0   1   2   3   4   5   6   7   8   9  13  20  24  49  52  74  85 337 
 75  52 130  82  34  13   2   4   1   1   1   1   2   2   1   1   1   1
```

Now we have 75 elements with 0 length.
These are the scanned ones plus the 334 page book.


It is possible to have many nodes in the title.
For example, multiple font changes will lead to different nodes.
337 nodes is almost definitely wrong.

We can compute the text from these would-be "titles" and see how many characters/words are in each.
```
ti = titles[len > 0]
txt = sapply(ti, function(x) paste(sapply(unlist(x), xmlValue), collapse = " "))
summary(nchar(txt))
len = sapply(ti, len)
```
```
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    7.0    59.0    84.0   121.2   117.0  2434.0 
```
```
quantile(nchar(txt), seq(.1, 1, by = .1))
```
```
   10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
  38.8   54.0   64.0   74.0   84.0   93.0  106.0  125.0  152.0 2434.0 
```



We take a snapshot of what we currently have.
As we make changes to getDocTitle() and other functions, we can compare the results to these to see
which ones were affected. 
```
ti.orig = ti
```
We cannot directly compare XML nodes, so we will compare the number
and content of the nodes.


Let's look at the element with 20 nodes:
```
names(ti)[len == 20]
txt[len == 20]
```
```
                                                                         LatestDocs/PDF/2255202025/1-s2.0-S0147957114000058-main.xml 
"and Infectious Diseases Evaluation of a West Nile virus surveillance and early warning system in Greece, based on domestic pigeons" 
```
We get the title, but we also have additional text "and Infectious Diseases". This comes from the
journal title. This is an elsevier journal with the 2 images at the top corners and a grey box with
the journal name. In this case, the journal name spans 2 lines and  we are picking up the second
line as part of the title.
"Comparative Immunology, Microbiology and Infectious Diseases".

Of the 9 articles with 20 or more nodes in the title, 3 start with "and Infectious Diseases":
```
grep("^and Infectious Diseases", txt[len >= 20])
```
So we can resolve these simultaneously.
```
[1] "LatestDocs/PDF/2255202025/1-s2.0-S0147957114000058-main.xml"
[2] "LatestDocs/PDF/1306081813/1-s2.0-S0147957115000788-main.xml"
[3] "LatestDocs/PDF/3861143486/1-s2.0-S0147957116300972-main.xml"
```


The other 6 are
```
names(grep("^and Infectious Diseases", txt[len >= 20], invert = TRUE, value = TRUE))
```
```
[1] "LatestDocs/PDF/1601876396/OIE Iran.xml"                          
[2] "LatestDocs/PDF/2430316441/OIE Kuwait.xml"                        
[3] "LatestDocs/PDF/3814962940/OIE Oman.xml"                          
[4] "LatestDocs/PDF/0818313444/vir.0.81576-0-SuppTableEdited.xml"     
[5] "LatestDocs/PDF/4252077711/J. Virol.-2013-Galvin-JVI.03555-12.xml"
[6] "LatestDocs/PDF/1214934190/Baak et al SWE Mar 2016.xml"           
```
The first three are special reports from OIE; they are not regular journal articles but tables.  We should identify these and use a separate approach
for these.


+ "2853897527/978-1-4020-6106-6.pdf" is supplementary material document and consists of tables.
The footer contains the bibliographic information for the paper, including the title. But there is
no point in programmatically extracting it.

+ 
This is a special format for an article, specifically proofs. So there are numbers on each line.
We get the title and also the author list and other meta data from the entire page.
The true title is 
"Identification of Recombination in the Envelope Gene of Simian Foamy Virus Serotype 2 Isolated from Macaca cyclopis (SFVmcy-2)"

+ "LatestDocs/PDF/1214934190/Baak et al SWE Mar 2016.xml"           
We are collecting up the entire first page. But the title is clearly in bold.

## Fixes
So there several things to fix
1. The "and Infectious Diseases" prefix.
2. [done] OIE reports 
3. [done] "4252077711/J. Virol.-2013-Galvin-JVI.03555-12.xml" & "LatestDocs/PDF/1214934190/Baak et al SWE
Mar 2016.xml" to get the title correctly and not include the other material.


## #1 "and Infectious Diseases".

It is clear that this text inside the grey box at the top of the page.
The splitElsevierTitle() function attempts to find this.
It does find the box and computes where it is so we keep nodes below that.
The box is actually a line with a large line width.
We were not adding 1/2 the line width to the line's horizontal location
which is the center of the box. So adding .5 * linewidth gives us the bottom
of the box and now the "and Infectious Disease" nodes are above that.
Problem solved.


## #2

See getOIETitle() for #2



### #3, 
We implemented the isBold() methods so that we can guess if a font is bold.
This allows us to get the title for 4252077711/J. Virol.-2013-Galvin-JVI.03555-12.xml.
It includes * and space much futher down the page. We can remove those later if necessary
but for our purposes, it is not worth the effort now.

oThis also mostly fixes "LatestDocs/PDF/1214934190/Baak et al SWE Mar 2016.xml".
Again, we pick up other nodes using that font that are further down the page.


We can compute the locations of the resulting nodes (with getBBox2),
sort them and compute the differences. Or order them by line
and see how far apart they are, OR see if there are other nodes in between lines.
(This could get more complex if we are have columns.)
fixTitleNodes() does this now.



## Verify

The next step is to rerun getDocTitle on all of the documents
and see how the results have changed.
We want to see if any of the changes break the results for documents
we thought were originally correct.


## Checking the Titles are Correct.

Eventhough we have some content for the title, we have to check it is correct.




