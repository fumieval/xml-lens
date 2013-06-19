xml-lens
========

Lenses and traversals for [xml-conduit](http://hackage.haskell.org/package/xml-conduit).

Example
========

    > doc <- Text.XML.readFile def "examples\\books.xml"

    > doc ^.. root . el "books" ./ el "book" . attributeIs "category" "Textbooks" ./ el "title" . text
    ["Learn You a Haskell for Great Good!","Programming in Haskell","Real World Haskell"]

    > lengthOf ?? doc $ root . el "books" ./ el "book"
    7

    > doc ^? root . el "books" ./ attributeIs "category" "Joke" ./ el "title" . text
    Just "Functional Ikamusume"

    > doc & root . el "books" ./ el "book" ./ el "pages" . text <>~ " pages" & renderLBS def & BL.putStrLn

```xml
<?xml version="1.0" encoding="ISO-8859-1"?>
<books>
<book category="Language and library definition">
    <title>Haskell 98 language and libraries: the Revised Report</title>
    <author year="2003">Simon Peyton Jones</author>
    <pages>272 pages</pages>
    <price>£45.00</price>
</book>
<book category="Textbooks">
    <title>Learn You a Haskell for Great Good!</title>
    <author year="2011">Miran Lipovaca</author>
    <pages>360 pages</pages>
</book>
<book category="Textbooks">
    <title>Programming in Haskell</title>
    <author year="2007">Graham Hutton</author>
    <pages>200 pages</pages>
</book>
…
```


