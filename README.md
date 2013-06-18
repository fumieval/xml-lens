xml-lens
========

Lenses and traversals for [xml-conduit](http://hackage.haskell.org/package/xml-conduit).

Example
========

> doc <- Text.XML.readFile def "examples\\books.xml"

> doc ^.. root . el "books" ./ el "book" ./ el "title" . text
["Haskell 98 language and libraries: the Revised Report","Learn You a Haskell for Great Good!","Programming in Haskell","Real World Haskell","The Fun of Program
ming","Types and Programming Languages","Functional Ikamusume"]

> doc ^.. root . el "books" ./ el "book" . attributeIs "category" "Textbooks" ./ el "title" . text
["Learn You a Haskell for Great Good!","Programming in Haskell","Real World Haskell"]
