{-# LANGUAGE Rank2Types, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Lens
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
-- Useful traversals inspired by XPath
----------------------------------------------------------------------------
module Text.XML.Lens (
    -- * Lenses, traversals for 'Element'
    (./)
    -- ** Names
    , name
    , el
    -- ** Attributes
    , attributeIs
    , attributeSatisfies
    , attr
    , attribute
    , attrs
    -- ** Contents
    , text
    , comment
    -- ** Children
    , entire
    , nodes
    -- * Prisms for 'Node'
    , _Element
    , _Content
    , AsInstruction(..)
    , AsComment(..)
    -- * Lenses for 'Document'
    , root
    , prologue
    , epilogue
    , doctype
    -- * Lenses for 'Name'
    , _nameLocalName
    , _nameNamespace
    , _namePrefix
    -- * Lenses for 'Instruction'
    , _instructionTarget
    , _instructionData
    ) where
import Text.XML
import Control.Lens
import Data.Text (Text)
import Data.Map (Map)
import Control.Applicative

infixr 9 ./

prologue :: Lens' Document Prologue
prologue f doc = fmap (\p -> doc { documentPrologue = p} ) $ f $ documentPrologue doc

-- | The root element of the document.
root :: Lens' Document Element
root f doc = fmap (\p -> doc { documentRoot = p} ) $ f $ documentRoot doc

epilogue :: Lens' Document [Miscellaneous]
epilogue f doc = fmap (\p -> doc { documentEpilogue = p} ) $ f $ documentEpilogue doc

doctype :: Lens' Prologue (Maybe Doctype)
doctype f doc = fmap (\p -> doc { prologueDoctype = p} ) $ f $ prologueDoctype doc

class AsInstruction t where
    _Instruction :: Prism' t Instruction

_instructionTarget :: Lens' Instruction Text
_instructionTarget f (Instruction t d) = f t <&> \t' -> Instruction t' d

_instructionData :: Lens' Instruction Text
_instructionData f (Instruction t d) = f d <&> \d' -> Instruction t d'

instance AsInstruction Node where
    _Instruction = prism' NodeInstruction $ \s -> case s of
        NodeInstruction e -> Just e
        _ -> Nothing

instance AsInstruction Miscellaneous where
    _Instruction = prism' MiscInstruction $ \s -> case s of
        MiscInstruction e -> Just e
        _ -> Nothing

class AsComment t where
    _Comment :: Prism' t Text

instance AsComment Node where
    _Comment = prism' NodeComment $ \s -> case s of
        NodeComment e -> Just e
        _ -> Nothing

instance AsComment Miscellaneous where
    _Comment = prism' MiscComment $ \s -> case s of
        MiscComment e -> Just e
        _ -> Nothing

_nameLocalName :: Lens' Name Text
_nameLocalName f n = fmap (\x -> n { nameLocalName = x }) $ f $ nameLocalName n

_nameNamespace :: Lens' Name (Maybe Text)
_nameNamespace f n = fmap (\x -> n { nameNamespace = x }) $ f $ nameNamespace n

_namePrefix :: Lens' Name (Maybe Text)
_namePrefix f n = fmap (\x -> n { namePrefix = x }) $ f $ namePrefix n

_Element :: Prism' Node Element
_Element = prism' NodeElement $ \s -> case s of
    NodeElement e -> Just e
    _ -> Nothing

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \s -> case s of
    NodeContent e -> Just e
    _ -> Nothing

name :: Lens' Element Name
name f e = fmap (\x -> e { elementName = x }) $ f $ elementName e

attrs :: Lens' Element (Map Name Text)
attrs f e = fmap (\x -> e { elementAttributes = x }) $ f $ elementAttributes e

nodes :: Lens' Element [Node]
nodes f e = fmap (\x -> e { elementNodes = x }) $ f $ elementNodes e

attr :: Name -> IndexedTraversal' Name Element Text
attr n = attrs . ix n

attribute :: Name -> IndexedLens' Name Element (Maybe Text)
attribute n = attrs . at n

-- | Traverse itself with its all children.
entire :: Traversal' Element Element
entire f e@(Element _ _ ns) = com <$> f e <*> traverse (_Element (entire f)) ns where
    com (Element n a _) ns = Element n a ns

-- | Traverse elements which has the specified name.
el :: Name -> Traversal' Element Element
el n f s
    | elementName s == n = f s
    | otherwise = pure s

attributeSatisfies :: Name -> (Text -> Bool) -> Traversal' Element Element
attributeSatisfies n p = filtered (maybe False p . preview (attrs . ix n))

attributeIs :: Name -> Text -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (==v)

-- | Traverse all contents of the element.
text :: Traversal' Element Text
text = nodes . traverse . _Content

-- | Traverse all comments of the element.
comment :: Traversal' Element Text
comment = nodes . traverse . _Comment

instance Plated Element where
    plate = nodes . traverse . _Element

-- | Combine two 'Traversal's just like XPath's slash.
-- 
-- @ 
-- l ./ m ≡ l . 'plate' . m
-- @
(./) :: Plated a => Traversal s t a a -> Traversal a a u v -> Traversal s t u v
l ./ m = l . plate . m
