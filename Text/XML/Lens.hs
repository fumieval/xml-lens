{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Lens
-- Copyright   :  (C) 2015 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.XML.Lens (
    -- * Lenses, traversals for 'Element'
    Element(..)
    , (./)
    , (...)
    -- ** Names
    , name
    , localName
    , el
    , ell
    , named
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
    , Node(..)
    , _Element
    , _Content
    , AsInstruction(..)
    , AsComment(..)
    -- * Lenses for 'Document'
    , Document(..)
    , root
    , prologue
    , epilogue
    , doctype
    -- * Lenses for 'Name'
    , Name(..)
    , _nameLocalName
    , _nameNamespace
    , _namePrefix
    -- * Lenses for 'Instruction'
    , Instruction(..)
    , _instructionTarget
    , _instructionData
    ) where
import Text.XML
import Control.Lens
import Data.Text (Text)
import Data.Map (Map)
import Control.Applicative
import qualified Data.CaseInsensitive as CI

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
_nameLocalName f n = f (nameLocalName n) <&> \x -> n { nameLocalName = x }

_nameNamespace :: Lens' Name (Maybe Text)
_nameNamespace f n = f (nameNamespace n) <&> \x -> n { nameNamespace = x }

_namePrefix :: Lens' Name (Maybe Text)
_namePrefix f n = f (namePrefix n) <&> \x -> n { namePrefix = x }

_Element :: Prism' Node Element
_Element = prism' NodeElement $ \s -> case s of
    NodeElement e -> Just e
    _ -> Nothing

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \s -> case s of
    NodeContent e -> Just e
    _ -> Nothing

name :: Lens' Element Name
name f e = f (elementName e) <&> \x -> e { elementName = x }

localName :: Lens' Element Text
localName = name . _nameLocalName
{-# INLINE localName #-}

attrs :: Lens' Element (Map Name Text)
attrs f e = fmap (\x -> e { elementAttributes = x }) $ f $ elementAttributes e

nodes :: Lens' Element [Node]
nodes f e = fmap (\x -> e { elementNodes = x }) $ f $ elementNodes e

attr :: Name -> Traversal' Element Text
attr n = attrs . ix n

attribute :: Name -> Lens' Element (Maybe Text)
attribute n = attrs . at n

-- | Traverse itself with its all children.　Rewriting subnodes of each children will break a traversal law.
entire :: Traversal' Element Element
entire f e@(Element _ _ ns) = com <$> f e <*> traverse (_Element (entire f)) ns where
    com (Element n a _) = Element n a
{-# DEPRECATED entire "Use cosmos or deep instead" #-}

-- | Traverse elements which has the specified *local* name (case-insensitive).
named :: CI.CI Text -> Traversal' Element Element
named n f s
    | CI.mk (nameLocalName (elementName s)) == n = f s
    | otherwise = pure s

-- | Old name for 'named'
ell :: Text -> Traversal' Element Element
ell = named . CI.mk

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

-- | 'plate' traverses over its sub-elements.
instance Plated Element where
    plate = nodes . traverse . _Element

-- | Combine two 'Traversal's just like XPath's slash. Identical to ('...').
--
-- @
-- l ./ m ≡ l . 'plate' . m
-- @
(./) :: (Applicative f, Plated c) => LensLike f s t c c -> Over p f c c a b -> Over p f s t a b
(./) = (...)
{-# INLINE (./) #-}

{-# DEPRECATED (./) "Use (...) instead" #-}
