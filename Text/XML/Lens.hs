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
    , attributeSatisfies'
    , withoutAttribute
    , attr
    , attribute
    , attrs
    -- ** Contents
    , text
    , comment
    -- ** Children
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
import qualified Data.CaseInsensitive as CI
import Data.Maybe (isNothing)

prologue :: Lens' Document Prologue
prologue f doc = fmap (\p -> doc { documentPrologue = p} ) $ f $ documentPrologue doc
{-# INLINE prologue #-}

-- | The root element of the document.
root :: Lens' Document Element
root f doc = fmap (\p -> doc { documentRoot = p} ) $ f $ documentRoot doc
{-# INLINE root #-}

epilogue :: Lens' Document [Miscellaneous]
epilogue f doc = fmap (\p -> doc { documentEpilogue = p} ) $ f $ documentEpilogue doc
{-# INLINE epilogue #-}

doctype :: Lens' Prologue (Maybe Doctype)
doctype f doc = fmap (\p -> doc { prologueDoctype = p} ) $ f $ prologueDoctype doc
{-# INLINE doctype #-}

class AsInstruction t where
    _Instruction :: Prism' t Instruction

_instructionTarget :: Lens' Instruction Text
_instructionTarget f (Instruction t d) = f t <&> \t' -> Instruction t' d
{-# INLINE _instructionTarget #-}

_instructionData :: Lens' Instruction Text
_instructionData f (Instruction t d) = f d <&> \d' -> Instruction t d'
{-# INLINE _instructionData #-}

instance AsInstruction Node where
    _Instruction = prism' NodeInstruction $ \s -> case s of
        NodeInstruction e -> Just e
        _ -> Nothing
    {-# INLINE _Instruction #-}

instance AsInstruction Miscellaneous where
    _Instruction = prism' MiscInstruction $ \s -> case s of
        MiscInstruction e -> Just e
        _ -> Nothing
    {-# INLINE _Instruction #-}

class AsComment t where
    _Comment :: Prism' t Text

instance AsComment Node where
    _Comment = prism' NodeComment $ \s -> case s of
        NodeComment e -> Just e
        _ -> Nothing
    {-# INLINE _Comment #-}

instance AsComment Miscellaneous where
    _Comment = prism' MiscComment $ \s -> case s of
        MiscComment e -> Just e
        _ -> Nothing
    {-# INLINE _Comment #-}

_nameLocalName :: Lens' Name Text
_nameLocalName f n = f (nameLocalName n) <&> \x -> n { nameLocalName = x }
{-# INLINE _nameLocalName #-}

_nameNamespace :: Lens' Name (Maybe Text)
_nameNamespace f n = f (nameNamespace n) <&> \x -> n { nameNamespace = x }
{-# INLINE _nameNamespace #-}

_namePrefix :: Lens' Name (Maybe Text)
_namePrefix f n = f (namePrefix n) <&> \x -> n { namePrefix = x }
{-# INLINE _namePrefix #-}

_Element :: Prism' Node Element
_Element = prism' NodeElement $ \s -> case s of
    NodeElement e -> Just e
    _ -> Nothing
{-# INLINE _Element #-}

_Content :: Prism' Node Text
_Content = prism' NodeContent $ \s -> case s of
    NodeContent e -> Just e
    _ -> Nothing
{-# INLINE _Content #-}

name :: Lens' Element Name
name f e = f (elementName e) <&> \x -> e { elementName = x }
{-# INLINE name #-}

localName :: Lens' Element Text
localName = name . _nameLocalName
{-# INLINE localName #-}

attrs :: Lens' Element (Map Name Text)
attrs f e = fmap (\x -> e { elementAttributes = x }) $ f $ elementAttributes e
{-# INLINE attrs #-}

nodes :: Lens' Element [Node]
nodes f e = fmap (\x -> e { elementNodes = x }) $ f $ elementNodes e
{-# INLINE nodes #-}

attr :: Name -> Traversal' Element Text
attr n = attrs . ix n
{-# INLINE attr #-}

attribute :: Name -> Lens' Element (Maybe Text)
attribute n = attrs . at n
{-# INLINE attribute #-}

-- | Traverse elements which has the specified *local* name (case-insensitive).
named :: CI.CI Text -> Traversal' Element Element
named n f s
    | CI.mk (nameLocalName (elementName s)) == n = f s
    | otherwise = pure s
{-# INLINE named #-}

-- | Old name for 'named'
ell :: Text -> Traversal' Element Element
ell = named . CI.mk

-- | Traverse elements which has the specified name.
el :: Name -> Traversal' Element Element
el n f s
    | elementName s == n = f s
    | otherwise = pure s
{-# DEPRECATED el "Use named instead" #-}

attributeSatisfies :: Name -> (Text -> Bool) -> Traversal' Element Element
attributeSatisfies n p = attributeSatisfies' n (maybe False p)
{-# INLINE attributeSatisfies #-}

attributeSatisfies' :: Name -> (Maybe Text -> Bool) -> Traversal' Element Element
attributeSatisfies' n p = filtered (p . preview (attrs . ix n))
{-# INLINE attributeSatisfies' #-}

withoutAttribute :: Name -> Traversal' Element Element
withoutAttribute n = attributeSatisfies' n isNothing
{-# INLINE withoutAttribute #-}

attributeIs :: Name -> Text -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (==v)
{-# INLINE attributeIs #-}

-- | Traverse all contents of the element.
text :: Traversal' Element Text
text = nodes . traverse . _Content
{-# INLINE text #-}

-- | Traverse all comments of the element.
comment :: Traversal' Element Text
comment = nodes . traverse . _Comment
{-# INLINE comment #-}

-- | 'plate' traverses over its sub-elements.
instance Plated Element where
    plate = nodes . traverse . _Element
    {-# INLINE plate #-}