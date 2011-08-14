-- |
-- Module: Data.XML.Types
-- Copyright: 2010-2011 John Millikin
-- License: MIT
--
-- Maintainer: jmillikin@gmail.com
-- Portability: portable
--
-- Basic types for representing XML.
--
-- The idea is to have a full set of appropriate types, which various XML
-- libraries can share. Instead of having equivalent-but-incompatible types
-- for every binding, parser, or client, they all share the same types can
-- can thus interoperate easily.
--
-- This library contains complete types for most parts of an XML document,
-- including the prologue, node tree, and doctype. Some basic combinators
-- are included for common tasks, including traversing the node tree and
-- filtering children.
--
module Data.XML.Types
	( -- * Types
	
	  -- ** Document prologue
	  Document (..)
	, Prologue (..)
	, Instruction (..)
	, Miscellaneous (..)
	
	-- ** Document body
	, Node (..)
	, Element (..)
	, Content (..)
	, Name (..)
	
	-- ** Doctypes
	, Doctype (..)
	, ExternalID (..)
	
	-- ** Incremental processing
	, Event (..)
	
	-- * Combinators
	
	-- ** Filters
	, isElement
	, isInstruction
	, isContent
	, isComment
	, isNamed
	
	-- ** Element traversal
	, elementChildren
	, elementContent
	, elementText
	
	-- ** Node traversal
	, nodeChildren
	, nodeContent
	, nodeText
	
	-- ** Attributes
	, hasAttribute
	, hasAttributeText
	, attributeContent
	, attributeText
  , Attribute
	) where
import Control.Monad ((>=>))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (IsString, fromString)
import Data.Function (on)
import Data.Typeable ( Typeable, TypeRep, typeOf
                     , mkTyConApp, mkTyCon)

data Document = Document
	{ documentPrologue :: Prologue
	, documentRoot :: Element
	, documentEpilogue :: [Miscellaneous]
	}
	deriving (Show, Eq)

instance Typeable Document where
	typeOf = typeString "Document"

data Prologue = Prologue
	{ prologueBefore :: [Miscellaneous]
	, prologueDoctype :: Maybe Doctype
	, prologueAfter :: [Miscellaneous]
	}
	deriving (Show, Eq)

instance Typeable Prologue where
	typeOf = typeString "Prologue"

data Instruction = Instruction
	{ instructionTarget :: Text
	, instructionData :: Text
	}
	deriving (Show, Eq, Ord)

instance Typeable Instruction where
	typeOf = typeString "Instruction"

data Miscellaneous
	= MiscInstruction Instruction
	| MiscComment Text
	deriving (Show, Eq)

instance Typeable Miscellaneous where
	typeOf = typeString "Miscellaneous"

data Node
	= NodeElement Element
	| NodeInstruction Instruction
	| NodeContent Content
	| NodeComment Text
	deriving (Show, Eq)

instance Typeable Node where
	typeOf = typeString "Node"

type Attribute = (Name, [Content])

data Element = Element
	{ elementName :: Name
	, elementAttributes :: [(Name, [Content])]
	, elementNodes :: [Node]
	}
	deriving (Show, Eq)

instance Typeable Element where
	typeOf = typeString "Element"

data Content
	= ContentText Text
	| ContentEntity Text -- ^ For pass-through parsing
	deriving (Show, Eq)

instance Typeable Content where
	typeOf = typeString "Content"

-- | A fully qualified name.
--
-- Prefixes are not semantically important; they are included only to
-- simplify pass-through parsing. When comparing names with 'Eq' or 'Ord'
-- methods, prefixes are ignored.
--
-- The @IsString@ instance supports Clark notation; see
-- <http://www.jclark.com/xml/xmlns.htm> and
-- <http://infohost.nmt.edu/tcc/help/pubs/pylxml/etree-QName.html>. Use
-- the @OverloadedStrings@ language extension for very simple @Name@
-- construction:
--
-- > myname :: Name
-- > myname = "{http://example.com/ns/my-namespace}my-name"
--
data Name = Name
	{ nameLocalName :: Text
	, nameNamespace :: Maybe Text
	, namePrefix :: Maybe Text
	}
	deriving (Show)

instance Typeable Name where
	typeOf = typeString "Name"

instance Eq Name where
	(==) = (==) `on` (\x -> (nameNamespace x, nameLocalName x))

instance Ord Name where
	compare = compare `on` (\x -> (nameNamespace x, nameLocalName x))

instance IsString Name where
	fromString "" = Name T.empty Nothing Nothing
	fromString full@('{':rest) = case break (== '}') rest of
		(_, "") -> error ("Invalid Clark notation: " ++ show full)
		(ns, local) -> Name (T.pack (drop 1 local)) (Just (T.pack ns)) Nothing
	fromString local = Name (T.pack local) Nothing Nothing

-- | Note: due to the incredible complexity of DTDs, this type only supports
-- external subsets. I've tried adding internal subset types, but they
-- quickly gain more code than the rest of this module put together.
--
-- It is possible that some future version of this library might support
-- internal subsets, but I am no longer actively working on adding them.
data Doctype = Doctype
	{ doctypeName :: Text
	, doctypeID :: Maybe ExternalID
	}
	deriving (Show, Eq, Ord)

instance Typeable Doctype where
	typeOf = typeString "Doctype"

data ExternalID
	= SystemID Text
	| PublicID Text Text
	deriving (Show, Eq, Ord)

instance Typeable ExternalID where
	typeOf = typeString "ExternalID"

-- | Some XML processing tools are incremental, and work in terms of events
-- rather than node trees. The 'Event' type allows a document to be fully
-- specified as a sequence of events.
--
-- Event-based XML libraries include:
--
-- * <http://hackage.haskell.org/package/xml-enumerator>
--
-- * <http://hackage.haskell.org/package/libxml-enumerator>
--
-- * <http://hackage.haskell.org/package/expat-enumerator>
--
data Event
	= EventBeginDocument
	| EventEndDocument
	| EventBeginDoctype Text (Maybe ExternalID)
	| EventEndDoctype
	| EventInstruction Instruction
	| EventBeginElement Name [(Name, [Content])]
	| EventEndElement Name
	| EventContent Content
	| EventComment Text
	| EventCDATA Text
	deriving (Show, Eq)

instance Typeable Event where
	typeOf = typeString "Event"

isElement :: Node -> [Element]
isElement (NodeElement e) = [e]
isElement _ = []

isInstruction :: Node -> [Instruction]
isInstruction (NodeInstruction i) = [i]
isInstruction _ = []

isContent :: Node -> [Content]
isContent (NodeContent c) = [c]
isContent _ = []

isComment :: Node -> [Text]
isComment (NodeComment t) = [t]
isComment _ = []

isNamed :: Name -> Element -> [Element]
isNamed n e = [e | elementName e == n]

elementChildren :: Element -> [Element]
elementChildren = elementNodes >=> isElement

elementContent :: Element -> [Content]
elementContent = elementNodes >=> isContent

elementText :: Element -> [Text]
elementText = elementContent >=> contentText

nodeChildren :: Node -> [Node]
nodeChildren = isElement >=> elementNodes

nodeContent :: Node -> [Content]
nodeContent = nodeChildren >=> isContent

nodeText :: Node -> [Text]
nodeText = nodeContent >=> contentText

hasAttribute :: Name -> Element -> [Element]
hasAttribute name e = [e | isJust (attributeContent name e)]

hasAttributeText :: Name -> (Text -> Bool) -> Element -> [Element]
hasAttributeText name p e = [e | maybe False p (attributeText name e)]

attributeContent :: Name -> Element -> Maybe [Content]
attributeContent name e = lookup name (elementAttributes e)

attributeText :: Name -> Element -> Maybe Text
attributeText name e = fmap contentFlat (attributeContent name e)

contentText :: Content -> [Text]
contentText (ContentText t) = [t]
contentText (ContentEntity entity) = [T.pack "&", entity, T.pack ";"]

contentFlat :: [Content] -> Text
contentFlat cs = T.concat (cs >>= contentText)

typeString :: String -> a -> TypeRep
typeString str _ = mkTyConApp (mkTyCon ("Data.XML.Types." ++ str)) []
