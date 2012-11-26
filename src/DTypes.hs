module DTypes where


userAgent :: String
userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1309.0 Safari/537.17"

type ElementType = String
type ElementClass = String
type ElementAttribute = String

data ElementSpec = ElementSpec {
      elementType :: Maybe String, -- ie, div, span, etc
      elementAttribute :: Maybe String, -- ie, id, class, etc
      elementAttributeValue :: Maybe String,
      elementValue :: Maybe String -- where to get the value. If Nothing, it's INSIDE the tag.

} deriving (Show) 


data Hint = Hint {
      productsBlock :: Maybe ElementSpec, -- Matches a repeating element that contains products
      productBlock  :: Maybe ElementSpec, -- Matches the string that describes the product
      priceBlock    :: Maybe ElementSpec -- Matches the price of the product
} deriving (Show)


-- | Support functions for our graph
data Color = Black | White | Red | Grey
type CategoryMatcher = (String -> Maybe Hint -> Bool)

-- | Basic ADT for our graph
data Node = Node {
      category   :: String,
      parentCategory :: Maybe Node,
      depth      :: Integer,
      color      :: Color,
      matcher    :: CategoryMatcher
 } 

instance Show Color where
    show Black = "Black"
    show White = "White"
    show Red = "Red"
    show Grey = "Grey"

instance Show Node where
    show n = "Node Category [" ++ category n ++  "], Depth [" ++ show (depth n) ++ "], Color [" ++ show (color n) ++"]"


-- | A "root" should also be a special type of node
type Root = Node


-- | Some helper functions for creating new nodes
newNode :: String -> CategoryMatcher -> Maybe Node -> Node
newNode categoryName mf parent = Node {category = categoryName, parentCategory = parent, depth = 0, color = White, matcher = mf}

newRoot :: Root
newRoot = Node { category = "root", parentCategory = Nothing, depth = 0, color = Red, matcher = (\s h -> True)}


{- | 
The nodes in our graph as purely as they can be defined.
-}

-- default dummy matcher
dm :: CategoryMatcher
dm s h = False

dmf :: CategoryMatcher
dmf s h = True






{- |
The tax graph is a lookup structure of 3-tuples. 

1) The first element is the node name/category
2) The second element is the node itself
3) The third element is the list of nodes adjacent to this node

-}
type Graph = [(String, (Node, [Node]))]

