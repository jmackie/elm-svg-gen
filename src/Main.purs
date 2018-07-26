module Main where

import Prelude

import Control.Alt ((<|>))

import Data.Array as Array
import Data.Char.Unicode as Char
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String
import Data.String.Pattern (Pattern(Pattern))
import Data.Traversable (traverse, intercalate)
import Data.Tuple (Tuple(Tuple), fst)

import Effect (Effect)
import Effect.Console (log)

import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath, basenameWithoutExt)
import Node.Process (argv)

import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS


main :: Effect Unit
main = do
    files <- Array.drop 2 <$> argv
    mod <- mkModule <$> traverse process files
    log mod


mkModule :: Array String -> String
mkModule fns = intercalate "\n\n" $
    [ "module Icons exposing (..)"
    , "import Svg"
    , "import Svg.Attributes"
    ] <> fns


process :: FilePath -> Effect String
process path = do
    contents <- readTextFile UTF8 path
    let result = P.runParser contents (PC.optional prolog *> space *> parseTagTree)
    case result of
         Left err -> pure (show (Tuple path err))
         Right tag -> pure (renderSvgFunction (mkFunctionName path) tag)
  where
    mkFunctionName =
        flip basenameWithoutExt ".svg" >>> toCamelCase'


data Tag = Tag
    { name :: String
    , attrs :: Array (Tuple String String)
    , children :: Array Tag
    }


renderSvgFunction :: String -> Tag -> String
renderSvgFunction name tag = intercalate "\n"
    [ name <> " : List (Svg.Attribute msg) -> Svg.Svg msg"
    , name <> " attrs = " <> renderSvg 0 tag
    ]


renderSvg :: Int -> Tag -> String
renderSvg depth (Tag { name, attrs, children }) =
    intercalate " "
        [ renderTag name
        , if depth == 0 then
            paren (bracket (commaJoin (renderAttr <$> attrs)) <> " ++ attrs")
          else
            bracket (commaJoin (renderAttr <$> attrs))
        , bracket (commaJoin (renderSvg (depth + 1) <$> children))
        ]
  where
    commaJoin = intercalate ", "
    paren   s = "(" <> s <> ")"
    bracket s = "[" <> s <> "]"
    renderTag = ("Svg." <> _)
    renderAttr (Tuple key value) =
        "Svg.Attributes." <> key <> " " <> show value


{- PARSING -}


type Parser = P.Parser String


parseTagTree :: Parser Tag
parseTagTree = do
    _        <- symbol "<"
    name     <- ident
    attrs    <- Array.many attr <#>
        Array.filter (not <<< startsWith "xmlns" <<< fst)
    end      <-
        Left <$> symbol ">" <|> Right <$> symbol "/>"
    children <-
        case end of
            Left  _ -> do
                inner <- Right <$> (parseTagTree `PC.manyTill` close name)
                     <|> Left <$> (PS.satisfy (_ /= '<') `PC.manyTill` close name)
                case inner of
                    Left _ -> pure []
                    Right children -> pure (Array.fromFoldable children)
            Right _ -> pure []
    pure $ Tag { name, attrs, children }
  where
    close name = lexeme do
       _ <- symbol "</"
       _ <- symbol name
       _ <- symbol ">"
       pure unit

    attr = do
       key   <- ident
       _     <- PS.char '='
       value <- stringLit
       pure $ Tuple key value

    stringLit = lexeme do
       _     <- PS.char '"'
       chars <- PC.manyTill PS.anyChar (PS.char '"')
       pure (String.fromCharArray $ Array.fromFoldable chars)

    ident = lexeme do
       chars <- Array.some identChar
       pure $ toCamelCase' (String.fromCharArray chars)

    identChar  =
        PS.oneOf ['-', '_', ':'] <|> PS.satisfy Char.isAlpha


lexeme :: forall a . Parser a -> Parser a
lexeme p = p <* space


symbol :: String -> Parser String
symbol str = PS.string str <* space


space :: Parser Unit
space = PC.skipMany (PC.skipMany1 (PS.satisfy Char.isSpace) <|> comment)
  where
    comment = do
        _ <- PS.string "<!--"
        _ <- PS.anyChar `PC.manyTill` PS.string "-->"
        pure unit


prolog :: Parser Unit
prolog = do
    _ <- PS.string "<?"
    _ <- PS.anyChar `PC.manyTill` PS.string "?>"
    pure unit


toCamelCase' :: String -> String
toCamelCase' = toCamelCase (flip elem [':', '_', '-'])


toCamelCase :: (Char -> Boolean) -> String -> String
toCamelCase p =
    String.toCharArray
    >>> List.fromFoldable
    >>> go
    >>> Array.fromFoldable
    >>> String.fromCharArray
  where
    go List.Nil       = List.Nil
    go (c : c' : rest)
        | p c       = Char.toUpper c' : go rest
        | otherwise = c : go (c' : rest)
    go (c : rest)
        | p c       = go rest
        | otherwise = c : go rest


startsWith :: String -> String -> Boolean
startsWith prefix str = String.indexOf (Pattern prefix) str == Just 0
