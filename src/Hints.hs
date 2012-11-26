module Hints where


import DTypes

{- |

For a given site, we can define a "hint" for it which helps us to parse tokens and extract text.

This is just for HTML at the moment, but we should define parsers for other types. For example, newegg lets us do the following:

http://www.ows.newegg.com/Products.egg/N82E16834127693/Specification

And the results are IN JSON format. SOMETHING TO THINK ABOUT!

-}

hints :: String -> Maybe Hint
hints site = lookup site [
              ("newegg.com",  
                           Hint 
                               {
                                 productsBlock = Nothing,
                                 productBlock = Just ElementSpec 
                                                {
                                                  elementType = Just "span"
                                                , elementAttribute = Just "itemprop"
                                                , elementAttributeValue = Just "name"
                                                , elementValue = Nothing 
                                                },
                                 priceBlock = Just ElementSpec 
                                              {
                                                elementType = Just "div"
                                              , elementAttribute = Just "itemprop"
                                              , elementAttributeValue = Just "price"
                                              , elementValue = Just "content"
                                              }
                               }
              ),

              ("bestbuy.com",  
                           Hint 
                               {
                                 productsBlock = Nothing,
                                 productBlock = Just ElementSpec 
                                                {
                                                  elementType = Just "div"
                                                , elementAttribute = Just "id"
                                                , elementAttributeValue = Just "sku-title"
                                                , elementValue = Nothing 
                                                },
                                 priceBlock = Just ElementSpec 
                                              {
                                                elementType = Just "div"
                                              , elementAttribute = Just "id"
                                              , elementAttributeValue = Just "item-price"
                                              , elementValue = Nothing
                                              }
                               }
              ),


              -- more hints here.
              ("cdw.com",  
                           Hint 
                               {
                                 productsBlock = Nothing,
                                 productBlock = Just ElementSpec 
                                                {
                                                  elementType = Just "span"
                                                , elementAttribute = Just "id"
                                                , elementAttributeValue = Just "_primaryProductInformation__titleModule__productName"
                                                , elementValue = Nothing 
                                                },
                                 priceBlock = Just ElementSpec 
                                              {
                                                elementType = Just "span"
                                              , elementAttribute = Just "id"
                                              , elementAttributeValue = Just "_primaryProductInformation__price_lblAP"
                                              , elementValue = Nothing
                                              }
                               }
              ),

              ("target.com",  
                           Hint 
                               {
                                 productsBlock = Nothing,
                                 productBlock = Just ElementSpec 
                                                {
                                                  elementType = Just "span"
                                                , elementAttribute = Just "itemprop"
                                                , elementAttributeValue = Just "name"
                                                , elementValue = Nothing 
                                                },
                                 priceBlock = Just ElementSpec 
                                              {
                                                elementType = Just "span"
                                              , elementAttribute = Just "class"
                                              , elementAttributeValue = Just "offerPrice"
                                              , elementValue = Nothing
                                              }
                               }
              )
              ]


