module Explorer.Util.String where

foreign import substituteImpl :: String -> Array String -> String

-- | Substitutes `{0}` placeholders of a string
-- | * `substitute "Hello {0}, what's going on {1}" ["Jane", "today"]`
-- | * `-- output: "Hello Jane, what's going on today"`
substitute :: String -> Array String -> String
substitute = substituteImpl
