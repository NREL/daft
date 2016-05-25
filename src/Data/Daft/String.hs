module Data.Daft.String (
  readEither
) where


-- Read a string or return a message that it cannot be parsed.
readEither :: Read a => String -> Either String a
readEither x =
  case reads x of
    [(result, [])] -> Right result
    _              -> Left $ "failed to parse \"" ++ x ++ "\""
