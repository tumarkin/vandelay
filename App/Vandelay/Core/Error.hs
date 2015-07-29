-- module App.Vandelay.Core.Error 
--   ( VLError(..)
--   )
-- where

-- import Text.Parsec.Error (ParseError)

-- data VLError = FileNotFound String String -- Filename, other info
--              | GlobPatternError String -- Pattern
--              | UserHalt 
--              | VLParseError ParseError
--              | VLError String -- Message

-- instance Show VLError where

--   show (FileNotFound f e  ) = unwords [e,f,"not found."]
--   show (GlobPatternError s) = unwords ["No files found matching pattern",s]
--   show (UserHalt)           = "Execution halted."

