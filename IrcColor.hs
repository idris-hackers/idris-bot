module IrcColor where

type StyleCmd = String
type ColorCode = String

bold, underlined, italic, reset :: StyleCmd
bold = "\x02"
underlined = "\x1F"
italic = "\x16"
reset = "\x0F"

white, black, blue, green, red, maroon, purple, orange, yellow, lightGreen, teal, cyan, lightBlue, pink, grey, lightGrey :: ColorCode
white = "00"
black = "01"
blue = "02"
green = "03"
red = "04"
maroon = "05"
purple = "06"
orange = "07"
yellow = "08"
lightGreen = "09"
teal = "10"
cyan = "11"
lightBlue = "12"
pink = "13"
grey = "14"
lightGrey = "15"

colorHeader = "\x03"

color :: ColorCode -> Maybe ColorCode -> StyleCmd
color fg Nothing = colorHeader ++ fg
color fg (Just bg) = colorHeader ++ fg ++ ',' : bg

applyStyle :: StyleCmd -> String -> String
applyStyle cmd str = cmd ++ str ++ reset
