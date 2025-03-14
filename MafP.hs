import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Applicative (asum)

type Parser s = Parsec Void s

spaces = many spaceChar
umhülle x = spaces *> x <* spaces

plus = asum $ map try [(+) <$> (mals <* plusZeichen) <*> plus, mals]
  where
    plusZeichen = umhülle $ single '+'

mals = asum $ map try [(*) <$> (hoch <* malZeichen) <*> mals, hoch]
  where
    malZeichen = umhülle $ single '*'

hoch = asum $ map try [(**) <$> (atom <* hochZeichen) <*> hoch, atom]
  where
    hochZeichen = umhülle $ string "**"
                      
atom = asum $ map try $ [lb *> plus <* rb
                          , negate <$> (negTok *> atom)
                          , recip <$> (recTok *> atom)
                          , piP
                          , gleitkomma
                          , num]
  where
    [lb, rb] = umhülle . single <$> "()"
    negTok = umhülle $ single 'n'
    recTok = umhülle $ single 'r'

    piP = pi <$ umhülle (string "π")
    num :: Read a => Parser String a
    num = read <$> some digitChar
    gleitkomma = comb <$> (num <* punkt) <*> num
      where
        comb a b = read $ a <> "." <> b
        punkt = umhülle $ single '.'

run = runParser plus "happy π day" "π * 42 ** 2"
