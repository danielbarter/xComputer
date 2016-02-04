module PpComputerState (ppComputerState) where

import Text.PrettyPrint
import Numeric (showHex)
import Machine
import Data.Map ((!))

p x | l == 1 = "000" ++ s
    | l == 2 = "00"  ++ s
    | l == 3 = "0"   ++ s
    | l == 4 =          s
  where s = showHex x ""
        l = length s

ppX :: X -> Doc
ppX x = (text "X :") <+> (text $ p x)  

ppY :: Y -> Doc
ppY y = (text "Y :") <+> (text $ p y)

ppFLAG :: FLAG -> Doc
ppFLAG f = (text "FLAG :") <+> (text $ show f)

ppAC :: AC -> Doc
ppAC ac = (text "AC :") <+> (text $ p ac)

ppPC :: PC -> Doc
ppPC pc = (text "PC :") <+> (text $ show pc)

ppIR :: IR -> Doc
ppIR ir = (text "IR :") <+> (text $ p ir)

ppMEM :: MEM -> Doc
ppMEM m = vcat $ fmap hsep $ group 32 [ text $ p (m ! k) | k <- [0,1..1023]]

group :: Int -> [a] -> [[a]]
group n [] = []
group n l  = h : (group n t)
  where (h,t) = splitAt n l

ppComputerState' :: XComputerState -> Doc
ppComputerState' (x,y,flag,ac,count,pc,ir,addr,mem) = 
  (foldr1 (\x y -> x <+> text "    " <+> y) [ppX x, ppY y, ppFLAG flag, ppAC ac, ppPC pc, ppIR ir]) $$ char ' ' $$ ppMEM mem 

ppComputerState = render . ppComputerState'
