-- "Identity" type constructor
newtype Id a = Id a

-- Signal Function (initialized) datatype
data SFInitialized :: * -> * -> * where
  SFInitialized :: (DTime -> ([SVIndex Id svOut], SFInitialized svIn svOut)) -> 
                   (SVIndex Id svIn -> ([SVIndex Id svOut], SFInitialized svIn svOut)) ->
                   SFInitialized svIn svOut

