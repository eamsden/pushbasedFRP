-- Signal Vectors
data SVEmpty
data SVSignal :: * -> *
data SVEvent  :: * -> *
data SVAppend :: * -> * -> *

-- Signal Vector indexes
data SVIndex :: (* -> *) -> * -> * where
  SVISignal :: a -> SVIndex p (SVSignal p a)
  SVIEvent  :: a -> SVIndex p (SVEvent p a)
  SVILeft   :: SVIndex p svl -> SVIndex p (SVAppend svl svr)
  SVIRight  :: SVIndex p svr -> SVIndex p (SVAppend svl svr)

