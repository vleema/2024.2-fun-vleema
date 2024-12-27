{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits (Nat, type (+))

data Color = Red | Black

-- A RBTree encodes both the color and the "black-height" of the tree in its type.
data RBTree (c :: Color) (h :: Nat) a where
  Nil :: RBTree 'Black 0 a
  RedNode :: RBTree 'Black h a -> a -> RBTree 'Black h a -> RBTree 'Red h a
  BlackNode :: RBTree c h a -> a -> RBTree c h a -> RBTree 'Black (h + 1) a

-- Example: A leaf is always a black node with black-height 0:
-- Leaf :: RBTree 'B 0 a

-- A red node has red children replaced by black nodes, ensuring no consecutive reds.
-- A black node increments black-height.

-- You would also define smart constructors and operations to ensure these invariants.
