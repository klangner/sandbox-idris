module DataFrame

import Data.Vect


data Index : Type where
  MkIndex : (Vect n a) -> Index

data Column : Type where
  MkCol : String -> Vect n a -> Column


data DataFrame = DF Index (List Column)
