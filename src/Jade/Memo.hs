module Jade.Memo where







emptyMemo = Memo DM.empty
emptyTopl = TopLevel DM.empty

getMemo :: J Memo
getMemo = globalMemo <$> get

putMemo memo = do
  Global x _ <- get
  put $ Global x memo
