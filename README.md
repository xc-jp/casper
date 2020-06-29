# casper

Extremely simple content-addressable storage.

```haskell
main = do
  store <- initStore "./store"
  runCasperT $ do
    addr :: Address [Int] <- write [1,2,3]
    ...
    read addr >>= liftIO . print

```
