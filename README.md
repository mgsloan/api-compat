To use, do something like this after loading ApiCompat.GetApi:

```haskell
:m + ApiCompat.GetApi

writeFile "diagrams-core-0.5.1.api" =<< getApi ["diagrams-core-0.5.1"] ["Graphics.Rendering.Diagrams"]
writeFile "diagrams-core-0.4.api" =<< getApi ["diagrams-core-0.4"] ["Graphics.Rendering.Diagrams"]
```

```bash
diff diagrams-core-0.4.api diagrams-core-0.5.1.api -U 6 > diagrams-core.api.diff2
```
