To use, do something this after loading ApiCompat.GhcApi:

```haskell
writeFile "diagrams-core-0.5.1.api" =<< getApis ["diagrams-core-0.5.1"] ["Graphics.Rendering.Diagrams"]
writeFile "diagrams-core-0.4.api" =<< getApis ["diagrams-core-0.4"] ["Graphics.Rendering.Diagrams"]
```
