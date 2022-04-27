# weegeeprepro

A Wee little Graphics shader language PreProcessor

---

This is a very limited, badly tested, immature, non-compliant (non-compliant
with what? *everything*) preprocessor primarily for shading languages that
*lack* built-in preprocessors like WGSL. It might work for any C-ish language
with no strings that can't syntactically have `#` at the beginning of lines, but
it's primarily meant for --and only tested with-- WGSL.

This is not on crates.io because you should probably not use this until it grows
up. The very silly name is also meant as a slight disincentive to using it for
anything important.

## License ##
 
`weegeeprepro` is licensed under the [MIT-0](LICENSE) license
