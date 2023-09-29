# TODO

- [x] Prioritize functions whose src/dst type is defined in the same package
   - To prioritize e.g. `text-2.0.2:Data.Text.Lazy.unpack` over `incipit-base-0.5.1.0:Data.Text.Lazy.unpack` (the latter being a re-export of the former)
   - Related: is it possible to use GHC to query whether one function is a re-export of another (e.g. by looking at the definition site of the function in question)?
- [x] Filter per shortest path tree (`[NonEmpty Function]`): if exists a path (`[Function]`) using only functions from `src` or `dst` package then ignore the other results in the shortest path tree
   - Note: the other results are not ignored; they just appear later in the result list.
- [x] Prioritize functions from the same package as the from/to type
- [ ] Prioritize functions from 'existing' dependencies (ie. take a list of dependencies for which functions are prioritized higher)
