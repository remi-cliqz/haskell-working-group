# haskell-working-group
Resources for the Haskell working group

## building
```bash
# install pandoc and shake
stack install --flag pandoc:highlighting-kate pandoc
stack install pandoc

# build files
stack runhaskell shakefile.hs

# clean
stack runhaskell shakefile.hs clean
```
