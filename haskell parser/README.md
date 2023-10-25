# 2022Assignment2

## Using the code bundle

`stack build`

Builds the packages.

`stack run`

Builds an executable that runs the main function in app/Main.hs.

`stack test`

Builds the packages and executes doctests on all hs files in the submission folder.

`stack clean --full`

Removes unnecessary build files to reduce bundle size.

## Troubleshooting

`/usr/bin/ld.gold: error: cannot find -lgmp`

Run `sudo apt-get install libgmp3-dev`
