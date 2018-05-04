# DipoleParam
ALICE dipole magnet field parameterization search part: Prepopulated Quick Segment Search algorithm.

## Build
Please install haskell-stack and cd to the repo directory. Then,

    $ stack build

## Run
To run this program, you need AliMagWrapCheb::DumpSave() file. Then, please run the following commands:

    $ stack exec genparam dip2k AliMagFastDip2k path/to/Sol12_Dip6_Hole.txt
    $ stack exec genparam dip5k AliMagFastDip5k path/to/Sol30_Dip6_Hole.txt

If succeeded, Sol12_Dip6_Hole.json, Sol30_Dip6_Hole.json, AliMagFastDip2k.cxx, AliMagFastDip5k.cxx are generated in current directory. *.json files are necessary to generate *.h files later.
