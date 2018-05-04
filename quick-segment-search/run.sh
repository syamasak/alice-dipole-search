stack build
stack exec genparam dip2k AliMagFastDip2k Sol12_Dip6_Hole.txt
cp AliMagFastDip2k.cxx ~/alice/AliRoot/STEER/STEERBase/AliMagFastDip2k
stack exec genparam dip5k AliMagFastDip5k Sol30_Dip6_Hole.txt
cp AliMagFastDip5k.cxx ~/alice/AliRoot/STEER/STEERBase/AliMagFastDip5k
