{
  echo Started at `date`
  time stack build

  time stack exec genparam dip5k AliMagFastDip5k Sol30_Dip6_Hole.txt
  mkdir -p AliMagFastDip5k
  mv AliMagFastDip5k.cxx AliMagFastDip5k

  time stack exec genparam dip2k AliMagFastDip2k Sol12_Dip6_Hole.txt
  mkdir -p AliMagFastDip2k
  mv AliMagFastDip2k.cxx AliMagFastDip2k

  echo Finished at `date`
} | tee -a genparam.log
