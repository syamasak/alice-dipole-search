(cd `dirname $0`; time stack install)  # install genparam binary to ~/.local/bin
{
  echo Started at `date`

  echo genparam dip5k AliMagFastDip5k Sol30_Dip6_Hole.txt
  time genparam dip5k AliMagFastDip5k Sol30_Dip6_Hole.txt
  mkdir -p AliMagFastDip5k
  mv AliMagFastDip5k.cxx AliMagFastDip5k

  echo genparam dip2k AliMagFastDip2k Sol12_Dip6_Hole.txt
  time genparam dip2k AliMagFastDip2k Sol12_Dip6_Hole.txt
  mkdir -p AliMagFastDip2k
  mv AliMagFastDip2k.cxx AliMagFastDip2k

  echo Finished at `date`
} 2>&1 | tee -a genparam.log
