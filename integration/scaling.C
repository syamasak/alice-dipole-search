#include "AliMagFast.h"
#include "AliMagF.h"
#include "AliMagWrapCheb.h"
#include "AliCheb3D.h"
#include "TTree.h"
#include "TFile.h"
#include "TTreeStream.h"
#include "TRandom.h"

int scaling() {
  AliMagF::SetFastFieldDefault(kFALSE);
  AliMagF   *m0 = new AliMagF("m", "m", 1, 1, AliMagF::k5kG, AliMagF::kBeamTypepp, -1, 1, 2, 15, "~/alice/mfchebKGI_sym.root");
  AliMagWrapCheb *mm = m0->GetMeasuredMap();

  TTreeSRedirector fDebugStreamer("scaling.root", "recreate");

  int ndip = mm->GetNParamsDip();
  float *boundsMin,*boundsMax,*boundsScale,*boundsOffset;
  float  xyzF[3],bxyzF[3];
  double xyzD[3],bxyzD[3];
  for (int ipar=0;ipar<ndip;ipar++) {
    AliCheb3D* dpar = mm->GetParamDip(ipar);
    boundsMin = dpar->GetBoundMin();
    boundsMax = dpar->GetBoundMax();
    boundsScale = dpar->GetBoundScale(); /* added to AliCheb3D */
    boundsOffset = dpar->GetBoundOffset(); /* added to AliCheb3D */
    printf("Doing %d \n",ipar);
    dpar->Print();

    fDebugStreamer<<"scaling"<<"pid="<<ipar
      <<"xmin="<<boundsMin[0]<<"ymin="<<boundsMin[1]<<"zmin="<<boundsMin[2]
      <<"xmax="<<boundsMax[0]<<"ymax="<<boundsMax[1]<<"zmax="<<boundsMax[2]
      <<"xscale="<<boundsScale[0]<<"yscale="<<boundsScale[1]<<"zscale="<<boundsScale[2]
      <<"xoff="<<boundsOffset[0]<<"yoff="<<boundsOffset[1]<<"zoff="<<boundsOffset[2]
      <<"\n";

  }
}
