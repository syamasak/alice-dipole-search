#include "AliMagFast.h"
#include "AliMagF.h"
#include "AliMagWrapCheb.h"
#include "AliCheb3D.h"
#include "TTree.h"
#include "TFile.h"
#include "TTreeStream.h"
#include "TRandom.h"

int benchF(int ntst=1000) {
  AliMagF::SetFastFieldDefault(kFALSE);
  AliMagFast *m = new AliMagFast();
  AliMagF   *m0 = new AliMagF("m", "m", 1, 1, AliMagF::k5kG, AliMagF::kBeamTypepp, -1, 1, 2, 15, "~/alice/mfchebKGI_sym.root");//AliMagF("m0","m0");
  AliMagWrapCheb *mm = m0->GetMeasuredMap();

  TTreeSRedirector fDebugStreamer("bbench.root","recreate");

  int ndip = mm->GetNParamsDip();
  float *boundsMin,*boundsMax;
  float  xyzF[3],bxyzF[3];
  double xyzD[3],bxyzD[3];
  for (int ipar=0;ipar<ndip;ipar++) {
    AliCheb3D* dpar =  mm->GetParamDip(ipar);
    boundsMin = dpar->GetBoundMin();
    boundsMax = dpar->GetBoundMax();
    printf("Doing %d \n",ipar);
    //dpar->Print();

    for (int ip=ntst;ip--;) {
      for (int j=3;j--;) {
	xyzF[j] = xyzD[j] = boundsMin[j]+gRandom->Rndm()*(boundsMax[j]-boundsMin[j]);
      }
      m0->Field(xyzD, bxyzD);
      Bool_t fb = m->Field(xyzF, bxyzF);
      int ipar1 = mm->FindDipSegment(xyzD);
      UShort_t ipar2;
      m->GetSegmentDip(xyzF, ipar2);
      fDebugStreamer<<"bbench"<<"pid="<<ipar<<"pid1="<<ipar1<<"fpid="<<ipar2
        <<"x="<<xyzF[0]<<"y="<<xyzF[1]<<"z="<<xyzF[2]
		    <<"bx="<<bxyzD[0]<<"by="<<bxyzD[1]<<"bz="<<bxyzD[2]
		    <<"fbx="<<bxyzF[0]<<"fby="<<bxyzF[1]<<"fbz="<<bxyzF[2]
        <<"fb="<<fb<<"\n";
    }

  }
}
