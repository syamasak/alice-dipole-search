#if !defined(__CINT__) || defined(__MAKECINT__)
#include <TMath.h>
#include <TSystem.h>
#include <TStopwatch.h>
#include <TProfile.h>
#endif

void PrintMem()
{
  static float mres=0,mvir=0, mres0=0,mvir0=0;
  static ProcInfo_t procInfo;
  static TStopwatch sw;
  const Long_t kMB = 1024;
  gSystem->GetProcInfo(&procInfo);
  mres = float(procInfo.fMemResident)/kMB;
  mvir = float(procInfo.fMemVirtual)/kMB;
  sw.Stop();
  printf("RSS: %.2f(%.2f) VMEM: %.2f(%.2f) MB, CpuTime:%.2f RealTime:%.2f\n",
	 mres,mres-mres0,mvir,mvir-mvir0,sw.CpuTime(),sw.RealTime());
  mres0 = mres;
  mvir0 = mvir;
  sw.Start();
}
