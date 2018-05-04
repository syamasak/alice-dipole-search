#include "AliMagFast.h"
#include "AliMagF.h"
#include "AliMagWrapCheb.h"
#include "TTree.h"
#include "TFile.h"
#include <random>
#include <ctime>

int benchmarkPrec() {
  AliMagFast *m = new AliMagFast();
  AliMagF   *m0 = new AliMagF("m0","m0");
  AliMagWrapCheb *mm = m0->GetMeasuredMap();

  std::default_random_engine g;
  std::uniform_real_distribution<float>
    dipxy(-330, 330), dipz(-1760, -535.3), solxy(-500,500), solr(0, 500), solp(-3.15,3.15), solz(-550, 550);
  std::vector<float> px, py, pz;
  std::vector<double> dx, dy, dz;
  std::vector<ushort> pi;
  std::vector<int> di;
  int N = 1000000;
  // dipole
  for (int i = 0; i < N;) {
    Double_t dxyz[] = {dipxy(g), dipxy(g), dipz(g)};
    int segr = mm->FindDipSegment(dxyz);
    if (mm->GetParamDip(segr)->IsInside(dxyz)) {
      px.push_back((float)dxyz[0]);
      py.push_back((float)dxyz[1]);
      pz.push_back((float)dxyz[2]);
      float xyz[] = {(float)dxyz[0], (float)dxyz[1], (float)dxyz[2]};
      ushort seg = -1;
      m->GetSegmentDip(xyz, seg);
      pi.push_back(seg);
      dx.push_back(dxyz[0]);
      dy.push_back(dxyz[1]);
      dz.push_back(dxyz[2]);
      di.push_back(segr);
      i++;
    }
  }
  TFile *root = new TFile("prec.root", "recreate");

{
  printf("START AliMagFast Dipole\n");
  double ms;
  float fxyz[] = {0, 0, 0};
  float fbxyz[] = {0, 0, 0};
  double dxyz[] = {0, 0, 0};
  double dbxyz[] = {0, 0, 0};
  int seg[2] = {-1, -1};
  TTree *t = new TTree("dip","dip");
  t->Branch("x",&fxyz[0],"x/f");
  t->Branch("y",&fxyz[1],"y/f");
  t->Branch("z",&fxyz[2],"z/f");
  t->Branch("bx",&fbxyz[0],"bx/f");
  t->Branch("by",&fbxyz[1],"by/f");
  t->Branch("bz",&fbxyz[2],"bz/f");
  t->Branch("seg",&seg[0],"seg/i"); // segment id
  t->Branch("bxr",&dbxyz[0],"bxr/d"); // r for reference (AliMagF)
  t->Branch("byr",&dbxyz[1],"byr/d");
  t->Branch("bzr",&dbxyz[2],"bzr/d");
  t->Branch("segr",&seg[1],"segr/i");
  t->Branch("t",&ms,"t/d"); // time consumed
  for (size_t i = 0; i < N; i++) {
    fxyz[0] = px[i];
    fxyz[1] = py[i];
    fxyz[2] = pz[i];
    std::clock_t start = std::clock();
    m->Field(fxyz, fbxyz);
    ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
    dxyz[0] = dx[i];
    dxyz[1] = dy[i];
    dxyz[2] = dz[i];
    m0->Field(dxyz, dbxyz);
    seg[0] = pi[i];
    seg[1] = di[i];
    t->Fill();
  }
t->Draw("bz-bzr");
}

// solenoid
/*
std::vector<float> fx, fy, fz;
std::vector<double> sx, sy, sz;
for (int i = 0; i < N;) {
  Double_t dxyz[] = {solxy(g), solxy(g), solz(g)};
  // mm->GetParamSol(mm->FindSolSegment(dxyz))->IsInside(dxyz)
  if (dxyz[0]*dxyz[0]+dxyz[1]*dxyz[1]<=500*500) {
    fx.push_back((float)dxyz[0]);
    fy.push_back((float)dxyz[1]);
    fz.push_back((float)dxyz[2]);
    sx.push_back(dxyz[0]);
    sy.push_back(dxyz[1]);
    sz.push_back(dxyz[2]);
    i++;
  }
}

{
  printf("START AliMagFast Solenoid\n");
  double ms;
  float fxyz[] = {0, 0, 0};
  float fbxyz[] = {0, 0, 0};
  double dxyz[] = {0, 0, 0};
  double dbxyz[] = {0, 0, 0};
  TTree *t = new TTree("sol","sol");
  t->Branch("x",&fxyz[0],"x/f");
  t->Branch("y",&fxyz[1],"y/f");
  t->Branch("z",&fxyz[2],"z/f");
  t->Branch("bx",&fbxyz[0],"bx/f");
  t->Branch("by",&fbxyz[1],"by/f");
  t->Branch("bz",&fbxyz[2],"bz/f");
  t->Branch("bxr",&dbxyz[0],"bxr/d");
  t->Branch("byr",&dbxyz[1],"byr/d");
  t->Branch("bzr",&dbxyz[2],"bzr/d");
  t->Branch("t",&ms,"t/f");
  for (size_t i = 0; i < N; i++) {
    fxyz[0] = fx[i];
    fxyz[1] = fy[i];
    fxyz[2] = fz[i];
    std::clock_t start = std::clock();
    m->Field(fxyz, fbxyz);
    ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
    dxyz[0] = sx[i];
    dxyz[1] = sy[i];
    dxyz[2] = sz[i];
    m0->Field(dxyz, dbxyz);
    t->Fill();
  }
}
*/
  return 0;
}
