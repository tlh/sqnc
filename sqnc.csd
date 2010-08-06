<CsoundSynthesizer>
<CsOptions>

-+rtaudio=pulse
-dWodac:0
</CsOptions>
<CsInstruments>
  sr =  44100
  kr =  441
  ksmps =  100
  nchnls =  2
  0dbfs =  1

instr  1
  idur =  p3
  itrk =  p4
  iamp =  p5
  ipan =  p6
  icps =  p7
  iatt =  p8
  ihol =  p9
  idec =  p10
  isus =  p11
  irel =  p12
   tigoto  skipinit
  a0 pluck  iamp, icps, icps, 0, 1
   skipinit: 
   a0, a1 pan2  a0, ipan
  if (iatt < 0.000001) then
  iatt =  0.000001 
  else
  iatt =  iatt
  endif
  if (ihol < 0.000001) then
  ihol =  0.000001 
  else
  ihol =  ihol
  endif
  if (idec < 0.000001) then
  idec =  0.000001 
  else
  idec =  idec
  endif
  if (irel < 0.000001) then
  irel =  0.000001 
  else
  irel =  irel
  endif
   tigoto  skipinit2
  aenv linseg  0, iatt, 1, ihol, 1, idec, isus, 0.01, isus
   skipinit2: 
  itie tival 
  if ((itie == 0) && (p3 < 0)) then
  itie =  0
  elseif ((p3 < 0) && (itie == 1)) then
  itie =  1
  elseif ((p3 > 0) && (itie == 1)) then
  itie =  2
  elseif ((p3 > 0) && (itie == 0)) then
  itie =  -1
  endif
  if (itie == 2) then
  aenv linsegr  isus, irel, 0
  endif
  a0 =  (a0 * aenv)
  a1 =  (a1 * aenv)
   MixerSend  a0, (1000 + itrk), (2000 + itrk), 0
   MixerSend  a1, (1000 + itrk), (2000 + itrk), 1
endin

instr  master, 1000.1
   chn_k  "chn0", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn0"
  kchn0 chnget  "chn0"
   chn_k  "chn1", 1, 2, 1, 0, 1
   chnset  1, "chn1"
  kchn1 chnget  "chn1"
   chn_k  "chn2", 1, 2, 0, 0, 1
   chnset  0, "chn2"
  kchn2 chnget  "chn2"
   chn_k  "chn3", 1, 2, 0, 0, 1
   chnset  0, "chn3"
  kchn3 chnget  "chn3"
   chn_k  "chn4", 1, 2, 0, 0, 1
   chnset  0, "chn4"
  kchn4 chnget  "chn4"
   chn_k  "chn5", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn5"
  kchn5 chnget  "chn5"
   chn_k  "chn6", 1, 2, 1, 0, 1
   chnset  1, "chn6"
  kchn6 chnget  "chn6"
   chn_k  "chn7", 1, 2, 0, 0, 1
   chnset  0, "chn7"
  kchn7 chnget  "chn7"
   chn_k  "chn8", 1, 2, 0, 0, 1
   chnset  0, "chn8"
  kchn8 chnget  "chn8"
   chn_k  "chn9", 1, 2, 0, 0, 1
   chnset  0, "chn9"
  kchn9 chnget  "chn9"
   chn_k  "chn10", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn10"
  kchn10 chnget  "chn10"
   chn_k  "chn11", 1, 2, 1, 0, 1
   chnset  1, "chn11"
  kchn11 chnget  "chn11"
   chn_k  "chn12", 1, 2, 0, 0, 1
   chnset  0, "chn12"
  kchn12 chnget  "chn12"
   chn_k  "chn13", 1, 2, 0, 0, 1
   chnset  0, "chn13"
  kchn13 chnget  "chn13"
   chn_k  "chn14", 1, 2, 0, 0, 1
   chnset  0, "chn14"
  kchn14 chnget  "chn14"
   chn_k  "chn15", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn15"
  kchn15 chnget  "chn15"
   chn_k  "chn16", 1, 2, 1, 0, 1
   chnset  1, "chn16"
  kchn16 chnget  "chn16"
   chn_k  "chn17", 1, 2, 0, 0, 1
   chnset  0, "chn17"
  kchn17 chnget  "chn17"
   chn_k  "chn18", 1, 2, 0, 0, 1
   chnset  0, "chn18"
  kchn18 chnget  "chn18"
   chn_k  "chn19", 1, 2, 0, 0, 1
   chnset  0, "chn19"
  kchn19 chnget  "chn19"
   chn_k  "chn20", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn20"
  kchn20 chnget  "chn20"
   chn_k  "chn21", 1, 2, 1, 0, 1
   chnset  1, "chn21"
  kchn21 chnget  "chn21"
   chn_k  "chn22", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn22"
  kchn22 chnget  "chn22"
   chn_k  "chn23", 1, 2, 1, 0, 1
   chnset  1, "chn23"
  kchn23 chnget  "chn23"
   chn_k  "chn24", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn24"
  kchn24 chnget  "chn24"
   chn_k  "chn25", 1, 2, 1, 0, 1
   chnset  1, "chn25"
  kchn25 chnget  "chn25"
   MixerSetLevel  (1000 + 0), (2000 + 0), 1
  at00 MixerReceive  (2000 + 0), 0
  at01 MixerReceive  (2000 + 0), 1
   MixerSetLevel  (1000 + 1), (2000 + 1), 1
  at10 MixerReceive  (2000 + 1), 0
  at11 MixerReceive  (2000 + 1), 1
   MixerSetLevel  (1000 + 2), (2000 + 2), 1
  at20 MixerReceive  (2000 + 2), 0
  at21 MixerReceive  (2000 + 2), 1
   MixerSetLevel  (1000 + 3), (2000 + 3), 1
  at30 MixerReceive  (2000 + 3), 0
  at31 MixerReceive  (2000 + 3), 1
  at40 =  ((at00 * kchn2) + (at10 * kchn7) + (at20 * kchn12) + (at30 * kchn17))
  at41 =  ((at01 * kchn2) + (at11 * kchn7) + (at21 * kchn12) + (at31 * kchn17))
  at50 =  ((at00 * kchn3) + (at10 * kchn8) + (at20 * kchn13) + (at30 * kchn18))
  at51 =  ((at01 * kchn3) + (at11 * kchn8) + (at21 * kchn13) + (at31 * kchn18))
  at60 =  ((at00 * kchn4) + (at10 * kchn9) + (at20 * kchn14) + (at30 * kchn19))
  at61 =  ((at01 * kchn4) + (at11 * kchn9) + (at21 * kchn14) + (at31 * kchn19))
  at00 =  (at00 * kchn0 * kchn1)
  at01 =  (at01 * kchn0 * kchn1)
  at10 =  (at10 * kchn5 * kchn6)
  at11 =  (at11 * kchn5 * kchn6)
  at20 =  (at20 * kchn10 * kchn11)
  at21 =  (at21 * kchn10 * kchn11)
  at30 =  (at30 * kchn15 * kchn16)
  at31 =  (at31 * kchn15 * kchn16)
  at40 =  (at40 * kchn20 * kchn21)
  at41 =  (at41 * kchn20 * kchn21)
  at50 =  (at50 * kchn22 * kchn23)
  at51 =  (at51 * kchn22 * kchn23)
  at60 =  (at60 * kchn24 * kchn25)
  at61 =  (at61 * kchn24 * kchn25)
  am0 =  (at00 + at10 + at20 + at30 + at40 + at50 + at60)
  am1 =  (at01 + at11 + at21 + at31 + at41 + at51 + at61)
   chn_k  "chn26", 1, 2, 0.5, 0, 1
   chnset  0.5, "chn26"
  kchn26 chnget  "chn26"
   chn_k  "chn27", 1, 2, 0.7, 0, 1
   chnset  0.7, "chn27"
  kchn27 chnget  "chn27"
  if (kchn26 > 0.5) then
  klpan =  (2 * (kchn26 - 0.5)) 
  else
  klpan =  0
  endif
  if (kchn26 < 0.5) then
  krpan =  (2 * kchn26) 
  else
  krpan =  1
  endif
   anl0, anr0 pan2  am0, klpan
   anl1, anr1 pan2  am1, krpan
  am0 =  (anl0 + anl1)
  am1 =  (anr0 + anr1)
  am0 =  (am0 * kchn27)
  am1 =  (am1 * kchn27)
   outs  am0, am1
   MixerClear 
endin

instr  1001
  Sname strget  p5
   chnset  p4, Sname
endin
</CsInstruments>
<CsScore>
f 0 1000000
</CsScore>
</CsoundSynthesizer>
