      double precision cf(7)
      double precision rqr(9), rqi(9), lqr(9), lqi(9),	
     &                 llr(9), lli(9)
      logical sets(6)
      common /checkmodel/ cf, rqr, rqi, lqr, lqi, llr, lli, sets

      double precision rqwidth(9), lqwidth(9), llwidth(9),
     &                 qwidth(9),
     &                 rqbr(9), lqbr(9), llbr(9), qbr(9)
      common /widthbr/ rqwidth, lqwidth, llwidth, qwidth,
     &                 rqbr, lqbr, llbr, qbr

      double precision qqrt(9,9),qqru(9,9),qlrt(9,9),qlru(9,9),
     &                 gqqscale(9,9), gqlscale(9,9)
      double complex smqckm(9), smlckm(9), gsmsu2l
      logical qqsm, qlsm
      common /rtru/ qqrt,qqru,qlrt,qlru,gqqscale,gqlscale,
     &              smqckm,smlckm,gsmsu2l,qqsm,qlsm
