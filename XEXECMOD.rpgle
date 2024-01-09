      // VERSION: 2023-10-20
     H option(*nodebugio) nomain
      //‚-----------------------------------------------------------------------
      //‚Replace %n by value n
      //‚-----------------------------------------------------------------------
     P REPLACEALL      B                   export
     D REPLACEALL      PI
     D   strIN                      228    varying  const
      *
     D   v1                          80    varying  const
     D   v2                          80    varying  const
     D   v3                          80    varying  const
     D   v4                          80    varying  const
     D   v5                          80    varying  const
     D   v6                          80    varying  const
     D   v7                          80    varying  const
     D   v8                          80    varying  const
     D   v9                          80    varying  const
      *
     D   strOut                     228    varying
      *
     D   nStrIn                       5i 0 const
      *
     D   n1                           5i 0 const
     D   n2                           5i 0 const
     D   n3                           5i 0 const
     D   n4                           5i 0 const
     D   n5                           5i 0 const
     D   n6                           5i 0 const
     D   n7                           5i 0 const
     D   n8                           5i 0 const
     D   n9                           5i 0 const
      *
     D  nStrOut                       5i 0
      *
     d   sqlStt                       5a
     d   function                   517a   varying
     d   specific                   128a   varying
     d   errorMsg                    70a   varying
      *
     d n               s              5i 0 dim(9)
     d v               s             80    varying dim(9)
     d i               s              3u 0
      *
       n(1)=n1;
       n(2)=n2;
       n(3)=n3;
       n(4)=n4;
       n(5)=n5;
       n(6)=n6;
       n(7)=n7;
       n(8)=n8;
       n(9)=n9;
       //
       v(1)=v1;
       v(2)=v2;
       v(3)=v3;
       v(4)=v4;
       v(5)=v5;
       v(6)=v6;
       v(7)=v7;
       v(8)=v8;
       v(9)=v9;
       //
       strOut=%trimR(strIn);
       for i=1 to %elem(v);
         if n(i)=-1;
           return;
         endIf;
       //strOut=%scanRpl('&'+%trim(%char(i)):v(i):strOut);
         strOut=scanRpl('&'+%trim(%char(i)):v(i):strOut);
       endFor;
       return;
     P                 e
      //‚-----------------------------------------------------------------------
      //‚scan and replace
      //‚-----------------------------------------------------------------------
     pscanRpl          b
     d scanRpl         pi           228    varying
     d wScan                        228    varying const
     d wReplace                     228    varying const
     d wSource                      228    varying const
      *
     d wTarget         s            228    varying
     d p1              s              3u 0
     d p3              s              3u 0
      *
       wTarget=wSource;
       p1=%scan(wScan:wTarget);
       dow p1>0;
         p3=%check(' '
                  :wTarget
                  :%min(%len(wTarget):p1+%len(wScann)));
         if p3>0 and%subst(wTarget:p3:p1)=']';
           %subst(wTarget:p1:p3-p1+1)='';
           wTarget=%replace(wReplace
                           :wTarget
                           :p1
                           :%min(%len(wReplace):p3-p1+1));
         else;
           wTarget=%replace(wReplace
                           :wTarget
                           :p1
                           :%len(wScan));
         endIf;
         p1=%scan(wScan:wTarget);
       enddo;
       return wTarget;
     p                 e
