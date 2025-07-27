      // VERSION: 2025-07-27
     H option(*nodebugio) nomain
      // -----------------------------------------------------------------------
      // Replace &n by value n
      // -----------------------------------------------------------------------
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
      // -----------------------------------------------------------------------
      // scan and replace
      // -----------------------------------------------------------------------
     pscanRpl          b
     d scanRpl         pi           228    varying
     d wScan                        228    varying const
     d wReplace                     228    varying const
     d wSource                      228    varying const
      *
     d spaces          s            132a   inz(*all' ')
     d wTarget         s            228    varying
     d p1              s              3u 0
     d p3              s              3u 0
     d l               s              3u 0
     d wReplace$       s                   like(wReplace)
      *
       wTarget=wSource;
       p1=%scan(wScan:wTarget);
       dow p1>0;
         p3=%check(' '
                  :wTarget
                  :p1+%len(wScan));
         if p3>0 and %subst(wTarget:p3:1)=' ';
           l=p3-p1+1;
           wReplace$=wReplace;
           if %len(wReplace)<l;
             wReplace$+=%subst(spaces:1:l-%len(wReplace));
           endIf;
           wTarget=%replace(wReplace$
                           :wTarget
                           :p1
                           :l);
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
      // -----------------------------------------------------------------------
      // script text formater
      // -----------------------------------------------------------------------
     pscrTxtFmt        b                   export
     d scrTxtFmt       pi            50
     d  in                           50    const
     d  status                        2    const
      *
     d p               s              3u 0
     d atr             s             21    varying inz('')
     d out$            s             50
       // Retrieve the position of the last significant character
       // 1234546789012345467890123454678901234546789012345467890
       // Bla bla   . . . . . . . . . . . .  24/10/24 18:23:00 ok
       // . . . . . . *12   *. . . . . . . . 24/10/24 18:23:00 ok
       out$=in;
       p=%checkr('. ':out$:29);

       if p=0;
         out$=*all'. ';
       elseIf %rem(p:2)=0;
         %subst(out$:p+2)=*all' .';
       else;
         %subst(out$:p+1)=*all' .';
       endIf;
       // format the timestamp
       atr=%char(%date():*dmy/)+' '+%char(%time():*hms);
       // define color and status
       if     status='OK';
         atr = x'3b'+atr+' Ok';                  // 
       elseIf status='KO';
         atr = x'2b'+atr+' Ko';                  // 
       elseIf status='SB';
         atr = x'33'+atr+' Sb';                  // 
       elseIf status='GO';
         atr = x'33'+atr+' go';                  // 
       elseIf status='XI';
         atr = '';                               // hide
       else;
         atr = x'3b'+atr+' ??';                  // 
       endIf;

       %subst(out$:30)=atr;

       return out$;
     p                 e
      // -----------------------------------------------------------------------
      // get now
      // -----------------------------------------------------------------------
     pgetNow           b                   export
     d getNow          pi            26a
       return %char(%timestamp());
     p                 e
      // -----------------------------------------------------------------------
      // get duration
      // -----------------------------------------------------------------------
     pgetDuration      b                   export
     d getDuration     pi            30a   varying
     d  z2                             z
     d  z1                             z
      *
     d w20n6           s             20  6
     d w30a            s             30a
     d duration        s             30a   varying
       // values now()+5 minutes-now() --> 500,00000
       exec sql values :z2 - :z1 into :w20n6;
       // yyyymmddhhmmssnnnnnn
       // 12345678901234567890
       // 0000000000
       w30a=%editc(w20n6:'X');

       duration=%subst(w30a:11:2)+':'+%subst(w30a:13:2);

       if %subst(w30a:1:10)<>*all'0';
         duration=%subst(w30a:9:2)+':'+duration;
       endIf;

       if %subst(w30a:1:8)<>*all'0';
         duration=%subst(w30a:7:2)+' days '+duration;
       endIf;

       return duration;

     p                 e
