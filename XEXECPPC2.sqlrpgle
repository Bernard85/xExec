      // Version: 2025-07-07
     H option(*nodebugio) DFTACTGRP(*NO) actgrp(*caller) bndDir('XEXEC')
      // -----------------------------------------------------------------------
      // CRTSQLRPGI OBJ(qtemp/XEXECPPC2)
      //            SRCFILE(bernard85/src)
      //            OBJTYPE(*module) DBGVIEW(*SOURCE)
      // -----------------------------------------------------------------------
      // IBM functions
      // -----------------------------------------------------------------------
      // command execute
     D QCmdExc         PR                  ExtPgm('QCMDEXC')
     D  CmdTxt                     3000A   Const
     D  CmdLen                       15P 5 Const
      // -----------------------------------------------------------------------
      // structures
      // -----------------------------------------------------------------------
      // Program Status Data Structure
     D  PGMSDS        SDS                  Qualified
     D    MsgId               40     46
     D    MsgTxt              91    170
     D    JOB                244    253
     D    user               254    263
     d    number             264    269
     D    user$              358    367
      //
     D tDta            ds
     D  dta                         228
     D oDta            ds                  likeDS(tDta) dim(20000)
     d iDta            s              5u 0
     D maxDta          S              5u 0 inz(20000)
      // Buffer for execution (command or sql)
     D buffer          s          30000    varying inz('')
      // flags
     D fError          s               n
     D fMonitor        s               n
     D fStop           s               n
     d message         S            100a   varying
     D order           s             50A   varying
     d margin          s              3u 0
     d logSeq          S              5u 0
     d job10           s             10a   varying
     d job28           s             28a   varying
     d root            s             10a   varying
      // variable to identify the script
     d xScrMbr$        s             10a   varying
     d xScrLib$        s             10a   varying
     d xScrFil$        s             10a   varying
     d t0              s               z
     d t1              s               z
      // array of parameters
     d maxParm$        c                   const(9)
     d tParm           s             80a   template varying
     d maxParm         s              5u 0 based(pMaxParm)
     d oParm           s                   dim(maxParm$) like(tParm)
      // constant for colors
     d RB              c                   const(x'2b')
     d R               c                   const(x'28')
     d GR              c                   const(x'21')
     d G               c                   const(x'20')
     d B               c                   const(x'3a')
      // get duration
     d getDuration     pr            30    varying
     d  z2                             z   const
     d  z1                             z   const
      // -----------------------------------------------------------------------
      // main routine
      // -----------------------------------------------------------------------
     d xExecPPC2       Pi
     d   retcod                       1a
     d   xscrMbr                     10    const
     d   xScrLib                     10    const
     d   xScrFil                     10    const
     d   Parms                      740a
     d   callLvl                      2p 0 const
     d   jobtype                      1    const

       init();
       main();
       End();

      // -----------------------------------------------------------------------
      // Initialisation
      // -----------------------------------------------------------------------
     pInit             b
     d init            pi
     d p               s              5u 0
     d n               s              5u 0
     D Parmn           S                   based(pParmn) like(tParm)
       // Return code
       retcod='0';
       // fit variables
       xScrMbr$= trim(xScrMbr);
       xScrLib$= trim(xScrLib);
       xScrFil$= trim(xScrFil);
       // current job/library/file
       job10= trim(pgmsds.job);
       job28=pgmsds.number+'/'+ trim(pgmsds.user)+'/'+ trim(pgmsds.job);
       // root
       root=xScrMbr$;
       p= scan('_':xScrMbr);
       if p>1;
         root= subst(xScrMbr:1:p-1);
       endIf;
       // Allow write
       exec sql SET OPTION commit=*none, DatFmt=*ISO, decmpt=*period;
       // Message for script begin (first call only)
       if callLvl=0;
         t0= timeStamp();
         addLog(GR:'Script '+xScrLib$+'/'+xScrFil$+'.'+xScrMbr$+
                ' started on ' + char( date())+ ' at ' + char( time())+G);
         addLog(GR
               :'the processing job is '+job28+'-'+ trim(pgmSds.user$)+G);
       endIf;
       // Loading script --> oDta() + maxDta
       exec sql declare Input cursor for
       select srcDta from xScript a where srcdta<>'' order by rrn(a);
       //
       exec sql open Input;
       exec sql fetch next from Input for :maxDta rows into :oDta;
       exec sql get diagnostics :maxDta=ROW_COUNT;
       exec sql close Input;
       if maxDta=0;
         addLog(RB:'Error during script loading'+G);
         retcod='2';
         return;
       endIf;
       // Loading Parmarameter -> oParm() + maxParm
       pMaxParm= addr(Parms);
       clear oParm;
       for n=1 to maxParm;
         pParmn= addr(Parms)+ size(maxParm)+( size(tParm))*(n-1);
         if maxParm=1 and parmn='*NONE';
           return;
         endif;
         oParm(n)=Parmn;
       endFor;
     pInit             e
      // -----------------------------------------------------------------------
      // main process
      // -----------------------------------------------------------------------
     pmain             b
     d main            pi
     D xDta            s            228
     D xDta_           s            228
       // Loop on all rows loaded
       for iDta=1 to maxDta;
         // Replace variable Parmaters
         xDta=replaceVal(oDta(iDta));
         xDta_= trim(xDta);
         // copy line to log
         addLog(' ':xDta);

         // to load copy-book
          if  len(xDta_)>=5
         and  subst(xDta_:1:5)='/COPY';
           copyBook(xDta);
         else;
           // Buffer loading
           LoadBuffer(xDta);
           // Try to execute the buffer
           exec();
         endif;
         // Message
         checkMessage();
         // Stop
         if fStop;
           leave;
         endIf;
       endFor;
     p                 e
      // -----------------------------------------------------------------------
      // Loading Variable values
      // -----------------------------------------------------------------------
     preplaceVal       b
     dreplaceVal       pi           228
     d inDta                        228    const
      *
     d outDta          s            228    varying
     d v               s              3u 0
       // Current script
       outDta= scanRpl('#0':xScrMbr$:inDta);
       // Values for parameters
       for v=1 to maxParm$;
         outDta= scanRpl('#'+ char(v):oParm(v):outDta);
       endFor;
       // Values for other keyWords
       replaceKW(outDta:'#R'     :root                                 );
       replaceKW(outDta:'#JOB'   :job28                                );
       replaceKW(outDta:'#J'     :job10                                );
       replaceKW(outDta:'#L'     :xScrLib$                             );
       replaceKW(outDta:'#F'     :xScrFil$                             );
       replaceKW(outDta:'#NOW(0)': subst( char( timeStamp():*iso):1:19));
       replaceKW(outDta:'#DATE(6)': subst( char( date():*iso0):3:6)    );
       replaceKW(outDta:'#DATE(8)': subst( char( date():*iso0):1:8)    );
       return outDta;
     p                 e
      // -----------------------------------------------------------------------
      // Replace specific keyword
      // -----------------------------------------------------------------------
     preplacekw        b
     d ReplaceKW       pi
     d  outDta                      228    varying
     d  KW                           80    const varying
     d  KWVal                        80    const varying
      *
     d outDta_         s            228
     d p               s              3u 0

       // upper case
       outDta_=$upper(outDta);
       // loop on each occurences
       p= scan(KW:outDta_);
       dow p>0;

         outDta =  replace(KWVal:outDta :p: len(KW));
         outDta_=  replace(KWVal:outDta :p: len(KW));

         p= scan(KW:outDta_:p);
        endDo;
     p                 e
      // -----------------------------------------------------------------------
      // Load buffer
      // -----------------------------------------------------------------------
     pLoadBuffer       b
     dLoadBuffer       pi
     D xDta                         228    const
      *
     D trimDta         s            228    varying inz('')
     D p               s              3u 0
       trimDta= trim(xDta);
       // Comment line
       if   len(trimDta)>=2
       and ( subst(trimDta:1:2)='//'or  subst(trimDta:1:2)='--');
         return;
       endIf;
       // Remove right comment
       p=$scanr('//':trimDta);
       if p>1;
         trimDta= trim( subst(trimDta:1:p-1));
       endif;
       // Margin for indentation (log file)
       if buffer='';
         margin= check(' ':xDta);
         Buffer=trimDta;
       else;
         Buffer+=' '+trimDta;
       endIf;
     p                 e
      // -----------------------------------------------------------------------
      // Execute buffer
      // -----------------------------------------------------------------------
     pExec             b
       fError=*off;
       message='';
       // If the last character is not semicolon it is not a command
       if buffer='' or  subst(buffer: len(buffer):1)<>';';
         return;
       endIf;
       // Clear final semicolon
        subst(buffer: len(buffer):1)=' ';
       // Retreive the order (1st word)
       getOrder();
       // test/launch the buffer
       t1= timeStamp();
       if isSql();
         goSql();
       elseif isSpecif();
         goSpecif();
       else;
         goAS400cmd();
       endIf;

       buffer='';
     p                 e
      // -----------------------------------------------------------------------
      // get order (first word of the buffer)
      // -----------------------------------------------------------------------
     P getOrder        B
     D getOrder        PI
     D  p              s              3u 0
       order='';
       p= scan(' ':buffer);
       if p=0;
         p= len(buffer);
       endIf;
       if p> size(order)-2 or p=0;
         return;
       endIf;
       order= subst(buffer:1:p-1);
       ORDER=$upper(order);

     p                 e
      // -----------------------------------------------------------------------
      // is it sql order ?
      // -----------------------------------------------------------------------
     P isSQL           B
     D isSQL           PI              n

       // call$ -> call sql
       if order='CALL$';
          subst(buffer:1:5)='call ';
       endif;

       return order = 'CALL$'
           or order = 'CREATE'
           or order = 'ALTER'
           or order = 'DROP'
           or order = 'INSERT'
           or order = 'UPDATE'
           or order = 'DELETE'
           or order = 'MERGE'
           or order = 'TRUNCATE'
           or order = 'LABEL'
           or order = 'RENAME'
           or order = 'SET';

     P                 E
      // -----------------------------------------------------------------------
      // is it a specific order ?
      // -----------------------------------------------------------------------
     P isSpecif        B
     D isSpecif        PI              n

       return order = 'MONITOR'
           or order = 'END-MONITOR'
           or order = 'STOP'
           or order = 'SELF-SUBMIT';

     P                 E
      // -----------------------------------------------------------------------
      // Execute sql order
      // -----------------------------------------------------------------------
     P goSQL           B
     D goSQL           PI
     D  sqlstate       s              5A
       exec sql execute immediate :buffer;
       exec sql get diagnostics condition 1
              :message=MESSAGE_TEXT,
              :sqlstate = RETURNED_sqlstate;
       if  subst(sqlstate:1:1)<>'0';
         fError=*on;
       endIf;
     P                 e
      // -----------------------------------------------------------------------
      // Execute specific command
      // -----------------------------------------------------------------------
     P goSpecif        B
       if order = 'MONITOR';
         fMonitor   = *on;
         message    = 'The executor enters monitor mode';
       elseif order = 'END-MONITOR';
         fMonitor   = *off;
         message    = 'The executor leaves monitor mode';
       elseif order = 'STOP';
         fStop      = *on;
         message    = 'The executor stops the script';
       elseif order = 'SELF-SUBMIT';
         self_submit();
         message   = 'The job was self-submited';
       endif;
     P                 E
      // -----------------------------------------------------------------------
      // copy
      // -----------------------------------------------------------------------
     pcopyBook         b
     d copyBook        pi
     d   xDta                       228    const
      *
     d xDta_           s            228    varying
     d tCpy            ds
     d  cpy                         228
     d oCpy            ds                  likeDs(tCpy) dim(20000)

     d maxCpy          s              5u 0 inz(20000)
     d cmd             s            100    varying
     d mbr             s             10    varying
     d toFile          s             21    varying
     d p1              s              3u 0
     d oDta2           ds                  likeDs(tDta) dim(20000)
       xDta_ = trim(xDta);
       mbr   =getParm(xDta_:'MBR');
       toFile=getParm(xDta_:'TOFILE':xScrLib$+'/XCOPY');
       // prepare/launch the ovrdbf
       cmd='OVRDBF FILE(XCOPY) TOFILE( F) MBR( M)';
       cmd= scanRpl(' M':mbr:cmd);
       cmd= scanRpl(' F':tofile:cmd);
       qcmdExc(cmd: len(cmd));

       // Load copy sources
       exec sql declare cCopy cursor for
       select srcdta from xCopy y where srcdta<>'' order by rrn(y);
       exec sql open cCopy;
       exec sql fetch next from cCopy for :maxCpy rows into :oCpy;
       exec sql get diagnostics :maxcpy=ROW_COUNT;
       Message='Copy was executed without error message : '
              + trim( char( dec(maxCpy)))+' lines were included';
       exec sql close cCopy;

       cmd='DLTOVR FILE(XCOPY)';
       qCmdExc(cmd: len(cmd));

       // lines are included in the original source
        subarr(oDta2:1)= subarr(oDta:1:iDta);
        subarr(oDta2:iDta+1)= subarr(oCpy:1:maxCpy);
        subarr(oDta2:iDta+1+maxCpy)= subarr(oDta:iDta+1:maxDta-iDta);

       oDta=oDta2;

       maxDta+=MaxCpy;
     p                 e
      // -----------------------------------------------------------------------
      // self submit job
      // -----------------------------------------------------------------------
     pgetParm          b
     d getPArm         pi            80a   varying
     d   cmd                        256a   varying const
     d   prm                         20    varying const
     d   default                     50    varying const options(*nopass)
      *
     d  p              s              3u 0
     d  p2             s              3u 0
     d  l              s              3u 0
     d  prmValue       s             80a    varying
      *
        p= scan(' '+$upper(prm)+'(':$upper(cmd));

         if p=0
        and  parms()=3;
          return default;
        elseif p=0
        and  parms()<3;
          return '';
        endIf;

       p+= len(prm)+2;
       p2= scan(')':cmd:p);
       l=p2-p;

       prmValue= subst(cmd:p:l);

       return prmValue;
     p                 e
      // -----------------------------------------------------------------------
      // self submit job
      // -----------------------------------------------------------------------
     P SELF_SUBMIT     B
     D sbmBuffer       s          10000    varying inz('')
     d v               s              3u 0
       if jobtype='0';
         message='job already in submit mode, order has not been taken care of';
         return;
       endif;
       sbmBuffer='sbmjob cmd(XEXEC SCRIPT('+xScrMbr$+') file('+xScrLIB$+
                 '/'+xScrFIL$+')';
       if MaxParm>0;
         sbmBuffer+=' Parm(';
         for v=1 to maxParm;
           sbmBuffer+=''''+ scanrpl('''':'''''':oParm(v))+''' ';
         endFor;
         sbmBuffer+=')';
       endIf;
       sbmBuffer+=')';
       // other parameters
       sbmBuffer+= subst(buffer: len(order)+1);
       buffer=sbmBuffer;
       RetCod='1';
       fStop=*on;
       addLog(' ':buffer);
       order='SBMJOB';
       goAs400Cmd();
     P                 E
      // -----------------------------------------------------------------------
      // Execute as400 command
      // -----------------------------------------------------------------------
     P goAS400cmd      B
     D goAS400cmd      PI
       // if xExec : increment the recursion call level
       if order='XEXEC';
         buffer='XEXEC$' +  subst(buffer:6);
         if  scan(' FILE(':$upper(buffer))=0;
           buffer+=' FILE('+xScrLib$+'/'+xScrFil$+')';
         endif;
         if  scan(' PARM(':$upper(buffer))=0;
           buffer+=' PARM(*NONE)';
         endIf;
         buffer+=' CALLLVL('+ char(callLvl+1)+')';
       endIf;
       monitor;
         QCmdExc(buffer: len(buffer));
       on-error;
         Message= trim(pgmSds.msgtxt);
         fError=*on;
         return;
       endmon;
       Message='Command '+order+' was executed without error message';
     P                 e
      // -----------------------------------------------------------------------
      // Write in the xLog file
      // -----------------------------------------------------------------------
     PaddLog           B
     d                 pi
     d color                          1    const
     d pDta                         228    const
     d fSync                           n   const options(*nopass)
      *
     D space           s            228    inz('')
     D pDta_           S            228
       pDta_=pDta;
       if  parms()=3;
         pDta_= subst(space:1:margin-1)+ trimR(pDta_);
       endif;
       pDta_=color+ subst(space:1:callLvl*2)+ trimR(pDta_);
       logSeq+=1;
       exec sql insert into xLog (srcdta, srcseq) values(:pDta_,:logSeq);
     p                 e
      // -----------------------------------------------------------------------
      // Message
      // -----------------------------------------------------------------------
     p checkMessage    b
     d t2              s               z
     d duration        s             30a   varying

       if message<>'';
         t2= timeStamp();
         duration=getDuration(t2:t1);
         message+='  '+duration+' ';
       endif;

       if     fError and fMonitor;
         addLog(R:message+G:*on);
       elseif fError and not fMonitor;
         addLog(RB:message+G:*on);
         addLog(RB:'Process canceled due to error above'+G:*on);
         retcod='2';
         fStop=*on;
       elseIf message<>'';
         addLog(B:message+G:*on);
       endIF;

     p                 e
      // -----------------------------------------------------------------------
      // Ending
      // -----------------------------------------------------------------------
     pend              b
     d end             pi
     d t2              s               z
     d duration        s             30a   varying
       // Buffer in not empty
       if buffer<>'';
         addLog(RB:'Buffer is not empty'+G:*on);
         retcod='2';
       endIf;
       // Message for script ending (first call only)
       if callLvl=0;
         t2= timestamp();
         duration=getDuration(t2:t0);
         addLog(GR:'Script '+xScrLib$+'/'+xScrFil$+'.'+xScrMbr$
               +' ended on ' + char( date())+ ' at ' + char( time())
               +'  '+duration+' '+g);
       endIf;
       *inlr=*on;
     p                 e
      // -----------------------------------------------------------------------
      // upper
      // -----------------------------------------------------------------------
     p$upper           b
     d $upper          pi           256    varying
     d  xIn                         256    varying const

       return  xlate('abcdefghijklmnopqrstuvwxyz_'
                    :'ABCDEFGHIJKLMNOPQRSTUVWXYZ-'
                    :xIn);
     p                 e
      // -----------------------------------------------------------------------
      // scanr
      // -----------------------------------------------------------------------
     p$scanr           b
     d $scanr          pi             5u 0
     d  search                       80    varying const
     d  phrase                      256    varying const
      *
     d  p              s              5u 0
       if  len(phrase)< len(search);
         return 0;
       endIf;
       for p= len(phrase)- len(search)+1 downto 1;
         if  subst(phrase:p: len(search))=search;
           return p;
         endif;
       endfor;
       return 0;
     p                 e
