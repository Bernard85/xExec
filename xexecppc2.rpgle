     H option(*nodebugio) DFTACTGRP(*NO) actgrp(*caller) bnddir('QC2LE')
      //-----------------------------------------------------------------------
      //CRTSQLRPGI OBJ(yourLib/XEXECPPC2)
      //           SRCFILE(yourLib/xExecsrc)
      //           OBJTYPE(*PGM) DBGVIEW(*SOURCE)
      //-----------------------------------------------------------------------
      //IBM functions
      //-----------------------------------------------------------------------
      //command execute
     D QCmdExc         PR                  ExtPgm('QCMDEXC')
     D  CmdTxt                     3000A   Const
     D  CmdLen                       15P 5 Const
      //-----------------------------------------------------------------------
      //structures
      //-----------------------------------------------------------------------
      //Program Status Data Structure
     D  PGMSDS        SDS                  Qualified
     D    MsgId               40     46
     D    MsgTxt              91    170
     D    JOB                244    253
     D    user               254    263
     d    number             264    269
      //
     D tDta            ds
     D  dta                         228
     D oDta            ds                  likeDS(tDta) dim(2000)
     d iDta            s              5u 0
     D maxDta          S              5u 0 inz(2000)
      //Buffer for execution (command or sql)
     D buffer          s          10000    varying inz('')
      //flags
     D fError          s               n
     D fMonitor        s               n
     D fStop           s               n
     d message         S            100a   varying
     D order           s             50A   varying
     d margin          s              3u 0
     d logSeq          S              5u 0
      //array of values
     d maxVal$         c                   const(8)
     d tVal            s             50a   template varying
     d maxVal          s              5u 0 based(pMaxVal)
     d oVal            s                   dim(maxVal$) like(tVal)
      //-----------------------------------------------------------------------
      //main routine
      //-----------------------------------------------------------------------
     d xExecPPC2       Pi
     d   retcod                       1a
     d   xscript                     10    const
     d   lib                         10    const
     d   fil                         10    const
     d   vals                       258a
     d   callLvl                      2p 0 const
     d   jobtype                      1    const
       init();
       main();
       End();
      //-----------------------------------------------------------------------
      //Initialisation
      //-----------------------------------------------------------------------
     pInit             b
     d init            pi
     d n               s              5u 0
     D Valn            S                   based(pValn) like(tVal)
       //Return code
       retcod='0';
       //Allow write
       exec sql SET OPTION commit=*none, DatFmt=*ISO;
       //Message for script begin (first call only)
       if callLvl=0;
       addLog('':'Script '+%trim(lib)+'/'+%trim(fil)+'.'+%trim(xscript)+
              ' started on ' +%char(%date())+ ' at ' +%char(%time())+x'20');
       addLog('':'the processing job is '+pgmsds.number+'/'+
              %trim(pgmsds.user)+'/'+%trim(pgmsds.job)+x'20');
       endIf;
       //Loading script --> oDta() + maxDta
       exec sql declare Input cursor for
       select srcDta from xScript a where srcdta<>'' order by rrn(a);
       //
       exec sql open Input;
       exec sql fetch next from Input for :maxDta rows into :oDta;
       exec sql get diagnostics :maxDta=ROW_COUNT;
       exec sql close Input;
       if maxDta=0;
         addLog('':'Error during script loading');
         retcod='2';
         return;
       endIf;
       //Loading values --> oVal() + maxVal
       pMaxVal=%addr(vals);
       clear oVal;
       for n=1 to maxVal;
         pValn=%addr(vals)+%size(maxVal)+(%size(tVal))*(n-1);
         oVal(n)=Valn;
       endFor;
     pInit             e
      //-----------------------------------------------------------------------
      //main process
      //-----------------------------------------------------------------------
     pmain             b
     d main            pi
     D xDta            s            228
       //Loop on all rows loaded
       for iDta=1 to maxDta;
         //Replace variable values
         xDta=replaceVal(oDta(iDta));
         //copy line to log
         addLog(' ':xDta);
         //Buffer loading
         LoadBuffer(xDta);
         //Try to execute the buffer
         exec();
         //Message
         checkMessage();
         //Stop
         if fStop;
           leave;
         endIf;
       endFor;
     p                 e
      //-----------------------------------------------------------------------
      //Loading Variable values
      //-----------------------------------------------------------------------
     preplaceVal       b
     dreplaceVal       pi           228
     d inDta                        228    const
      *
     d outDta          s            228
     d v               s              3u 0
       outDta=%scanRpl('%0':%trim(xScript):inDta);
       for v=1 to maxVal$;
         outDta=%scanRpl('%'+%char(v):oval(v):outDta);
       endFor;
       return outDta;
     p                 e
      //-----------------------------------------------------------------------
      //Load buffer
      //-----------------------------------------------------------------------
     pLoadBuffer       b
     dLoadBuffer       pi
     D xDta                         228    const
      *
     D trimDta         s            228    varying inz('')
       trimDta=%trim(xDta);
       //Comment line
       if %len(trimDta)>=2 and %subst(trimDta:1:2)='//';
         return;
       endIf;
       //Margin for indentation (log file)
       if buffer='';
         margin=%check(' ':xDta);
         Buffer=trimDta;
       else;
         Buffer+=' '+trimDta;
       endIf;
     p                 e
      //-----------------------------------------------------------------------
      //Execute buffer
      //-----------------------------------------------------------------------
     pExec             b
       fError=*off;
       message='';
       //If the last character is not semicolon it is not a command
       if buffer='' or %subst(buffer:%len(buffer):1)<>';';
         return;
       endIf;
       //Clear final semicolon
       %subst(buffer:%len(buffer):1)=' ';
       //Retreive the order (1st word)
       getOrder();
       //test/launch the buffer
       select;
       when isSql();
         goSql();
       when isSpecif();
         goSpecif();
       other;
         goAS400cmd();
       endsl;
       buffer='';
     p                 e
      //-----------------------------------------------------------------------
      //get order (first word of the buffer)
      //-----------------------------------------------------------------------
     P getOrder        B
     D getOrder        PI
     D  p              s              3u 0
       order='';
       p=%scan(' ':buffer);
       if p=0;
         p=%len(buffer);
       endIf;
       if p>%size(order)-2 or p=0;
         return;
       endIf;
       order=%subst(buffer:1:p-1);
       ORDER=%xlate('abcdefghijklmnopqrstuvwxyz_':
                    'ABCDEFGHIJKLMNOPQRSTUVWXYZ-':order);

     p                 e
      //-----------------------------------------------------------------------
      //is it sql order ?
      //-----------------------------------------------------------------------
     P isSQL           B
     D isSQL           PI              n
       IF order = 'CREATE'
       or order = 'ALTER'
       or order = 'DROP'
       or order = 'INSERT'
       or order = 'UPDATE'
       or order = 'DELETE'
       or order = 'MERGE'
       or order = 'LABEL';
         return *on;
       endif;
       return *off;
     P                 E
      //-----------------------------------------------------------------------
      //is it a specific order ?
      //-----------------------------------------------------------------------
     P isSpecif        B
     D isSpecif        PI              n
       IF order = 'MONITOR'     or
          order = 'END-MONITOR' or
          order = 'STOP'        or
          order = 'SELF-SUBMIT';
         return *on;
       endif;
       return *off;
     P                 E
      //-----------------------------------------------------------------------
      //Execute sql order
      //-----------------------------------------------------------------------
     P goSQL           B
     D goSQL           PI
     D  sqlstate       s              5A
       exec sql execute immediate :buffer;
       exec sql get diagnostics condition 1
              :message=MESSAGE_TEXT,
              :sqlstate = RETURNED_sqlstate;
       if %subst(sqlstate:1:1)<>'0';
         fError=*on;
       endIf;
     P                 e
      //-----------------------------------------------------------------------
      //Execute specific command
      //-----------------------------------------------------------------------
     P goSpecif        B
       select;
       when ORDER = 'MONITOR';
         fMonitor=*on;
         message='The executor enters monitor mode';
       when order = 'END-MONITOR';
         fMonitor=*off;
         message='The executor leaves monitor mode';
       when order = 'STOP';
         fStop=*on;
         message='The executor stops the script';
       when order = 'SELF-SUBMIT';
         self_submit();
         message='The job was self-submited';
       endsl;
     P                 E
      //-----------------------------------------------------------------------
      //self submit job
      //-----------------------------------------------------------------------
     P SELF_SUBMIT     B
     D sbmBuffer       s          10000    varying inz('')
     d v               s              3u 0
       if jobtype='0';
         message='job already in submit mode, order has not been taken care of';
         return;
       endif;
       sbmBuffer='sbmjob cmd(XEXEC SCRIPT('+%trim(xScript)+') file('+%trim(lib)+
                 '/'+%trim(fil)+')';
       if MaxVal>0;
         sbmBuffer+=' val(';
         for v=1 to maxVal;
           sbmBuffer+=''''+%scanrpl('''':'''''':oVal(v))+''' ';
         endFor;
         sbmBuffer+=')';
       endIf;
       sbmBuffer+=')';
       //other parameters
       sbmBuffer+=%subst(buffer:%len(order)+1);
       buffer=sbmBuffer;
       RetCod='1';
       fStop=*on;
       addLog(' ':buffer);
       order='SBMJOB';
       goAs400Cmd();
     P                 E
      //-----------------------------------------------------------------------
      //Execute as400 command
      //-----------------------------------------------------------------------
     P goAS400cmd      B
     D goAS400cmd      PI
       //if xExec : increment the recursion call level
       if order='XEXEC';
         buffer+=' CALLLVL('+%char(callLvl+1)+')';
       endIf;
       monitor;
         QCmdExc(buffer:%len(buffer));
       on-error;
         Message=%trim(pgmSds.msgtxt);
         fError=*on;
         return;
       endmon;
       Message='Command '+order+' was executed without error message';
       return;
     P                 e
      //-----------------------------------------------------------------------
      //Write in the xLog file
      //-----------------------------------------------------------------------
     PaddLog           B
     d                 pi
     d color                          1    const
     d pDta                         228    const
     d fSync                           n   const options(*nopass)
      *
     D space           s            228    inz('')
     D pDta_           S            228
       pDta_=pDta;
       if %parms()=3;
         pDta_=%subst(space:1:margin-1)+%trimR(pDta_);
       endif;
       pDta_=color+%subst(space:1:callLvl*2)+%trimR(pDta_);
       logSeq+=1;
       exec sql insert into xLog (srcdta, srcseq) values(:pDta_,:logSeq);
     p                 e
      //-----------------------------------------------------------------------
      //Message
      //-----------------------------------------------------------------------
     p checkMessage    b
       select;
       when fError and fMonitor;
         addLog('':message+'':*on);
       when fError and not fMonitor;
         addLog('':message+'':*on);
         addLog('':'Process canceled due to error above':*on);
         retcod='2';
         fStop=*on;
       when message<>'';
         addLog('':message+'':*on);
       endSl;
       return;
     p                 e
      //-----------------------------------------------------------------------
      //Ending
      //-----------------------------------------------------------------------
     pend              b
     d end             pi
       //Buffer in not empty
       if buffer<>'';
         addLog('':'Buffer is not empty':*on);
         retcod='2';
       endIf;
       //Message for script ending (first call only)
       if callLvl=0;
       addLog('':'Script '+%trim(lib)+'/'+%trim(fil)+'.'+%trim(xscript)+
              ' ended on ' +%char(%date())+ ' at ' +%char(%time())+'');
       endIf;
       *inlr=*on;
     p                 e
