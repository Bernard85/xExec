/*------------------------------------------------------------------*/
/*CRTCMD   CMD(yourLib/XEXEC) PGM(XEXECPPC1)                        */
/*         SRCFILE(YourLib/xexecsrc) SRCMBR(XEXEC)      15/12       */
/*------------------------------------------------------------------*/
             CMD        PROMPT('Execute XScript')

             PARM       KWD(SCRIPT) TYPE(*NAME) LEN(10) MIN(1) +
                          PROMPT('Script')

             PARM       KWD(FILE) TYPE(Q1) PROMPT('File')

 Q1:         QUAL       TYPE(*NAME) LEN(10) DFT(XSCRIPT) +
                          SPCVAL((XSCRIPT))
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
                          SPCVAL((*LIBL)) PROMPT('Library')

             PARM       KWD(VAL) TYPE(*CHAR) LEN(50) MIN(0) MAX(8) +
                          VARY(*YES *INT2) CASE(*MIXED) +
                          PROMPT('Value')
             PARM       KWD(callLvl) TYPE(*DEC) LEN(2) DFT(0) +
                          PROMPT('call Level')
