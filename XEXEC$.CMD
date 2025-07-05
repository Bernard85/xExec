/* VERSION: 2025-07-07                                               */
/* ------------------------------------------------------------------*/
/* CRTCMD   CMD(BERNARD85/XEXEC)                                     */
/*          PGM(BERNARD85/XEXECPPC1)                                 */
/*          SRCFILE(BERNARD85/SRC)                                   */
/*          SRCMBR(XEXEC)                                            */
/* ------------------------------------------------------------------*/
             CMD        PROMPT('Execute XScript')

             PARM       KWD(SCRIPT) TYPE(*NAME) LEN(10) MIN(1) +
                          PROMPT('Script')

             PARM       KWD(FILE) TYPE(Q1) PROMPT('File')

 Q1:         QUAL       TYPE(*NAME) LEN(10) DFT(XSCRIPT) +
                          SPCVAL((XSCRIPT))
             QUAL       TYPE(*NAME) LEN(10) DFT(*LIBL) +
                          SPCVAL((*LIBL)) PROMPT('Library')

             PARM       KWD(PARM) TYPE(*CHAR) LEN(80) DFT(*NONE) +
                          SPCVAL((*NONE *NONE)) MIN(0) MAX(9) +
                          VARY(*YES *INT2) CASE(*MIXED) +
                          PROMPT('Parameter')
             PARM       KWD(CALLLVL) TYPE(*DEC) LEN(2) DFT(0) +
                          PROMPT('call Level')
