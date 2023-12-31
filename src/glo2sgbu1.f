      MODULE GLOSGBMODULE
        INTEGER,SAVE,POINTER ::NSGB,MXSGB,NSGBVL,ISGBCB,IPRSGB
        INTEGER,SAVE,POINTER  ::NPSGB,ISGBPB,NNPSGB
        CHARACTER(LEN=16),SAVE, DIMENSION(:),   ALLOCATABLE     ::SGBAUX
        REAL,             SAVE, DIMENSION(:,:), ALLOCATABLE     ::SGB
      END MODULE GLOSGBMODULE
C
      SUBROUTINE GLO2SGBU1AR(IN)
C     ******************************************************************
C     ALLOCATE ARRAY STORAGE FOR SPECIFIED GRADIENT BOUNDARY PACKAGE
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,NODES,IUNSTR,NEQS,INSGB
      USE GLOSGBMODULE, ONLY:NSGB,MXSGB,NSGBVL,ISGBCB,IPRSGB,NPSGB,
     1                       ISGBPB,NNPSGB,SGBAUX,SGB
C
      CHARACTER*400 LINE
C     ------------------------------------------------------------------
      ALLOCATE(NSGB,MXSGB,NSGBVL,ISGBCB,IPRSGB)
      ALLOCATE(NPSGB,ISGBPB,NNPSGB)
C
C1------IDENTIFY PACKAGE AND INITIALIZE NSGB.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'SGB -- SPECIFIED GRADIENT PACKAGE, VERSION 1,',
     1' 1/22/2013, INPUT READ FROM UNIT ',I4)
      NSGB=0
      NNPSGB=0
      INSGB=1
C
C2------READ MAXIMUM NUMBER OF SGB CELLS AND UNIT OR FLAG FOR
C2------CELL-BY-CELL FLOW TERMS.
      CALL URDCOM(IN,IOUT,LINE)
      CALL UPARLSTAL(IN,IOUT,LINE,NPSGB,MXS)
      IF(IFREFM.EQ.0) THEN
         READ(LINE,'(2I10)') MXACTS,ISGBCB
         LLOC=31
      ELSE
         LLOC=1
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,MXACTS,R,IOUT,IN)
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ISGBCB,R,IOUT,IN)
      END IF
      WRITE(IOUT,3) MXACTS
    3 FORMAT(1X,'MAXIMUM OF ',I6,' ACTIVE SGB CELLS AT ONE TIME')
      IF(ISGBCB.LT.0) WRITE(IOUT,7)
    7 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE PRINTED WHEN ICBCFL NOT 0')
      IF(ISGBCB.GT.0) WRITE(IOUT,8) ISGBCB
    8 FORMAT(1X,'CELL-BY-CELL FLOWS WILL BE SAVED ON UNIT ',I4)
      WRITE(IOUT,9) MXACTS,ISGBCB
    9 FORMAT(1X,'MAXIMUM NUMBER OF SGB CELLS    (MXACTS) =',I7
     *  /1X,    'C-B-C FLUX FLAG OR UNIT NUMBER (ISGBCB) =',I3)
C
C3------READ AUXILIARY VARIABLES AND PRINT FLAG.
      ALLOCATE(SGBAUX(20))
      NAUX=0
      IPRSGB=1
   10 CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
      IF(LINE(ISTART:ISTOP).EQ.'AUXILIARY' .OR.
     1        LINE(ISTART:ISTOP).EQ.'AUX') THEN
         CALL URWORD(LINE,LLOC,ISTART,ISTOP,1,N,R,IOUT,IN)
         IF(NAUX.LT.20) THEN
            NAUX=NAUX+1
            SGBAUX(NAUX)=LINE(ISTART:ISTOP)
            WRITE(IOUT,12) SGBAUX(NAUX)
   12       FORMAT(1X,'AUXILIARY SGB VARIABLE: ',A)
         END IF
         GO TO 10
      ELSE IF(LINE(ISTART:ISTOP).EQ.'NOPRINT') THEN
         WRITE(IOUT,13)
   13    FORMAT(1X,'LISTS OF SGB CELLS WILL NOT BE PRINTED')
         IPRSGB = 0
         GO TO 10
      END IF
C3A-----THERE ARE TWO INPUT VALUES PLUS ONE LOCATION FOR
C3A-----CELL-BY-CELL FLOW - ADD 2 DUMMY LOCATIONS FOR STRUCTURED GRIDS
      NSGBVL=5+NAUX
C
C4------ALLOCATE SPACE FOR THE SGB DATA.
      ISGBPB=MXACTS+1
      MXSGB=MXACTS+MXS
      IF(MXSGB.LT.1) THEN
         WRITE(IOUT,17)
   17    FORMAT(1X,
     1'Deactivating the SGB Package because MXSGB=0')
         IN=0
      END IF
      ALLOCATE (SGB(NSGBVL,MXSGB))
C
C5------READ NAMED PARAMETERS.
      WRITE(IOUT,18) NPSGB
   18 FORMAT(1X,//1X,I5,' SGB parameters')
      IF(NPSGB.GT.0) THEN
        LSTSUM=ISGBPB
        DO 120 K=1,NPSGB
          LSTBEG=LSTSUM
          CALL UPARLSTRP(LSTSUM,MXWELL,IN,IOUT,IP,'SGB','SGB',1,
     &                   NUMINST)
          NLST=LSTSUM-LSTBEG
          IF(NUMINST.EQ.0) THEN
C5A-----READ PARAMETER WITHOUT INSTANCES.
           CALL ULSTRDU(NLST,SGB,LSTBEG,NSGBVL,MXSGB,1,IN,
     &      IOUT,'SGB NO.       NODE       GRADIENT     ',
     &      SGBAUX,20,NAUX,IFREFM,NEQS,2,2,IPRSGB)
          ELSE
C5B-----READ INSTANCES.
            NINLST=NLST/NUMINST
            DO 110 I=1,NUMINST
            CALL UINSRP(I,IN,IOUT,IP,IPRSGB)
            CALL ULSTRDU(NINLST,SGB,LSTBEG,NSGBVL,MXSGB,1,IN,
     &        IOUT,'SGB NO.       NODE       GRADIENT     ',
     &        SGBAUX,20,NAUX,IFREFM,NEQS,2,2,IPRSGB)
            LSTBEG=LSTBEG+NINLST
  110       CONTINUE
          END IF
  120   CONTINUE
      END IF
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GLO2SGBU1RP(IN)
C     ******************************************************************
C     READ SGB DATA FOR A STRESS PERIOD
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,ONLY:IOUT,NCOL,NROW,NLAY,IFREFM,NODES,IUNSTR,NEQS,INCLN
      USE GLOSGBMODULE, ONLY:NSGB,MXSGB,NSGBVL,IPRSGB,NPSGB,
     1                       ISGBPB,NNPSGB,SGBAUX,SGB
C
      CHARACTER*6 CSGB
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
      WRITE(IOUT,1)IN
    1 FORMAT(1X,/1X,'SGB -- SPECIFIED GRADIENT PACKAGE, VERSION 1,',
     1' 1/22/2013, INPUT READ FROM UNIT ',I4)
C
C1----READ NUMBER OF SGB CELLS (OR FLAG SAYING REUSE SGB DATA).
C1----AND NUMBER OF PARAMETERS
      IF(IFREFM.EQ.0) THEN
        READ(IN,'(2I10)') ITMP,NP
      ELSE
        READ(IN,*) ITMP,NP
      END IF
C
C------Calculate some constants.
      NAUX=NSGBVL-5
      IOUTU = IOUT
      IF (IPRSGB.EQ.0) IOUTU=-IOUTU
C
C1A-----IF ITMP LESS THAN ZERO REUSE NON-PARAMETER DATA. PRINT MESSAGE.
C1A-----IF ITMP=>0, SET NUMBER OF NON-PARAMETER SGB CELLS EQUAL TO ITMP.
      IF(ITMP.LT.0) THEN
         WRITE(IOUT,6)
    6    FORMAT(1X,/
     1    1X,'REUSING NON-PARAMETER SGB CELLS FROM LAST STRESS PERIOD')
      ELSE
         NNPSGB=ITMP
      END IF
C
C1B-----IF THERE ARE NEW NON-PARAMETER SGB CELLS, READ THEM.
      MXACTS=ISGBPB-1
      IF(ITMP.GT.0) THEN
        IF(NNPSGB.GT.MXACTS) THEN
          WRITE(IOUT,99) NNPSGB,MXACTS
   99     FORMAT(1X,/1X,'THE NUMBER OF ACTIVE SGB CELLS (',I6,
     1                   ') IS GREATER THAN MXACTS(',I6,')')
          CALL USTOP(' ')
        END IF
C
        CALL ULSTRDU(NNPSGB,SGB,1,NSGBVL,MXSGB,1,IN,IOUT,
     &  'SGB NO.       NODE       GRADIENT     ',
     &   SGBAUX,20,NAUX,IFREFM,NEQS,2,2,IPRSGB)
      END IF
      NSGB=NNPSGB
C
C1C-----IF THERE ARE ACTIVE SGB PARAMETERS, READ THEM AND SUBSTITUTE
      CALL PRESET('Q')
      NREAD=NSGBVL-1
      IF(NP.GT.0) THEN
         DO 30 N=1,NP
         CALL UPARLSTSUB(IN,'SGB',IOUTU,'G',SGB,NSGBVL,MXSGB,NREAD,
     1                MXACTS,NSGB,2,2,
     2            'SGB NO.         NODE          GRADIENT   ',
     3            SGBAUX,20,NAUX)
   30    CONTINUE
      END IF
C
C3------PRINT NUMBER OF SGB CELLS IN CURRENT STRESS PERIOD.
      CSGB='   SGB'
      IF(NSGB.EQ.1) CSGB='  SGB '
      WRITE(IOUT,101) NSGB,CSGB
  101 FORMAT(1X,/1X,I6,A)
C
C6------RETURN
      RETURN
      END
      SUBROUTINE GLO2SGBU1FM
C     ******************************************************************
C     SUBTRACT Q FROM RHS FOR SGB
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IBOUND,RHS,AMAT,IA,TOP,BOT,HNEW,NODES,AKR,NODLAY,
     1  NLAY
      USE GWFBCFMODULE, ONLY: HK
      USE GLOSGBMODULE, ONLY:NSGB,SGB
      USE CLN1MODULE, ONLY: ACLNNDS
      USE SMSMODULE, ONLY: EPSILON
      DOUBLE PRECISION Q,QA,QEPS,DQ,BBOT,TTOP,THCK,HD,TOTTHICK,SW,EKR
C     ------------------------------------------------------------------
C
C1------IF NUMBER OF SGB CELLS <= 0 THEN RETURN.
      IF(NSGB.LE.0) RETURN
C
C2------PROCESS EACH SGB CELL IN THE LIST.
      DO 100 L=1,NSGB
      N=SGB(1,L)
      Q=SGB(4,L)
C
C2A-----IF THE CELL IS INACTIVE THEN BYPASS PROCESSING.
      IF(IBOUND(N).LE.0) GO TO 100
C
C2B-----COMPUTE FLUX AND ITS DERIVATIVE.
        IPIV = IA(N)
        HD = HNEW(N)+EPSILON
        IF(N.GT.NODES)THEN
          QA = Q * AKR(N)
          IFN = N-NODES
          BBOT = ACLNNDS(IFN,5)
          CALL CLN_THIK(IFN,HD,BBOT,THCK)
          CALL CLN_AKR(IFN, HD, THCK)
          QEPS = Q * THCK
        ELSE
C---------FIND LAYER FOR NODE            
          KK = 0  
          DO K=1,NLAY
            NNDLAY = NODLAY(K)
            NSTRT = NODLAY(K-1)+1
            IF(N.GE.NSTRT.AND.N.LE.NNDLAY)THEN
              KK = K
              GO TO 110
            ENDIF  
          ENDDO  
110       CONTINUE
C ------------------------------------
C --------COMPUTE FLUX           
          QA = Q * HK(N) * AKR(N)
          BBOT=BOT(N)
          TTOP=TOP(N)
          TOTTHICK = TTOP - BBOT
          CALL SAT_THIK(N,HD,TOTTHICK,BBOT,SW,KK)
          CALL KR_CAL(N,SW,EKR,KK,HD)
          QEPS = Q * HK(N) * EKR
        ENDIF
C-------CALCULATE DQ/DH
        DQ = (QEPS - QA) / EPSILON
        AMAT(IPIV) = AMAT(IPIV) + DQ
        RHS(N) = RHS(N) - QA + DQ*HD
  100 CONTINUE
C
C3------RETURN
      RETURN
      END
c ---------------------------------------------------
      SUBROUTINE GLO2SGBU1BD(KSTP,KPER)
C     ******************************************************************
C     CALCULATE VOLUMETRIC BUDGET FOR SGB CELLS
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL,      ONLY:IOUT,NCOL,NROW,NLAY,IBOUND,BUFF,NODES,
     1                  IUNSTR,TOP,BOT,HNEW,NEQS,INCLN,AKR,NODLAY,FMBE
      USE GWFBCFMODULE, ONLY: HK
      USE CLN1MODULE, ONLY: ACLNNDS,NCLNNDS,ICLNCB
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,IAUXSV,DELT,PERTIM,TOTIM,
     1                      VBVL,VBNM
      USE GLOSGBMODULE,ONLY:NSGB,ISGBCB,SGB,NSGBVL,SGBAUX
C
      CHARACTER*16 TEXT(2)
      DOUBLE PRECISION RATIN,RATOUT,QQ
      DOUBLE PRECISION BBOT,TTOP,THCK,HD,TOTTHICK,SW,EKR
      DATA TEXT(1) /'       SGB CELLS'/
      DATA TEXT(2) /'   CLN SGB CELLS'/
C     ------------------------------------------------------------------
C
C1------CLEAR RATIN AND RATOUT ACCUMULATORS, AND SET CELL-BY-CELL
C1------BUDGET FLAG.
      ZERO=0.
      RATIN=ZERO
      RATOUT=ZERO
      IBD=0
      IF(ISGBCB.LT.0 .AND. ICBCFL.NE.0) IBD=-1
      IF(ISGBCB.GT.0) IBD=ICBCFL
      IBDLBL=0
C
C2-----IF CELL-BY-CELL FLOWS WILL BE SAVED AS A LIST, WRITE HEADER.
      IF(IBD.EQ.2) THEN
         NAUX=NSGBVL-5
         IF(IAUXSV.EQ.0) NAUX=0
         IICLNCB=0
         NNCLNNDS=0
         IF(INCLN.GT.0) THEN
           IICLNCB=ICLNCB
           NNCLNNDS=NCLNNDS
         ENDIF
         CALL UBDSVHDR(IUNSTR,KSTP,KPER,IOUT,ISGBCB,IICLNCB,NODES,
     1    NNCLNNDS,NCOL,NROW,NLAY,NSGB,NSGBVL,NAUX,IBOUND,
     2    TEXT(1),SGBAUX,DELT,PERTIM,TOTIM,SGB)
      END IF
C
C3------CLEAR THE BUFFER.
      DO 50 N=1,NEQS
      BUFF(N)=ZERO
50    CONTINUE
C
C4------IF THERE ARE NO SGB CELLS, DO NOT ACCUMULATE FLOW.
      IF(NSGB.EQ.0) GO TO 200
C
C5------LOOP THROUGH EACH SGB CELL CALCULATING FLOW.
      DO 100 L=1,NSGB
C
C5A-----GET NODE NUMBER OF CELL CONTAINING WELL.
      N=SGB(1,L)
      Q=ZERO
C
C5B-----IF THE CELL IS NO-FLOW OR CONSTANT_HEAD, IGNORE IT.
      IF(IBOUND(N).LE.0)GO TO 99
C
C5C-----GET FLOW RATE FROM SGB CELL LIST.
      QQ=SGB(4,L)
C-------HONOR SUPPLY/DEMAND CONDITIONS FOR EXTRACTION WELLS
      HD = HNEW(N)    
      IF(N.GT.NODES)THEN
        IFN = N-NODES
        BBOT = ACLNNDS(IFN,5)
        CALL CLN_THIK(IFN,HD,BBOT,THCK)
        CALL CLN_AKR(IFN, HD, THCK)
        QQ = QQ * THCK  
      ELSE
        KK = 0  
        DO K=1,NLAY
          NNDLAY = NODLAY(K)
          NSTRT = NODLAY(K-1)+1
          IF(N.GE.NSTRT.AND.N.LE.NNDLAY)THEN
            KK = K
            GO TO 110
          ENDIF  
        ENDDO  
110     CONTINUE
C                 
        BBOT=BOT(N)
        TTOP=TOP(N)
        TOTTHICK = TTOP - BBOT
        CALL SAT_THIK(N,HD,TOTTHICK,BBOT,SW,KK)
        CALL KR_CAL(N,SW,EKR,KK,HD)
        QQ = QQ * HK(N) * EKR
      ENDIF
      Q=QQ
C
C5D-----PRINT FLOW RATE IF REQUESTED.
      IF(IBD.LT.0) THEN
         IF(IBDLBL.EQ.0) WRITE(IOUT,61) TEXT(1),KPER,KSTP
   61    FORMAT(1X,/1X,A,'   PERIOD ',I4,'   STEP ',I3)
        IF(IUNSTR.EQ.0)THEN
          IF(N.GT.NODES)THEN
            WRITE(IOUT,64)N-NODES,Q
   64       FORMAT(1X,'SGB CELL',I6,'   RATE',1PG15.6)
          ELSE
            IL = (N-1) / (NCOL*NROW) + 1
            IJ = N - (IL-1)*NCOL*NROW
            IR = (IJ-1)/NCOL + 1
            IC = IJ - (IR-1)*NCOL
            WRITE(IOUT,62) L,IL,IR,IC,Q
   62       FORMAT(1X,'SGB CELL ',I6,'   LAYER ',I3,'   ROW ',I5,
     1       '   COL ',I5,'   RATE ',1PG15.6)
          ENDIF
        ELSE
           WRITE(IOUT,63) L,N,Q
   63    FORMAT(1X,'SGB CELL ',I6,'    NODE ',I8,'   RATE ',1PG15.6)
        ENDIF
         IBDLBL=1
      END IF
C
C5E-----ADD FLOW RATE TO BUFFER.
      BUFF(N)=BUFF(N)+QQ
      FMBE(N) = FMBE(N) + QQ
C
C5F-----SEE IF FLOW IS POSITIVE OR NEGATIVE.
      IF(QQ.GE.ZERO) THEN
C
C5G-----FLOW RATE IS POSITIVE (RECHARGE). ADD IT TO RATIN.
        RATIN=RATIN+QQ
      ELSE
C
C5H-----FLOW RATE IS NEGATIVE (DISCHARGE). ADD IT TO RATOUT.
        RATOUT=RATOUT-QQ
      END IF
C
C5I-----IF SAVING CELL-BY-CELL FLOWS IN A LIST, WRITE FLOW.  ALSO
C5I-----COPY FLOW TO WELL LIST.

   99 CONTINUE
      IF(IBD.EQ.2)THEN
        CALL UBDSVREC(IUNSTR,N,NODES,NNCLNNDS,ISGBCB,IICLNCB,NSGBVL,
     1    6,NAUX,Q,SGB(:,L),IBOUND,NCOL,NROW,NLAY)
      ENDIF
      SGB(NSGBVL,L)=Q
  100 CONTINUE
C
C6------IF CELL-BY-CELL FLOWS WILL BE SAVED AS A 3-D ARRAY,
C6------CALL UBUDSV TO SAVE THEM.
      IF(IUNSTR.EQ.0)THEN
        IF(IBD.EQ.1)CALL UBUDSV(KSTP,KPER,TEXT(1),ISGBCB,BUFF(1),NCOL,
     1                   NROW,NLAY,IOUT)
        IF(IBD.EQ.1.AND.INCLN.GT.0)THEN
          IF(ICLNCB.GT.0) CALL UBUDSVU(KSTP,KPER,TEXT(2),ICLNCB,
     1         BUFF(NODES+1),NCLNNDS,IOUT,PERTIM,TOTIM)
        ENDIF
      ELSE
        IF(IBD.EQ.1) CALL UBUDSVU(KSTP,KPER,TEXT(1),ISGBCB,BUFF,NEQS,
     1                          IOUT,PERTIM,TOTIM)
      ENDIF
C
C7------MOVE RATES, VOLUMES & LABELS INTO ARRAYS FOR PRINTING.
  200 RIN=RATIN
      ROUT=RATOUT
      VBVL(3,MSUM)=RIN
      VBVL(4,MSUM)=ROUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+RIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+ROUT*DELT
      VBNM(MSUM)=TEXT(1)
C
C8------INCREMENT BUDGET TERM COUNTER(MSUM).
      MSUM=MSUM+1
C
C9------RETURN
      RETURN
      END
      SUBROUTINE GLO2SGBU1DA
C  Deallocate WEL MEMORY
      USE GLOSGBMODULE
C
        DEALLOCATE(NSGB)
        DEALLOCATE(MXSGB)
        DEALLOCATE(NSGBVL)
        DEALLOCATE(ISGBCB)
        DEALLOCATE(IPRSGB)
        DEALLOCATE(NPSGB)
        DEALLOCATE(ISGBPB)
        DEALLOCATE(NNPSGB)
        DEALLOCATE(SGBAUX)
        DEALLOCATE(SGB)
C
      RETURN
      END

