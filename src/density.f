C -------------------------------------------------------------------------------------
      MODULE DDFMODULE
C
        INTEGER, SAVE, POINTER :: IDDF, ITHICKAV, IMPHDD 
        DOUBLE PRECISION, SAVE, POINTER :: RHOFRESH,RHOSTD,CSTD
        DOUBLE PRECISION, SAVE, POINTER :: DRHODC
        DOUBLE PRECISION, SAVE,DIMENSION(:),ALLOCATABLE::
     1         RHONORM, ZEECELL, THICKCELL
C        DOUPLE PRECISION, SAVE, DIMENSION (:), ALLOCATABLE :: AMATDD
c -------amatdd to save matrix before implicit dd term; but no need since we recompute (good to debug)
C
      END MODULE DDFMODULE
C
C -------------------------------------------------------------------------------------
C
      SUBROUTINE DDF1AR(IUDDF)
C     ******************************************************************
C     ALLOCATE SPACE AND READ INFORMATION FOR DENSITY DEPENDENT FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------c
      USE DDFMODULE
      USE GLOBAL,ONLY:IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,NJA
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION ZEE,RNORM,THICK
      REAL TEMPVAR
      CHARACTER*400 LINE
C
C     ------------------------------------------------------------------
C
C1------IDENTIFY PACKAGE.
        INDDF = IUDDF
        WRITE(IOUT,1)INDDF
    1   FORMAT(1X,/1X,'DDF -- DENSITY DEPENDENT FLOW MODULE ',
     1    'VERSION 1, 4/4/2016 INPUT READ FROM UNIT ',I4)
C
C2------ALLOCATE SCALAR VARIABLES VECTORS, AND INITIALIZE.
      ALLOCATE(IDDF, ITHICKAV, IMPHDD)
        IDDF = 1  ! INDEX THAT DENSITY DEPENDENT FLOW IS ACTIVE ON ONE SPECIES (HARDWIRE)
      ALLOCATE(RHOFRESH,RHOSTD,CSTD)
      ALLOCATE (DRHODC)
      ALLOCATE(RHONORM(NEQS),ZEECELL(NEQS))
C
C3------READ FRESHWATER DENSITY, STANDARD SOLUTION DENSITY AND STANDARD CONCENTRATION
      CALL URDCOM(INDDF,IOUT,LINE)
      IF(IFREFM.EQ.0) THEN
        READ(LINE,'(5F10.4)') RHOFRESH,RHOSTD,CSTD,ITHICKAV,IMPHDD
        LLOC=51
      ELSE
        LLOC=1
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TEMPVAR,IOUT,INDDF)
        RHOFRESH = TEMPVAR
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TEMPVAR,IOUT,INDDF)
        RHOSTD = TEMPVAR
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,3,I,TEMPVAR,IOUT,INDDF)
        CSTD = TEMPVAR 
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,ITHICKAV,R,IOUT,INDDF)
        CALL URWORD(LINE,LLOC,ISTART,ISTOP,2,IMPHDD,R,IOUT,INDDF)
      END IF
      IF(ITHICKAV.NE.0) THEN
        ALLOCATE(THICKCELL(NEQS))  
      ENDIF    
c      IF(IMPHDD.NE.0) THEN 
c        ALLOCATE(AMATDD(NJA))  
c      ENDIF       
C---------------------------------------------------------------------------
C3A-----REFLECT INPUT IN OUTPUT LISTING FILE
      WRITE(IOUT,3) RHOFRESH,RHOSTD,CSTD,ITHICKAV,IMPHDD
    3 FORMAT(1X,'DENSITY OF FRESHWATER (RHOFRESH) =',F15.3,
     1  /1X,'DENSITY OF STANDARD SOLUTION (RHOSTD) =', F15.3,
     1  /1X,'CONCENTRATION OF STANDARD SOLUTION (CSTD) =',F15.3
     1  /1X,'FLAG FOR USING THICKNESS WEIGHTED AVERAGE (ITHICKAV) =',I2
     1  /1X,'FLAG FOR IMPLICIT TREATMENT OF HEAD TERM (IMPHDD) =',I2 )
C---------------------------------------------------------------------------
C4------FILL ARRAY  ZEECELL, INITIALIZE RHO/RHOFRESH, AND 
C4------INITIALIZE HNEW TO BE THE NORMALIZED POTENTIAL (RHO/RHOFRESH*HEAD)
      DRHODC = (RHOSTD - RHOFRESH)/CSTD 
      DO N=1,NEQS
        CALL RHONORMCALC(N,RNORM)  
        RHONORM(N) = RNORM  
C        
        CALL ZCELLCALC(N,ZEE,THICK)
        ZEECELL(N) = ZEE
        IF(ITHICKAV.NE.0) THEN 
          THICKCELL(N) = THICK
        ENDIF  
C        
      ENDDO
C---------------------------------------------------------------------------
C5----RETURN
      RETURN
      END
C--------------------------------------------------------------------------- 
      SUBROUTINE DDF1AD(IUDDF)
C     ******************************************************************
C     ADVANCE DENSITY OF ALL CELLS FROM CONCENTRATION AFTER TRANSPORT 
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE, ONLY: DRHODC,RHONORM
      USE GLOBAL,ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,SN
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION ZEE,RNORM
      CHARACTER*400 LINE
C
C     ------------------------------------------------------------------
C1----LOOP OVER ALL CELLS
      DO N=1,NEQS
C2------COMPUTE RHO/RHOFRESH
c        IF(CONC(N,1).GT.CSTD) CONC(N,1) = CSTD
c        IF(CONC(N,1).LT.0.0) CONC(N,1) = 0.0
        CALL RHONORMCALC(N,RNORM)  
        RHONORM(N) = RNORM  
      ENDDO
C---------------------------------------------------------------------------
C3----RETURN
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE RHONORMCALC(N,RNORM)
C     ******************************************************************
C     CALCULATE THE NORMALIZED RHO FOR A GIVEN CONC AT CELL N
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE
      USE GLOBAL,ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,Sn
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION RNORM,CNC
C     ------------------------------------------------------------------
C
C1----COMPUTE RNORM
      CNC = CONC(N,1)
      IF(CNC.LT.0.0) CNC = 0.0
      IF(CNC.GT.CSTD) CNC = CSTD
      RNORM = (RHOFRESH + Sn(N) * DRHODC * CNC)/RHOFRESH
C---------------------------------------------------------------------------
C2----RETURN
      RETURN
      END      
C---------------------------------------------------------------------------
      SUBROUTINE ZCELLCALC(N,ZEE,THICK)
C     ******************************************************************
C     CALCULATE CELL ELEVATION FOR DENSITY DEPENDENT FLOW
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE DDFMODULE
      USE GLOBAL, ONLY: IUNIT,IOUT,NEQS,NODES,IFREFM,IUNSTR,INDDF,HNEW,
     1            TOP,BOT
      USE CLN1MODULE, ONLY: ACLNNDS
      USE GWTBCTMODULE, ONLY: CONC
      DOUBLE PRECISION ZEE,FRAD,FLENG,FANGLE,FDPTH,THICK
C     ------------------------------------------------------------------
C
C1----CHECK IF CELL IS BCF OR CLN
      IF(N.LE.NODES)THEN
C2------GROUNDWATER CELL ELEVATION IS AVERAGE OF TOP AND BOT
        ZEE = 0.5 * (TOP(N) + BOT(N))
        THICK = TOP(N) - BOT(N)
      ELSE 
C3------CLN CELL ELEVATION SET AT BOTTOM OF THE CLN CELL (LOCAL CELL INDEX)
        I = N - NODES
        ZEE = ACLNNDS(I,5) 
C4------ADJUST CLN CELL ELEVATION TO CENTER DEPENDING ON ORITENTATION
        IFDIR = ACLNNDS(I,3) 
        IF(IFDIR.EQ.2)THEN 
C5--------ANGLED PIPE            
          FLENG = ACLNNDS(I,4) 
          FANGLE = ACLNNDS(I,6)   
          FDPTH = FLENG * SIN(FANGLE)
        ELSEIF(IFDIR.EQ.1)THEN
C6--------HORIZONTAL PIPE
          IFTYP =  ACLNNDS(I,2)
          CALL CLNR(IFTYP,FRAD)
C          FDPTH = 2.0 * FRAD
          FDPTH = FRAD
        ELSEIF(IFDIR.EQ.0)THEN  
C7--------VERTICAL PIPE
         FLENG = ACLNNDS(I,4) 
         FDPTH = FLENG
        ENDIF  
        ZEE = ZEE + 0.5 * FDPTH 
        THICK = FDPTH
      ENDIF
C---------------------------------------------------------------------------
C8----RETURN
      RETURN
      END      
C---------------------------------------------------------------------------
      SUBROUTINE DDF1FM(KSTP,KPER)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS DUE TO DENSITY TERM
C     FOR TRANSIENT SIMULATION ALSO ADD DENSITY STORAGE TERM      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IBOUND,NEQS,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,
     1                AMAT,RHS,Sn,ISSFLG,AREA,TOP,BOT,HNEW,HOLD
      USE GWFBASMODULE,ONLY:DELT
      USE SMSMODULE, ONLY: AMATFL
      USE GWTBCTMODULE, ONLY: PRSITY,CONC,CONCO
      USE CLN1MODULE, ONLY: ACLNNDS
      USE DDFMODULE, ONLY: ZEECELL,RHONORM,DRHODC,RHOFRESH,CSTD,
     1  THICKCELL, ITHICKAV, IMPHDD                                     !  , AMATDD
C
      DOUBLE PRECISION QCON,ZEENJJ,RHO,VOL,HPHI,RHOTERM,AMAT_TERM,
     1  CNC,CNCO,GRHO,OMEGA
C     ------------------------------------------------------------------
C     
C0 -----SAVE AMAT INTO AMATDD FOR IMPLICIT UPDATE  (NO NEED, CAN BACK OUT THE TERMS)    
c      IF(IMPHDD.EQ.1) THEN 
c        DO IJA = 1,NJA 
c          AMATDD(IJA) = AMAT(IJA)  
c        ENDDO
c      ENDIF  
C1------FILL DENSITY GRADIENT TERM FOR EVERY CELL
      DO N=1,NEQS
C
C2------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).LE.0) CYCLE
C
C3------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(IBOUND(JJ).LE.0) CYCLE
C4--------COMPUTE AVERAGE Z AND H OF THE TWO CELLS 
          IF(ITHICKAV.EQ.0) THEN 
            ZEENJJ = (ZEECELL(N) + ZEECELL(JJ)) * 0.5
            HPHI = (HNEW(N) + HNEW(JJ)) * 0.5  
          ELSE
            OMEGA = THICKCELL(N) / (THICKCELL(N) + THICKCELL(JJ))  
            ZEENJJ = (1.0 - OMEGA) * ZEECELL(N) + OMEGA * ZEECELL(JJ) 
            HPHI = (1.0 - OMEGA) * HNEW(N) + OMEGA * HNEW(JJ) 
          ENDIF  
C5--------CALCULATE FLOW DUE TO DENSITY TERM         
          IF(IMPHDD.EQ.0) THEN 
            QCON = AMAT(II) *(HPHI - ZEENJJ) * (RHONORM(N)-RHONORM(JJ))
          ELSE 
             QCON = AMAT(II) *(- ZEENJJ) * (RHONORM(N)-RHONORM(JJ))  
          ENDIF  
C6--------ADD THIS FLOW TERM ON RHS OF N        
          RHS(N) = RHS(N) + QCON 
        ENDDO
C
      ENDDO
C ------------------------------------------------------------------------------------
C7------UPDATE FLOW MATRIX WITH DENSITY TERM 
      DO N=1,NEQS
C
C8------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).LE.0) CYCLE
C
C9------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(JJ.LT.N. OR. IBOUND(JJ).LE.0) CYCLE
C10--------COMPUTE AVERAGE DENSITY OF THE TWO CELLS 
          IF(ITHICKAV.EQ.0) THEN 
            RHOTERM = (RHONORM(N) + RHONORM(JJ)) * 0.5
          ELSE
            OMEGA = THICKCELL(N) / (THICKCELL(N) + THICKCELL(JJ))   
            RHOTERM = OMEGA * RHONORM(N) + (1.0 - OMEGA) * RHONORM(JJ)
          ENDIF
C11 --------ADJUST MATRIX FOR DENSITY TERM DEPENDING ON IMPLICIT OR EXPLICIT         
          IF(IMPHDD. EQ. 0) THEN 
C11A -------FOR EXPLICIT UPDATE ON RHS ONLY ADJUST BY * RHOTERM TO ORIGINAL K-VALUE              
            AMAT_TERM = AMAT(II) 
            AMAT(II) = AMAT_TERM * RHOTERM
            AMAT(IA(N)) = AMAT(IA(N)) + AMAT_TERM * (1.0 - RHOTERM)
C            
            AMAT_TERM = AMAT(ISYM(II))            
            AMAT(ISYM(II)) = AMAT_TERM * RHOTERM
            AMAT(IA(JJ)) = AMAT(IA(JJ)) + AMAT_TERM * (1.0 - RHOTERM)
          ELSE  
C11B -------FOR IMPLICIT UPDATE NEED TO DESTROY MATRIX SYMMETRY
            GRHO = RHONORM(N)-RHONORM(JJ)
            AMAT_TERM = AMAT(II) 
            AMAT(II) = AMAT_TERM * (RHOTERM - OMEGA * GRHO)
            AMAT(IA(N)) = AMAT(IA(N)) + 
     1         AMAT_TERM * (1.0 - RHOTERM - GRHO*(1.0-OMEGA))
C            
            GRHO = -GRHO
            AMAT_TERM = AMAT(ISYM(II))            
            AMAT(ISYM(II)) = AMAT_TERM * (RHOTERM - GRHO*(1.0-OMEGA))
            AMAT(IA(JJ)) = AMAT(IA(JJ)) + 
     1         AMAT_TERM * (1.0 - RHOTERM - GRHO*OMEGA) 
          ENDIF     
        ENDDO
C
      ENDDO
C----------------------------------------------------------------------
C12------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO RHS
      ISS=ISSFLG(KPER)
c      IF(ISS.NE.0) RETURN  ! need this term even if flow is steady-state
c      RETURN    !skipping small transient density term
C13------FILL TRANSIENT DENSITY TERM FOR EACH CELL      
      DO N=1,NEQS      
C
C14------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).EQ.0) CYCLE
C
C15------COMPUTE TERM AND PUT ON RHS
        IF(N.LE.NODES)THEN
          VOL = AREA(N) * (TOP(N) - BOT(N))*PRSITY(N)
        ELSE
          I=N-NODES  
          VOL = AREA(N) * ACLNNDS(I,4)   
        ENDIF
csp        RHO = VOL*PRSITY(N)*Sn(N)/RHONORM(N)*DRHODC
        RHO = VOL*Sn(N)/RHONORM(N)/RHOFRESH*DRHODC
        CNC = CONC(N,1)
        IF(CNC.LT.0.0) CNC = 0.0
        IF(CNC.GT.CSTD) CNC = CSTD
        CNCO = CONCO(N,1)
        IF(CNCO.LT.0.0) CNCO = 0.0
        IF(CNCO.GT.CSTD) CNCO = CSTD        
        RHO = RHO * (CNC - CNCO) / DELT
        RHS(N) = RHS(N) + RHO
      ENDDO
c16----return
      RETURN
      END
C---------------------------------------------------------------------------
      SUBROUTINE DDF1BD(KSTP,KPER)
C     ******************************************************************
C     COMPUTE FLOW BETWEEN ADJACENT CELLS DUE TO DENSITY TERM
C     FOR TRANSIENT SIMULATION ALSO ADD DENSITY STORAGE TERM      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IBOUND,NEQS,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,
     1                AMAT,FLOWJA,Sn,ISSFLG,AREA,TOP,BOT,BUFF,HNEW
      USE GWFBASMODULE,ONLY:MSUM,ICBCFL,VBVL,VBNM,DELT,PERTIM,TOTIM
      USE SMSMODULE, ONLY: AMATFL
      USE GWTBCTMODULE, ONLY: PRSITY,CONC,CONCO
      USE DDFMODULE, ONLY: ZEECELL,RHONORM,DRHODC,RHOFRESH,CSTD, 
     1  THICKCELL, ITHICKAV, IMPHDD                                     !  , AMATDD 
      USE GWFBCFMODULE,ONLY:IBCFCB
      USE CLN1MODULE, ONLY: ACLNNDS
C
       double precision,    DIMENSION(:),    ALLOCATABLE ::temp
      DOUBLE PRECISION QCON,ZEENJJ,RHO,VOL,STOIN,STOUT,SSTRG,STIN,SOUT,
     1  ZERO,HPHI,RHOTERM,CNC,CNCO,STRG,GRHO,AMAT_TERM
      CHARACTER*16 TEXT
      DATA TEXT /' DENSITY STORAGE'/
C     ------------------------------------------------------------------
c      allocate (temp(nja))
c      do i=1,nja
c          temp = 0.0
c      enddo    
C1-----initialize
      ZERO = 0.0
      STOIN=ZERO
      STOUT=ZERO
C2------FILL DENSITY GRADIENT TERM FOR EVERY CELL
      DO N=1,NEQS
C
C3------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).LE.0) CYCLE
C
C4------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(JJ.LT.N. OR. IBOUND(JJ).LE.0) CYCLE 
C5--------COMPUTE AVERAGE DENSITY, Z AND H OF THE TWO CELLS
          IF(ITHICKAV.EQ.0) THEN 
            ZEENJJ = (ZEECELL(N) + ZEECELL(JJ)) * 0.5
            HPHI = (HNEW(N) + HNEW(JJ)) * 0.5  
            RHOTERM = (RHONORM(N) + RHONORM(JJ)) * 0.5
          ELSE
            OMEGA = THICKCELL(N) / (THICKCELL(N) + THICKCELL(JJ))  
            ZEENJJ = (1.0 - OMEGA) * ZEECELL(N) + OMEGA * ZEECELL(JJ) 
            HPHI = (1.0 - OMEGA) * HNEW(N) + OMEGA * HNEW(JJ) 
            RHOTERM = OMEGA * RHONORM(N) + (1.0 - OMEGA) * RHONORM(JJ) 
          ENDIF  
C6 -------BACK CALCULATE ORIGINAL K VALUE FOR CELLS N AND JJ (amat has K * rho/rho_o)  
            AMAT_TERM = AMATFL(II) / RHOTERM
C 
C7--------CALCULATE FLOW DUE TO DENSITY TERM          
          QCON = AMAT_TERM * (HPHI - ZEENJJ) 
     *         * (RHONORM(N)-RHONORM(JJ))
C8--------SUBTRACT THIS FLOW TERM ON FLOWJA (MINUS ON LHS, PLUS ON RHS)         
c          temp(ii) = qcon
c          temp(isym(ii)) = -qcon
          FLOWJA(II) = FLOWJA(II) + QCON
          FLOWJA(ISYM(II)) = FLOWJA(ISYM(II)) - QCON 
        ENDDO
C
      ENDDO
c      open(222,file = 'qcon.dat')
c      write(222,222) (temp(ii), ii=1,nja)
c  222 format(10(1x,1p,g18.7))    
c      deallocate(temp)
c      close (222)
C----------------------------------------------------------------------
C9------IF THE SIMULATION IS TRANSIENT ADD STORAGE TO RHS
      ISS=ISSFLG(KPER)
c      IF(ISS.NE.0) GO TO 400   ! need this term even if flow is steady-state
c      GO TO 400  !skipping small transient density term
C
C10------IF CELL-BY-CELL FLOWS WILL BE SAVED, SET FLAG IBD.
      IBD=0
      IF(IBCFCB.GT.0) IBD=ICBCFL 
C
C11------CLEAR BUFFER.
      DO 210 N=1,NEQS
        BUFF(N)=ZERO
210   CONTINUE
C12------FILL TRANSIENT DENSITY TERM FOR EACH CELL      
      DO N=1,NEQS      
C
C13------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).EQ.0) CYCLE
C
C14------COMPUTE TERM AND PUT ON RHS
        IF(N.LE.NODES)THEN
          VOL = AREA(N) * (TOP(N) - BOT(N))*PRSITY(N)
        ELSE
          I=N-NODES  
          VOL = AREA(N) * ACLNNDS(I,4)   
        ENDIF
csp        RHO = VOL*PRSITY(N)*Sn(N)/RHONORM(N)*DRHODC
        RHO = VOL*Sn(N)/RHONORM(N)/RHOFRESH*DRHODC
        CNC = CONC(N,1)
        IF(CNC.LT.0.0) CNC = 0.0
        IF(CNC.GT.CSTD) CNC = CSTD
        CNCO = CONCO(N,1)
        IF(CNCO.LT.0.0) CNCO = 0.0
        IF(CNCO.GT.CSTD) CNCO = CSTD        
        RHO = RHO * (CNC - CNCO) / DELT
        STRG = -RHO
C
C15-----STORE CELL-BY-CELL FLOW IN BUFFER AND ADD TO ACCUMULATORS.
        BUFF(N)=STRG
        FLOWJA(IA(N)) = FLOWJA(IA(N)) - STRG
        SSTRG=STRG
        IF(STRG.LT.ZERO) THEN
          STOUT=STOUT-SSTRG
        ELSE
          STOIN=STOIN+SSTRG
        END IF
      ENDDO  
C16------record contents of buffer for structured and unstructured grids
      IF(IUNSTR.EQ.0)THEN
C
C16A-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
        IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,
     1                       IBCFCB,BUFF,NCOL,NROW,NLAY,IOUT)
        IF(IBD.EQ.2) CALL UBDSV1(KSTP,KPER,TEXT,IBCFCB,
     1            BUFF,NCOL,NROW,NLAY,IOUT,DELT,PERTIM,TOTIM,IBOUND)
      ELSE
C
C16B-----IF IBD FLAG IS SET RECORD THE CONTENTS OF THE BUFFER.
        IF(IBD.EQ.1) CALL UBUDSVU(KSTP,KPER,TEXT,IBCFCB,BUFF(1),NODES,
     1         IOUT,PERTIM,TOTIM)
        IF(IBD.EQ.2) CALL UBDSV1U(KSTP,KPER,TEXT,IBCFCB,BUFF(1),NODES,
     1     IOUT,DELT,PERTIM,TOTIM,IBOUND,NODES)
      ENDIF
C
C17-----ADD TOTAL RATES AND VOLUMES TO VBVL & PUT TITLE IN VBNM.
  400 CONTINUE
      STIN=STOIN
      SOUT=STOUT
      VBVL(1,MSUM)=VBVL(1,MSUM)+STIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+SOUT*DELT
      VBVL(3,MSUM)=STIN
      VBVL(4,MSUM)=SOUT
      VBNM(MSUM)=TEXT
      MSUM=MSUM+1
c18----return
      RETURN
      END
C-----------------------------------------------------------------
      SUBROUTINE DDF1DA
C  Deallocate DDF data 
      USE DDFMODULE
C
        DEALLOCATE(IDDF)
        DEALLOCATE(RHOFRESH,RHOSTD,CSTD,DRHODC)
        DEALLOCATE(RHONORM,ZEECELL)
C
      RETURN
      END      
C---------------------------------------------------------------------------
      SUBROUTINE SDDF1BDADJ
C     ******************************************************************
C     RESET MATRIX TO REMOVE IMPLICIT DENSITY TERM EFFECT
C     FLOW TERM OF MATRIX IS KEPT AS K * (RHO/RHO_o) FOR BCF BUDGET ROUTINES      
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      USE GLOBAL, ONLY:IBOUND,NEQS,IOUT,NODES,NJA,IA,JA,JAS,IUNSTR,ISYM,
     1                AMAT,Sn,AREA,TOP,BOT,BUFF,HNEW
      USE SMSMODULE, ONLY: AMATFL
      USE DDFMODULE, ONLY: ZEECELL,RHONORM,DRHODC,RHOFRESH,CSTD, 
     1  THICKCELL, ITHICKAV, IMPHDD                                     !  , AMATDD 
      USE GWFBCFMODULE,ONLY:IBCFCB
      USE CLN1MODULE, ONLY: ACLNNDS
C
      DOUBLE PRECISION QCON,ZEENJJ,OMEGA,HPHI,RHOTERM,GRHO,AMAT_TERM
C     ------------------------------------------------------------------
C1------FILL DENSITY GRADIENT TERM FOR EVERY CELL
      DO N=1,NEQS
C
C2------IF CELL IS NOT ACTIVE GO ON TO NEXT CELL.
        IF (IBOUND(N).LE.0) CYCLE
C
C3------CALCULATE FOR ALL CONNECTING FACES.
        DO II = IA(N)+1,IA(N+1)-1
          JJ = JA(II)
          IIS = JAS(II)
          IF(JJ.LT.N. OR. IBOUND(JJ).LE.0) CYCLE 
C4--------COMPUTE AVERAGE DENSITY, Z AND H OF THE TWO CELLS
          IF(ITHICKAV.EQ.0) THEN 
            ZEENJJ = (ZEECELL(N) + ZEECELL(JJ)) * 0.5
            HPHI = (HNEW(N) + HNEW(JJ)) * 0.5  
C            HPHI = (HOLD(N) + HOLD(JJ)) * 0.5  !TIME LAG THIS TERM FOR STABILITY? USE HOLD? 
            RHOTERM = (RHONORM(N) + RHONORM(JJ)) * 0.5
          ELSE
            OMEGA = THICKCELL(N) / (THICKCELL(N) + THICKCELL(JJ))  
            ZEENJJ = (1.0 - OMEGA) * ZEECELL(N) + OMEGA * ZEECELL(JJ) 
            HPHI = (1.0 - OMEGA) * HNEW(N) + OMEGA * HNEW(JJ) 
C            HPHI = (1.0 - OMEGA) * HOLD(N) + OMEGA * HOLD(JJ)  !TIME LAG THIS TERM FOR STABILITY? USE HOLD?  
            RHOTERM = OMEGA * RHONORM(N) + (1.0 - OMEGA) * RHONORM(JJ) 
          ENDIF  
C5 -------BACK CALCULATE ORIGINAL K VALUE FOR CELLS N AND JJ INTO A CONSTANT VARIABLE 
          IF(IMPHDD.EQ.0) THEN 
            AMAT_TERM = AMATFL(II) / RHOTERM
          ELSE
            GRHO = RHONORM(N)-RHONORM(JJ)  
            AMAT_TERM = AMATFL(II) / (RHOTERM - OMEGA * GRHO) 
          ENDIF                    
C          
C6 --------ADJUST MATRIX FOR DENSITY TERM DEPENDING ON IMPLICIT OR EXPLICIT         
          IF(IMPHDD. EQ. 0) THEN 
C7----------NOTHING TO UPDATE FOR EXPLICIT SINCE FLOW COEFFICIENT IS 
C----------K * (RHO/RHO_o) AND LEAVE THAT FOR BCF BUDGET PACKAGE TO SOLVE
          ELSE 
C8----------BACK CALCULATE K * (RHO/RHO_o) FOR MATRIX TERMS FOR IMPLICIT DENSITY TERM
            GRHO = RHONORM(N)-RHONORM(JJ)
            AMATFL(II) = AMAT_TERM * RHOTERM 
            AMATFL(IA(N)) = AMATFL(IA(N)) + 
     1         AMAT_TERM * GRHO*(1.0-OMEGA) 
C            
            AMATFL(ISYM(II)) = AMAT_TERM * RHOTERM 
            AMATFL(IA(JJ)) = AMATFL(IA(JJ)) + 
     1         AMAT_TERM * GRHO * OMEGA 
          ENDIF               
        ENDDO
C
      ENDDO
C---------------------------------------------------------------------------
C9----RETURN
      RETURN
      END
