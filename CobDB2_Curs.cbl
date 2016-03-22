   File  Edit  Edit_Settings  Menu  Utilities  Compilers  Test  Help
 -------------------------------------------------------------------------------
 EDIT       ADS02.ADS0205.SRC(ADSD225) - 01.21              Columns 00001 00072
 ****** ***************************** Top of Data ******************************
 000001       *===============================================================*
 000002       *--                INFORMATIONS GENERALES                     --*
 000003       *---------------------------------------------------------------*
 000004       *  NOM DU PROGRAMME : XXXXXXXX                                  *
 000005       *  NOM DU REDACTEUR : MARTINLUC                                 *
 000006       *---------------------------------------------------------------*
 000007       *  SOCIETE          : XXXXXXXX                                  *
 000008       *  DATE DE CREATION : JJ/MM/SSAA                                *
 000009       *---------------------------------------------------------------*
 000010       *--               OBJECTIFS GENERAUX DU PROGRAMME             --*
 000011       * RECHERCHE DES ARTICLE PRESENTS EN STOCK MAIS NON VENDUS       *
 000012       * POUR LES SOLDER (CURSEUR). AFFICHAGE DE LA LISTE.             *
 000013       *---------------------------------------------------------------*
 000014       *--               HISTORIQUE DES MODIFICATIONS --               *
 000015       *---------------------------------------------------------------*
 000016       * DATE  MODIF   !          NATURE DE LA MODIFICATION            *
 000017       *---------------------------------------------------------------*
 000018       * JJ/MM/SSAA    !                                               *
 000019       *               !                                               *
 000020       *===============================================================*
 000021       *
 000022       *************************
 000023        IDENTIFICATION DIVISION.
 000024       *************************
 000025        PROGRAM-ID.      ADSD225.
 000026       *
 000027       *                  ==============================               *
 000028       *=================<  ENVIRONMENT      DIVISION   >==============*
 000029       *                  ==============================               *
 000030       *                                                               *
 000031       *===============================================================*
 000032       *
 000033       **********************
 000034        ENVIRONMENT DIVISION.
 000035       **********************
 000036       *
 000037       *======================
 000038        CONFIGURATION SECTION.
 000039       *======================
 000040       *
 000041       *--------------
 000042        SPECIAL-NAMES.
 000043       *--------------
 000044            DECIMAL-POINT IS COMMA.
 000045       *
 000046       *=====================
 000047        INPUT-OUTPUT SECTION.
 000048       *=====================
 000049       *
 000050       *-------------
 000051        FILE-CONTROL.
 000052       *-------------
 000053       *
 000059       *
 000060       *                  ==============================               *
 000061       *=================<       DATA        DIVISION   >==============*
 000062       *                  ==============================               *
 000063       *                                                               *
 000064       *===============================================================*
 000065       *
 000066       ***************
 000067        DATA DIVISION.
 000068       ***************
 000069       *
 000070       *=============
 000071        FILE SECTION.
 000072       *=============
 000073       *
 000074       *FD  XXXXXXXXXXXX
 000075       *    DATA RECORD IS XXXXXXXX.
 000076       *01  XXXXXXXXXXXX.
 000077       *
 000078       *========================
 000079        WORKING-STORAGE SECTION.
 000080       *========================
 000081       *77  WS-FS-XXXXXXX    PIC X(2).
 000082       *
 000083            EXEC SQL
 000084               BEGIN DECLARE SECTION
 000085            END-EXEC.
 000086
 000087            EXEC SQL
 000088               INCLUDE TARTICL
 000089            END-EXEC.
 000090
 000091            EXEC SQL
 000092               INCLUDE TLIGCOM
 000093            END-EXEC.
 000094
 000095            EXEC SQL
 000096               INCLUDE SQLCA
 000097            END-EXEC.
 000098
 000099            EXEC SQL
 000100               DECLARE CURS1
 000101               CURSOR FOR
 000102               SELECT TARTICL.CDNUMART, LBNOMART, PXVTEART, QTSTKART
 000103                 FROM TARTICL
 000104                WHERE NOT EXISTS (SELECT TLIGCOM.CDNUMART
 000105                                    FROM TLIGCOM
 000106                              WHERE TARTICL.CDNUMART = TLIGCOM.CDNUMART)
 000107            END-EXEC.
 000108
 000109            EXEC SQL
 000110                 END DECLARE SECTION
 000111            END-EXEC.
 000112
 000113        01  WS-LIG1.
 000114            05  FILLER      PIC X      VALUE '*'.
 000115            05  FILLER      PIC X(63)  VALUE ALL '-'.
 000116            05  FILLER      PIC X      VALUE '*'.
 000117
 000118        01  WS-LIG2.
 000119            05  FILLER      PIC XX     VALUE '| '.
 000120            05  FILLER      PIC X(62)  VALUE 'ARTICLES A SOLDER'.
 000121            05  FILLER      PIC X      VALUE '|'.
 000122
 000123        01  WS-LIG3.
 000124            05  FILLER      PIC X      VALUE '*'.
 000125            05  FILLER      PIC X(8)   VALUE ALL '-'.
 000126            05  FILLER      PIC X      VALUE '*'.
 000127            05  FILLER      PIC X(35)  VALUE ALL '-'.
 000128            05  FILLER      PIC X      VALUE '*'.
 000129            05  FILLER      PIC X(11)  VALUE ALL '-'.
 000130            05  FILLER      PIC X      VALUE '*'.
 000131            05  FILLER      PIC X(06)  VALUE ALL '-'.
 000132            05  FILLER      PIC X      VALUE '*'.
 000133
 000134        01  WS-LIG4.
 000135            05  FILLER      PIC X      VALUE '|'.
 000136            05  FILLER      PIC X(8)   VALUE 'CODE'.
 000137            05  FILLER      PIC X      VALUE '|'.
 000138            05  FILLER      PIC X(35)  VALUE 'LIBELLE ARTICLE'.
 000139            05  FILLER      PIC X      VALUE '|'.
 000140            05  FILLER      PIC X(11)  VALUE 'PRIX'.
 000141            05  FILLER      PIC X      VALUE '|'.
 000142            05  FILLER      PIC X(06)  VALUE 'QTE'.
 000143            05  FILLER      PIC X      VALUE '|'.
 000144
 000145        01  WS-LIG5.
 000146            05  FILLER      PIC X      VALUE '|'.
 000147            05  WS-CODE-ED  PIC X(8).
 000148            05  FILLER      PIC X      VALUE '|'.
 000149            05  WS-LIB-ED   PIC X(35).
 000150            05  FILLER      PIC X      VALUE '|'.
 000151            05  WS-PRIX-ED  PIC ZZZZZZZ9,99.
 000152            05  FILLER      PIC X      VALUE '|'.
 000153            05  WS-QTE-ED   PIC ZZZZZ9.
 000154            05  FILLER      PIC X      VALUE '|'.
 000155       *
 000156       *
 000157       *                  ==============================               *
 000158       *=================<   PROCEDURE       DIVISION   >==============*
 000159       *                  ==============================               *
 000160       *                                                               *
 000161       *===============================================================*
 000162       *
 000163        PROCEDURE           DIVISION.
 000164       *
 000181       *===============================================================*
 000182       *
 000183       *
 000184       *---------------------------------------------------------------*
 000185       *               DESCRIPTION DU COMPOSANT PROGRAMME              *
 000186       *               ==================================              *
 000187       *---------------------------------------------------------------*
 000188       *
 000189        0000-PROGRAMME-DEB.
 000190       *
 000191       *
 000192            PERFORM 8000-ENTETE-DEB
 000193               THRU 8000-ENTETE-FIN.
 000194
 000195            EXEC SQL
 000196                 OPEN CURS1
 000197            END-EXEC.
 000198
 000199            PERFORM 1000-SQL-DEB
 000200               THRU 1000-SQL-FIN
 000201              UNTIL SQLCODE = 100.
 000202
 000203
 000204            EXEC SQL
 000205                 CLOSE CURS1
 000206            END-EXEC.
 000207
 000208            DISPLAY WS-LIG1.
 000209
 000210            GOBACK.
 000211
 000212        0000-PROGRAMME-FIN.
 000213             EXIT.
 000214       *
 000215        1000-SQL-DEB.
 000216
 000217            EXEC SQL
 000218                 FETCH CURS1
 000219                 INTO :DCLTARTICL.CDNUMART,
 000220                      :DCLTARTICL.LBNOMART,
 000221                      :DCLTARTICL.PXVTEART,
 000222                      :DCLTARTICL.QTSTKART
 000223
 000224            END-EXEC.
 000225
 000226            IF SQLCODE = 100
 000227               CONTINUE
 000228            ELSE
 000229               EVALUATE TRUE
 000230               WHEN SQLCODE = 0 PERFORM 8010-CORPS-DEB
 000231                                   THRU 8010-CORPS-FIN
 000232               WHEN SQLCODE < 0 PERFORM 8020-ANOMALIE-DEB
 000233                                   THRU 8020-ANOMALIE-FIN
 000234               WHEN SQLCODE > 0 PERFORM 8030-WARNING-DEB
 000235                                   THRU 8030-WARNING-FIN
 000236               END-EVALUATE
 000237            END-IF.
 000238
 000239        1000-SQL-FIN.
 000240            EXIT.
 000241
 000242
 000243       *===============================================================*
 000244       *===============================================================*
 000245       *    STRUCTURATION DE LA PARTIE INDEPENDANTE DU PROGRAMME       *
 000246       *---------------------------------------------------------------*
 000247       *                                                               *
 000248       *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
 000249       *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
 000250       *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
 000251       *   9XXX-  : ORDRES DE MANIPULATION DES SOUS-PROGRAMMES         *
 000252       *   9999-  : PROTECTION FIN DE PROGRAMME                        *
 000253       *                                                               *
 000254       *===============================================================*
 000255       *===============================================================*
 000256       *
 000257       *---------------------------------------------------------------*
 000258       *   6XXX-  : ORDRES DE MANIPULATION DES FICHIERS                *
 000259       *---------------------------------------------------------------*
 000260       *                                                               *
 000261       *---------------------------------------------------------------*
 000262       *   7XXX-  : TRANSFERTS ET CALCULS COMPLEXES                    *
 000263       *---------------------------------------------------------------*
 000264       *
 000265       *7000-ORDRE-CALCUL-DEB.
 000266       *
 000267       *7000-ORDRE-CALCUL-FIN.
 000268       *    EXIT.
 000269       *
 000270       *---------------------------------------------------------------*
 000271       *   8XXX-  : ORDRES DE MANIPULATION DES EDITIONS                *
 000272       *---------------------------------------------------------------*
 000273       *
 000274        8000-ENTETE-DEB.
 000275
 000276             DISPLAY WS-LIG1.
 000277             DISPLAY WS-LIG2.
 000278             DISPLAY WS-LIG3.
 000279             DISPLAY WS-LIG4.
 000280             DISPLAY WS-LIG3.
 000281
 000282        8000-ENTETE-FIN.
 000283            EXIT.
 000284
 000285
 000286        8010-CORPS-DEB.
 000287
 000288
 000289             MOVE CDNUMART OF DCLTARTICL TO WS-CODE-ED.
 000290             MOVE LBNOMART OF DCLTARTICL TO WS-LIB-ED.
 000291             MOVE PXVTEART OF DCLTARTICL TO WS-PRIX-ED.
 000292             MOVE QTSTKART OF DCLTARTICL TO WS-QTE-ED.
 000293             DISPLAY WS-LIG5.
 000294
 000295        8010-CORPS-FIN.
 000296            EXIT.
 000297
 000298        8020-ANOMALIE-DEB.
 000299
 000300             DISPLAY 'PROBLEME DE CODE SQL : ' SQLCODE.
 000301             STOP RUN.
 000302
 000303        8020-ANOMALIE-FIN.
 000304            EXIT.
 000305
 000306        8030-WARNING-DEB.
 000307
 000308             DISPLAY 'ATTENTION AU CODE SQL : ' SQLCODE.
 000309
 000310        8030-WARNING-FIN.
 000311            EXIT.
 000312       *
 ****** **************************** Bottom of Data ****************************
























 Command ===>                                                  Scroll ===> CSR
  F1=Help      F2=Split     F3=Exit      F5=Rfind     F6=Rchange   F7=Up
  F8=Down      F9=Swap     F10=Left     F11=Right    F12=Cancel
================================================================================
   File  Edit  Edit_Settings  Menu  Utilities  Compilers  Test  Help
 