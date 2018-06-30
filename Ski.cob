        IDENTIFICATION DIVISION.
        PROGRAM-ID. Ski.

        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.

               SELECT fprof ASSIGN TO "fprof.dat"
               ORGANIZATION indexed
               ACCESS IS dynamic
               RECORD KEY IS fp_num
               ALTERNATE RECORD KEY IS fp_qualif WITH DUPLICATES
               FILE STATUS IS fp_stat.

               SELECT finscription ASSIGN TO "finscription.dat"
               ORGANIZATION indexed
               ACCESS IS dynamic
               RECORD KEY IS fi_cleins
               ALTERNATE RECORD KEY IS fi_numE WITH DUPLICATES
               ALTERNATE RECORD KEY IS fi_numC WITH DUPLICATES
               FILE STATUS IS fi_stat.

               SELECT fcours ASSIGN TO "cours.dat"
               ORGANIZATION indexed
               ACCESS IS dynamic
               FILE STATUS IS fc_stat
               RECORD KEY IS fc_num
               ALTERNATE RECORD KEY IS fc_activite WITH DUPLICATES.

               SELECT farchives ASSIGN TO "archives.dat"
               ORGANIZATION SEQUENTIAL
               ACCESS IS SEQUENTIAL
               FILE STATUS IS fa_stat.

               SELECT feleve ASSIGN TO "eleve.dat"
               ORGANIZATION IS INDEXED
               ACCESS IS dynamic
               RECORD KEY IS fe_num 
               ALTERNATE RECORD KEY IS fe_nom WITH DUPLICATES
               FILE STATUS IS fe_stat.
                                 
        DATA DIVISION.
        FILE SECTION.
               FD fprof.
                01 profTampon.
                  02 fp_num PIC 9(6).
                  02 fp_nom PIC X(30).
                  02 fp_prenom PIC X(30).
                  02 fp_qualif PIC X(30).
                  02 fp_tel PIC X(10).
                
                FD feleve.
                01 elTampon.
                  02 fe_num PIC 9(4).
                  02 fe_nom PIC A(20).
                  02 fe_prenom PIC A(20).
                  02 fe_tel PIC X(10).

               FD finscription.
                01 insTampon.
                  02 fi_cleins PIC X(13).         
                  02 fi_numC PIC 9(6).
                  02 fi_numE PIC 9(6).

                FD fcours.
                01 cTampon.
                  02 fc_num PIC 9(4).
                  02 fc_activite PIC X(30).
                  02 fc_horaire PIC X(30).
                  02 fc_numP PIC 9(4).
                  02 fc_type PIC X(30).
                  02 fc_jour PIC 9(2).
                  02 fc_mois PIC 9(2).
                  02 fc_capacite PIC 9(4).

                FD farchives.
                01 aTampon.
                  02 fa_num PIC X(30).
                  02 fa_activite PIC X(30).
                  02 fa_horaire PIC X(30).
                  02 fa_numP PIC 9(4).
                  02 fa_type PIC X(30).
                  02 fa_jour PIC 9(2).
                  02 fa_mois PIC 9(2).
                  02 fa_capacite PIC 9(4).

        WORKING-STORAGE SECTION.

                77 fp_stat PIC 9(2).
                77 fi_stat PIC 9(2).
                77 fc_stat PIC 9(2).
                77 fa_stat PIC 9(2).
                77 fe_stat PIC 9(2).
                77 Wrep PIC 9.
                77 Wfin PIC 9.
                77 Wchoix PIC 9(2).
                77 Wnum PIC 9(6).
                77 Wnum2 PIC 9(6).
                77 Wtrouve PIC 9.
                77 Wnom PIC A(20).
                77 Wprenom PIC A(20).
                77 Wjour PIC 9(2).
                77 Wmois PIC 9(2).
                77 Wannee PIC 9(4).
                77 WnumArchive PIC X(30).
                77 Wqualif PIC 9.
                77 Wactiv PIC 9.
                77 Wtype PIC 9.
                77 Whor PIC 9.
                77 connexion PIC 9.
                77 identifiant PIC X(30).
                77 mdp PIC X(30).
                77 Wfin2 PIC 9.
                77 WtotalIns PIC 9(6).
                77 WtotalCours PIC 9(6).
                77 WmoyenneCours PIC 9(2)V9(3).
                77 WtotalInsSnow PIC 9(6).
                77 WtotalCoursSnow PIC 9(6).
                77 WmoyenneCoursSnow PIC 9(2)V9(3).
                77 WtotalInsSkiA PIC 9(6).
                77 WtotalCoursSkiA PIC 9(6).
                77 WmoyenneCoursSkiA PIC 9(2)V9(3).
                77 WtotalInsSkiF PIC 9(6).
                77 WtotalCoursSkiF PIC 9(6).
                77 WmoyenneCoursSkiF PIC 9(2)V9(3).
                77 Wcdc PIC X(11).


        PROCEDURE DIVISION.

                OPEN I-O fprof
                IF fp_stat =35 THEN
                  OPEN OUTPUT fprof
                CLOSE fprof
                END-IF
                CLOSE fprof
                
                OPEN I-O finscription
                IF fi_stat =35 THEN
                  OPEN OUTPUT finscription
                CLOSE finscription
                END-IF
                CLOSE finscription

                OPEN I-O fcours
                IF fc_stat =35 THEN
                  OPEN OUTPUT fcours
                END-IF
                CLOSE fcours

                OPEN EXTEND farchives
                IF fa_stat =35 THEN
                  OPEN OUTPUT farchives
                END-IF
                CLOSE farchives

                 OPEN I-O feleve
                IF fe_stat = 35 THEN
                  OPEN OUTPUT feleve
                END-IF
                CLOSE feleve               



              PERFORM WITH TEST AFTER UNTIL connexion=1 OR connexion=2
                DISPLAY '----------------- CLIENT ---------------------'
                DISPLAY '1 : Connexion administration'
                DISPLAY '2 : Connexion professeur'
                DISPLAY '----------------------------------------------'
                ACCEPT connexion
              END-PERFORM
               
              IF connexion = 1 THEN

              DISPLAY '---------------- CONNEXION -------------------'
              DISPLAY 'Identifiant:'
              ACCEPT identifiant
              DISPLAY 'Mot de passe:'
              ACCEPT mdp
              DISPLAY '-----------------------------------------------'
              IF identifiant = 'admin' AND mdp = 'admin' THEN
                PERFORM WITH TEST AFTER UNTIL Wchoix < 1 OR Wchoix > 18
                DISPLAY '---------------- ACCUEIL -------------------' 
                DISPLAY 'Que souhaitez-vous faire?'
                DISPLAY ' ' 
                DISPLAY ' Partie Professeur'         
                DISPLAY '1 - Ajouter un professeur'
                DISPLAY '2 - Modifier un professeur existant'
                DISPLAY '4 - Consulter la liste des professeurs'
                DISPLAY '5 - Supprimer un professeur'
                DISPLAY ' '
                DISPLAY ' Partie Cours'    
                DISPLAY '6 - Inscrire un élève'
                DISPLAY '7 - Ajouter un cours'
                DISPLAY '8 - Modifier un cours'
                DISPLAY '9 - Supprimer un cours'
                DISPLAY '10 - Consulter les cours'
                DISPLAY '11 - Attribuer un professeur'
                DISPLAY '12 - Archiver'
                DISPLAY ' '
                DISPLAY ' Partie Eleve'    
                DISPLAY '13 - Ajouter un élève'
                DISPLAY '14 - Modifier un élève'
                DISPLAY '15 - Lister les éléves'
                DISPLAY '16 - Lister les inscriptions'
                DISPLAY ' '
                DISPLAY ' Abonnements' 
                DISPLAY '17 - Abonnement Découverte'
                DISPLAY '18 - Abonnement Progression'
                DISPLAY '19 - Abonnement Maîtrise'
                DISPLAY ' '
                DISPLAY ' Statistiques' 
                DISPLAY '20 - Fréquentation par date'
                DISPLAY '21 - Fréquentation par activité'
                DISPLAY '0 - Quitter'
                DISPLAY '---------------------------------------------'
                ACCEPT Wchoix
                   EVALUATE Wchoix
                      WHEN 1 PERFORM AJOUT_PROF
                      WHEN 2 PERFORM MODIFIER_PROF
                      WHEN 4 PERFORM LISTE_PROF
                      WHEN 5 PERFORM SUPPRIMER_PROF
                      WHEN 6 PERFORM INSCRIRE_ELEVE
                      WHEN 7 PERFORM AJOUT_COURS
                      WHEN 8 PERFORM MODIF_COURS
                      WHEN 9 PERFORM SUPPRIMER_COURS
                      WHEN 10 PERFORM LISTE_COURS
                      WHEN 11 PERFORM ATTRIBUER_PROF
                      WHEN 12 PERFORM ARCHIVER
                      WHEN 13 PERFORM AJOUT_ELEVE
                      WHEN 14 PERFORM MODIFIER_ELEVE
                      WHEN 15 PERFORM LISTE_ELEVE
                      WHEN 16 PERFORM LISTE_INSCRIPTION
                      WHEN 17 PERFORM ABONNEMENT_DEC
                      WHEN 18 PERFORM ABONNEMENT_PROG
                      WHEN 19 PERFORM ABONNEMENT_MAIT
                      WHEN 20 PERFORM FREQ_PAR_DATE
                      WHEN 21 PERFORM FREQ_PAR_ACT
                   END-EVALUATE
                END-PERFORM
              ELSE
                DISPLAY 'Utilisateur ou mot de passe incorrect'
                DISPLAY '---------------------------------------------'
              END-IF
              STOP RUN.

              IF connexion = 2 THEN

              DISPLAY '---------------- CONNEXION -------------------'
              DISPLAY 'Identifiant:'
              ACCEPT identifiant
              DISPLAY 'Mot de passe:'
              ACCEPT mdp
              DISPLAY '-----------------------------------------------'
              IF identifiant = 'prof' AND mdp = 'prof' THEN
                PERFORM WITH TEST AFTER UNTIL Wchoix < 1
                DISPLAY '---------------- ACCUEIL -------------------' 
                DISPLAY 'Que souhaitez-vous faire?'
                DISPLAY ' '
                DISPLAY '1 - Consulter emploi du temps'          
                DISPLAY '0 - Quitter'
                DISPLAY '---------------------------------------------'
                ACCEPT Wchoix
                   EVALUATE Wchoix
                      WHEN 1 PERFORM CONSULTER_EDT
                   END-EVALUATE
                END-PERFORM
              ELSE
                DISPLAY 'Utilisateur ou mot de passe incorrect'
                DISPLAY '---------------------------------------------'
              END-IF
              STOP RUN.




        AJOUT_PROF.
        OPEN I-O fprof
        MOVE 0 TO Wrep
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY 'Entrez l identifiant du professeur'
        ACCEPT fp_num
        START fprof, KEY IS = fp_num
        INVALID KEY
               PERFORM WITH TEST AFTER UNTIL fp_nom IS ALPHABETIC
               DISPLAY 'Entrez le nom du professeur'
               ACCEPT fp_nom
               END-PERFORM
               PERFORM WITH TEST AFTER UNTIL fp_prenom IS ALPHABETIC
               DISPLAY 'Entrez le prenom du professeur'
               ACCEPT fp_prenom
               END-PERFORM
               MOVE 0 TO Wqualif
               PERFORM WITH TEST AFTER UNTIL Wqualif>0 AND Wqualif<4
               DISPLAY 'Entrez la ou les qualifications du professeur'
               DISPLAY '1 - Ski alpin'
               DISPLAY '2 - Ski de fond'
               DISPLAY '3 - Snowboard'              
               ACCEPT Wqualif
               IF Wqualif = 1 THEN
                        STRING 'Ski alpin  ' INTO fp_qualif
               ELSE IF Wqualif = 2 THEN
                        STRING 'Ski de fond' INTO fp_qualif
               ELSE IF Wqualif = 3 THEN
                        STRING 'Snowboard  ' INTO fp_qualif
               END-IF
               END-PERFORM
               PERFORM WITH TEST AFTER UNTIL fp_tel IS NUMERIC
               DISPLAY 'Entrez le numero de téléphone du professeur'
               ACCEPT fp_tel
               END-PERFORM
        WRITE profTampon END-WRITE
        DISPLAY 'Le professeur a été ajouté'
        NOT INVALID KEY
                DISPLAY 'Ce professeur existe déjà'
        END-START
        PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Ajouter un autre professeur? 1- Oui ou 0- Non'
             ACCEPT Wrep
          END-PERFORM
        END-PERFORM
        CLOSE fprof.

        MODIFIER_PROF.
        MOVE 0 TO Wrep
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY 'Entrez identifiant du professeur à modifier'
        ACCEPT Wnum
        OPEN I-O fprof
        MOVE Wnum TO fp_num
        START fprof, KEY IS = fp_num

        INVALID KEY 
        DISPLAY 'Cet identifiant n est associé à aucun professeur'
        NOT INVALID KEY
                DISPLAY 'Entrez les nouvelles informations'
                PERFORM WITH TEST AFTER UNTIL fp_nom IS ALPHABETIC
                DISPLAY 'Nom :'
                ACCEPT fp_nom
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fp_nom IS ALPHABETIC
                DISPLAY 'Prenom :'
                ACCEPT fp_prenom
                END-PERFORM
                MOVE 0 TO Wqualif
                PERFORM WITH TEST AFTER UNTIL Wqualif>0 AND Wqualif<4
                DISPLAY 'Qualifications :'
                DISPLAY '2 - Ski alpin'
                DISPLAY '2 - Ski de fond'
                DISPLAY '3 - Snowvoard'              
                ACCEPT Wqualif
                IF Wqualif = 1 THEN
                        STRING 'Ski alpin  ' INTO fp_qualif
                ELSE IF Wqualif = 2 THEN
                        STRING 'Ski de fond' INTO fp_qualif
                ELSE IF Wqualif = 3 THEN
                        STRING 'Snowboard  ' INTO fp_qualif
                END-IF
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fp_tel IS NUMERIC
                DISPLAY 'Téléphone :'
                ACCEPT fp_tel
                END-PERFORM
        REWRITE profTampon END-REWRITE
        DISPLAY 'Le professeur a été modifié'
        END-START
        PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Modifier un autre professeur? 1- Oui ou 0- Non'
             ACCEPT Wrep
          END-PERFORM
        END-PERFORM
        CLOSE fprof.

        SUPPRIMER_PROF.
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
        DISPLAY 'Entrez identifiant du professeur à supprimer'
        ACCEPT Wnum
        OPEN I-O fprof
        MOVE Wnum TO fp_num
        READ fprof
        INVALID KEY 
                DISPLAY 'Ce professeur n existe pas'
        NOT INVALID KEY 
                DELETE fprof RECORD
                END-DELETE
                DISPLAY 'Le professeur a été supprimé'
        END-READ

        PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Supprimer un autre professeur? 1- Oui ou 0- Non'
             ACCEPT Wrep
          END-PERFORM
        END-PERFORM
        CLOSE fprof.

        LISTE_PROF.
        OPEN INPUT fprof
        MOVE 0 TO Wfin
        
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fprof NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                 DISPLAY '_______________________________'
                 DISPLAY 'Identifiant :', fp_num
                 DISPLAY 'Nom :', fp_nom
                 DISPLAY 'Prenom :', fp_prenom
                 DISPLAY 'Qualification :', fp_qualif
                 DISPLAY 'Téléphone :', fp_tel
                 DISPLAY '_______________________________'
           END-READ
        END-PERFORM
        CLOSE fprof.

        INSCRIRE_ELEVE.
        OPEN INPUT fcours
        DISPLAY 'Liste des cours:'
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fcours NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                 DISPLAY '_______________________________'
                 DISPLAY 'Identifiant :', fc_num
                 DISPLAY 'Activité :', fc_activite
                 DISPLAY 'Date :', fc_jour'/'fc_mois
                 DISPLAY 'Heure de début :', fc_horaire
                 DISPLAY 'Identifiant du professeur :', fc_numP
                 DISPLAY 'Type de cours :', fc_type
                 DISPLAY 'Capacité :', fc_capacite
                 DISPLAY '_______________________________'
           END-READ
        END-PERFORM
        CLOSE fcours
        DISPLAY 'Entrez identifiant de l élève à inscrire'
        ACCEPT Wnum
        OPEN I-O feleve
        MOVE Wnum TO fe_num
        START feleve, KEY IS = fe_num
        INVALID KEY
                DISPLAY 'Cet élève n existe pas'
        NOT INVALID KEY
                DISPLAY 'Entrez identifiant du cours souhaité'
                ACCEPT Wnum2
                OPEN I-O fcours
                MOVE Wnum2 TO fc_num
                START fcours, KEY IS = fc_num
                INVALID KEY
                        DISPLAY 'Ce cours n existe pas'
                NOT INVALID KEY
                        OPEN I-O finscription             
                         MOVE Wnum TO fi_numE
                         MOVE Wnum2 TO fi_numC
                         STRING Wnum'-'Wnum2 INTO fi_cleins
                WRITE insTampon END-WRITE
                END-START
                CLOSE finscription
                CLOSE fcours
        END-START
        CLOSE feleve
        CLOSE fprof.

        LISTE_INSCRIPTION.
        OPEN INPUT finscription
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ finscription NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                 DISPLAY '_______________________________'
                 DISPLAY 'Num Eleve :', fi_numE
                 DISPLAY 'Num Cours :', fi_numC
                 DISPLAY '_______________________________'
           END-READ
        END-PERFORM
        CLOSE finscription.


        AJOUT_COURS.
        OPEN I-O fcours
        MOVE 0 TO Wrep
        PERFORM WITH TEST AFTER UNTIL Wrep = 0
          DISPLAY 'Donnez les informations sur le cours'
          DISPLAY 'Identifiant'
          ACCEPT fc_num
          START fcours, KEY IS = fc_num
                INVALID KEY
                MOVE 0 TO Wactiv
                PERFORM WITH TEST AFTER UNTIL Wactiv > 0 AND Wactiv < 4
                  DISPLAY 'Activite'
                  DISPLAY '1 - Ski alpin'
                  DISPLAY '2 - Ski de fond'
                  DISPLAY '3 - Snowboard'
                  ACCEPT Wactiv
                  IF Wactiv = 1 THEN
                        STRING 'Ski alpin  ' INTO fc_activite
                  ELSE IF Wactiv = 2 THEN
                        STRING 'Ski de fond' INTO fc_activite
                  ELSE IF Wactiv = 3 THEN
                        STRING 'Snowboard  ' INTO fc_activite
                  END-IF
                END-PERFORM
                MOVE 0 TO Whor
                PERFORM WITH TEST AFTER UNTIL Whor > 0 AND Whor < 4
                  DISPLAY 'Horaires'
                  DISPLAY '1 - 10h-12h'
                  DISPLAY '2 - 12h-14h'
                  DISPLAY '3 - 14h-16h'
                  ACCEPT Whor
                  IF Whor = 1 THEN
                        STRING '10h-12h' INTO fc_horaire
                  ELSE IF Whor = 2 THEN
                        STRING '12h-14h' INTO fc_horaire
                  ELSE IF Whor = 3 THEN
                        STRING '14h-16h' INTO fc_horaire
                  END-IF
                END-PERFORM
                MOVE 0 TO Wtype
                PERFORM WITH TEST AFTER UNTIL Wtype > 0 AND Wtype < 3
                  DISPLAY 'Type :'
                  DISPLAY '1 - Particulier'
                  DISPLAY '2 - Collectif'
                  ACCEPT Wtype
                  IF Wtype = 1 THEN
                        STRING 'Particulier' INTO fc_type
                  ELSE IF Wtype = 2 THEN
                        STRING 'Collectif  ' INTO fc_type
                  END-IF
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fc_jour>0 AND fc_jour<32
                  DISPLAY 'Jour :'
                  ACCEPT fc_jour
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fc_mois>0 AND fc_mois<13
                  DISPLAY 'Mois :'
                  ACCEPT fc_mois
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fc_capacite>0 AND fc_capacite<51
                  DISPLAY 'Capacite :'
                  ACCEPT fc_capacite
                END-PERFORM
                  WRITE cTampon END-WRITE
                NOT INVALID KEY
                        DISPLAY 'Ce cours existe déjà'
                END-START
          PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY 'Souhaitez vous continuer ? 1(oui) ou 0(non)'
             ACCEPT Wrep
          END-PERFORM
        END-PERFORM 
        CLOSE fcours.

        MODIF_COURS.
        DISPLAY 'Entrez l identifiant du cours'
        ACCEPT Wnum
        OPEN I-O fcours
        MOVE Wnum TO fc_num
        START fcours, KEY IS = fc_num
        INVALID KEY 
                DISPLAY 'Ce cours n existe pas'
        NOT INVALID KEY
                MOVE 0 TO Wactiv
                PERFORM WITH TEST AFTER UNTIL Wactiv > 0 AND Wactiv < 4
                  DISPLAY 'Activite'
                  DISPLAY '1 - Ski alpin'
                  DISPLAY '2 - Ski de fond'
                  DISPLAY '3 - Snowboard'
                  ACCEPT Wactiv
                  IF Wactiv = 1 THEN
                        STRING 'Snowboard  ' INTO fc_activite
                  ELSE IF Wactiv = 2 THEN
                        STRING 'Ski de fond' INTO fc_activite
                  ELSE IF Wactiv = 3 THEN
                        STRING 'Ski alpin  ' INTO fc_activite
                  END-IF
                END-PERFORM
                MOVE 0 TO Whor
                PERFORM WITH TEST AFTER UNTIL Whor > 0 AND Whor < 4
                  DISPLAY 'Horaires'
                  DISPLAY '1 - 10h-12h'
                  DISPLAY '2 - 12h-14h'
                  DISPLAY '3 - 14h-16h'
                  ACCEPT Whor
                  IF Whor = 1 THEN
                        STRING '10h-12h' INTO fc_horaire
                  ELSE IF Whor = 2 THEN
                        STRING '12h-14h' INTO fc_horaire
                  ELSE IF Whor = 3 THEN
                        STRING '14h-16h' INTO fc_horaire
                  END-IF
                END-PERFORM
                MOVE 0 TO Wtype
                PERFORM WITH TEST AFTER UNTIL Wtype > 0 AND Wtype < 3
                  DISPLAY 'Type :'
                  DISPLAY '1 - Particulier'
                  DISPLAY '2 - Collectif  '
                  ACCEPT Wtype
                  IF Wtype = 1 THEN
                        STRING 'Particulier' INTO fc_type
                  ELSE IF Wtype = 2 THEN
                        STRING 'Collectif' INTO fc_type
                  END-IF
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fc_jour > 0 AND fc_jour<32
                  DISPLAY 'Jour :'
                  ACCEPT fc_jour
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fc_mois>0 AND fc_mois<13
                  DISPLAY 'Mois :'
                  ACCEPT fc_mois
                END-PERFORM
                PERFORM WITH TEST AFTER UNTIL fc_capacite>0 AND fc_capacite<51
                  DISPLAY 'Capacite :'
                  ACCEPT fc_capacite
                END-PERFORM
                  WRITE cTampon END-WRITE
         REWRITE cTampon END-REWRITE
         
         END-START
        CLOSE fcours.
        
        SUPPRIMER_COURS.
        DISPLAY 'Entrez l identifiant du cours'
        ACCEPT Wnum
        OPEN I-O fcours
                MOVE Wnum TO fc_num
                READ fcours
                INVALID KEY 
                        DISPLAY 'Ce cours n existe pas'
                NOT INVALID KEY 
                        DELETE fcours
                        DISPLAY 'Cours supprimé'
                END-READ
        CLOSE fcours.

        LISTE_COURS.
        OPEN INPUT fcours
        MOVE 0 TO Wfin
        DISPLAY 'Saisir un jour :'
        ACCEPT Wjour
        DISPLAY 'Saisir un mois :'
        ACCEPT Wmois
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fcours NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                IF Wjour = fc_jour AND Wmois = fc_mois THEN
                         DISPLAY '_______________________________'
                         DISPLAY 'Numero :', fc_num
                         DISPLAY 'Activite :', fc_activite
                         DISPLAY 'Horaire :', fc_horaire
                         DISPLAY 'Numero Professeur :', fc_numP
                         DISPLAY 'Type :', fc_type
                         DISPLAY 'Date :', fc_jour '/' fc_mois
                         DISPLAY 'Capacite :', fc_capacite
                         DISPLAY '_______________________________'
                END-IF
           END-READ
        END-PERFORM
        CLOSE fcours.

        ARCHIVER.
        DISPLAY 'Etes-vous sur de vouloir archiver ?'
        DISPLAY 'Tous les cours seront effacés. 1(oui) ou 0(non)'
        ACCEPT Wrep
        IF Wrep = 1 THEN
                OPEN I-O fCours
                OPEN EXTEND farchives
                DISPLAY 'Entrez l annee de l archive'
                ACCEPT Wannee
                MOVE 0 TO Wfin
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                   READ fcours NEXT
                   AT END MOVE 1 TO Wfin
                      DISPLAY 'Termine'
                   NOT AT END
                         STRING Wannee "_" fc_num INTO WnumArchive
                         MOVE WnumArchive TO fa_num
                         MOVE fc_activite TO fa_activite
                         MOVE fc_horaire TO fa_horaire
                         MOVE fc_numP TO fa_numP
                         MOVE fc_type TO fa_type
                         MOVE fc_jour TO fa_jour
                         MOVE fc_mois TO fa_mois
                         MOVE fc_capacite TO fa_capacite
                         WRITE aTampon END-WRITE
                         DELETE fcours
                   END-READ
                END-PERFORM
                CLOSE fcours
                CLOSE farchives
        END-IF.

       AJOUT_ELEVE.
       OPEN I-O feleve
       MOVE 0 TO Wrep
       PERFORM WITH TEST AFTER UNTIL Wrep = 0
       DISPLAY "Entrez l'identifiant de l'élève"
       ACCEPT fe_num
       START feleve, KEY IS = fe_num
       INVALID KEY 
               DISPLAY "Entrez le nom de l'élève"
               ACCEPT fe_nom
               DISPLAY "Entrez le prenom de l'élève"
               ACCEPT fe_prenom
               DISPLAY "Entrez le numero de téléphone de l'élève"
               ACCEPT fe_tel
       WRITE elTampon END-WRITE
       NOT INVALID KEY
                DISPLAY 'Cet eleve existe déjà'
       END-START
       PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                DISPLAY 'Souhaitez vous continuer ? 1(oui) ou 0(non)'
                ACCEPT Wrep
       END-PERFORM
       END-PERFORM
       CLOSE feleve.
      
        LISTE_ELEVE.
        OPEN INPUT feleve
        MOVE 0 TO Wfin
        
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ feleve NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                 DISPLAY 'Identifiant :', fe_num
                 DISPLAY 'Nom :', fe_nom
                 DISPLAY 'Prenom :', fe_prenom
                 DISPLAY 'Téléphone :', fe_tel
           END-READ
        END-PERFORM
        CLOSE feleve.
        
        MODIFIER_ELEVE.
        DISPLAY 'Entrez l identfiant de l élève'
        ACCEPT Wnum
        OPEN I-O feleve
        MOVE Wnum TO fe_num
        START feleve, KEY IS = fe_num
        INVALID KEY 
         DISPLAY 'Cet Identifiant n existe pas'
        NOT INVALID KEY
                DISPLAY 'Entrez les nouvelles informations'
                DISPLAY 'Nom :'
                ACCEPT fe_nom
                DISPLAY 'Prenom :'
                ACCEPT fe_prenom
                DISPLAY 'Téléphone :'
                ACCEPT fe_tel
         REWRITE elTampon END-REWRITE
         
         END-START
        CLOSE feleve.


        ATTRIBUER_PROF.
        OPEN INPUT fcours
        DISPLAY 'Liste des cours:'
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fcours NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                 DISPLAY '_______________________________'
                 DISPLAY 'Identifiant :', fc_num
                 DISPLAY 'Activité :', fc_activite
                 DISPLAY 'Date :', fc_jour'/'fc_mois
                 DISPLAY 'Heure de début :', fc_horaire
                 DISPLAY 'Identifiant du professeur :', fc_numP
                 DISPLAY 'Type de cours :', fc_type
                 DISPLAY 'Capacité :', fc_capacite
                 DISPLAY '_______________________________'
           END-READ
        END-PERFORM
        CLOSE fcours
        DISPLAY 'Entrez identifiant du prof à inscrire'
        ACCEPT Wnum
        OPEN I-O fprof
        MOVE Wnum TO fp_num
        START fprof, KEY IS = fp_num
        INVALID KEY
                DISPLAY 'Ce prof n existe pas'
        NOT INVALID KEY
                DISPLAY 'Entrez identifiant du cours souhaité'
                ACCEPT Wnum2
                OPEN I-O fcours
                MOVE Wnum2 TO fc_num
                START fcours, KEY IS = fc_num
                INVALID KEY
                        DISPLAY 'Ce cours n existe pas'
                NOT INVALID KEY
                          MOVE Wnum TO fc_numP
                REWRITE cTampon END-REWRITE
                END-START
                CLOSE fcours
        END-START
        CLOSE fprof.

        ABONNEMENT_DEC.
        MOVE 0 TO Wrep
        PERFORM WITH TEST AFTER UNTIL Wrep=2 
                PERFORM INSCRIRE_ELEVE
                COMPUTE Wrep = Wrep + 1
                DISPLAY '_______________________________'
                DISPLAY 'Inscription pour l heure numero ' Wrep ' terminée'
                DISPLAY '_______________________________'
        END-PERFORM
        DISPLAY '_______________________________'
        DISPLAY 'L ajout pour l abonnement Découverte est terminé'
        DISPLAY '_______________________________'.

        ABONNEMENT_PROG.
        MOVE 0 TO Wrep
        PERFORM WITH TEST AFTER UNTIL Wrep=4 
                PERFORM INSCRIRE_ELEVE
                COMPUTE Wrep = Wrep + 1
                DISPLAY '_______________________________'
                DISPLAY 'Inscription pour l heure numero ' Wrep ' terminée'
                DISPLAY '_______________________________'
        END-PERFORM
        DISPLAY '_______________________________'
        DISPLAY 'L ajout pour l abonnement Progession est terminé'
        DISPLAY '_______________________________'.

        ABONNEMENT_MAIT.
        MOVE 0 TO Wrep
        PERFORM WITH TEST AFTER UNTIL Wrep=6 
                PERFORM INSCRIRE_ELEVE
                COMPUTE Wrep = Wrep + 1
                DISPLAY '_______________________________'
                DISPLAY 'Inscription pour l heure numero ' Wrep ' terminée'
                DISPLAY '_______________________________'
        END-PERFORM
        DISPLAY '_______________________________'
        DISPLAY 'L ajout pour l abonnement Maîtrise est terminé'
        DISPLAY '_______________________________'.
        

        FREQ_PAR_DATE.
        OPEN INPUT fcours
        DISPLAY 'Entrez le mois que vous souhaitez'
        ACCEPT Wmois
        MOVE 0 TO WtotalIns
        MOVE 0 TO WtotalCours
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fcours NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                IF Wmois = fc_mois THEN
                        COMPUTE WtotalCours = WtotalCours + 1
                        OPEN INPUT finscription
                        MOVE 0 TO Wfin2
                        PERFORM WITH TEST AFTER UNTIL Wfin2 = 1
                           READ finscription NEXT
                           AT END MOVE 1 TO Wfin2 
                           NOT AT END
                                IF fc_num = fi_numC THEN
                                        COMPUTE WtotalIns = WtotalIns + 1
                                END-IF
                           END-READ
                        END-PERFORM
                        CLOSE finscription
                END-IF 
           END-READ
        END-PERFORM
        DISPLAY 'Nombre d inscriptions : 'WtotalIns
        DISPLAY 'Nombre de cours : 'WtotalCours
        DIVIDE  WtotalIns BY WtotalCours GIVING WmoyenneCours
        DISPLAY 'Moyenne inscription par cours :'WmoyenneCours
        CLOSE fcours.

        CONSULTER_EDT.
        OPEN INPUT fcours
        MOVE 0 TO Wfin
        DISPLAY 'Entrez votre identifiant :'
        ACCEPT Wnum
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fcours NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                IF Wnum = fc_numP THEN
                         DISPLAY '_______________________________'
                         DISPLAY 'Numero :', fc_num
                         DISPLAY 'Activite :', fc_activite
                         DISPLAY 'Horaire :', fc_horaire
                         DISPLAY 'Numero Professeur :', fc_numP
                         DISPLAY 'Type :', fc_type
                         DISPLAY 'Date :', fc_jour '/' fc_mois
                         DISPLAY 'Capacite :', fc_capacite
                         DISPLAY '_______________________________'
                END-IF
           END-READ
        END-PERFORM
        CLOSE fcours.

        FREQ_PAR_ACT.
        OPEN INPUT fcours
        MOVE 0 TO WtotalIns
        MOVE 0 TO WtotalCours
        MOVE 0 TO Wfin
        PERFORM WITH TEST AFTER UNTIL Wfin = 1
           READ fcours NEXT
           AT END MOVE 1 TO Wfin 
           NOT AT END
                STRING fc_activite INTO Wcdc
                IF Wcdc EQUALS 'Snowboard  ' THEN
                        COMPUTE WtotalCoursSnow = WtotalCoursSnow + 1
                        OPEN INPUT finscription
                        MOVE 0 TO Wfin2
                        PERFORM WITH TEST AFTER UNTIL Wfin2 = 1
                           READ finscription NEXT
                           AT END MOVE 1 TO Wfin2 
                           NOT AT END
                                IF fc_num = fi_numC THEN
                                        COMPUTE WtotalInsSnow = WtotalInsSnow + 1
                                END-IF
                           END-READ
                        END-PERFORM
                        CLOSE finscription
                END-IF 
                IF 'Ski alpin  ' EQUALS Wcdc THEN
                        COMPUTE WtotalCoursSkiA = WtotalCoursSkiA + 1
                        OPEN INPUT finscription
                        MOVE 0 TO Wfin2
                        PERFORM WITH TEST AFTER UNTIL Wfin2 = 1
                           READ finscription NEXT
                           AT END MOVE 1 TO Wfin2 
                           NOT AT END
                                IF fc_num = fi_numC THEN
                                        COMPUTE WtotalInsSkiA = WtotalInsSkia + 1
                                END-IF
                           END-READ
                        END-PERFORM
                        CLOSE finscription
                END-IF 
                IF 'Ski de fond' EQUALS Wcdc THEN
                        COMPUTE WtotalCoursSkiF = WtotalCoursSkiF + 1
                        OPEN INPUT finscription
                        MOVE 0 TO Wfin2
                        PERFORM WITH TEST AFTER UNTIL Wfin2 = 1
                           READ finscription NEXT
                           AT END MOVE 1 TO Wfin2 
                           NOT AT END
                                IF fc_num = fi_numC THEN
                                        COMPUTE WtotalInsSkiF = WtotalInsSkiF + 1
                                END-IF
                           END-READ
                        END-PERFORM
                        CLOSE finscription
                END-IF 
           END-READ
        END-PERFORM
        DISPLAY 'Snowboard'
        DISPLAY 'Nombre d inscriptions : 'WtotalInsSnow
        DISPLAY 'Nombre de cours : 'WtotalCoursSnow
        DIVIDE  WtotalInsSnow BY WtotalCoursSnow GIVING WmoyenneCoursSnow
        DISPLAY 'Moyenne inscription par cours :'WmoyenneCoursSnow
        DISPLAY ' '
        DISPLAY 'Ski Alpin'
        DISPLAY 'Nombre d inscriptions : 'WtotalInsSkiA
        DISPLAY 'Nombre de cours : 'WtotalCoursSkiA
        DIVIDE  WtotalInsSkiA BY WtotalCoursSkiA GIVING WmoyenneCoursSkiA
        DISPLAY 'Moyenne inscription par cours :'WmoyenneCoursSkiA
        DISPLAY ' '
        DISPLAY 'Ski de fond'
        DISPLAY 'Nombre d inscriptions : 'WtotalInsSkiF
        DISPLAY 'Nombre de cours : 'WtotalCoursSkiF
        DIVIDE  WtotalInsSkiF BY WtotalCoursSkiF GIVING WmoyenneCoursSkiF
        DISPLAY 'Moyenne inscription par cours :'WmoyenneCoursSkiF
        CLOSE fcours.
