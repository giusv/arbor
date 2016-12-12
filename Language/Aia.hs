{-#LANGUAGE TypeOperators,MultiParamTypeClasses,TypeSynonymInstances,FlexibleInstances,FlexibleContexts #-}
module Language.Aia where
import Language

-- initInterface = Pose $ seg "home" [] </> seg "dettagli" ["sinistro" <=> string "1"]
-- initInterface = Pose $ seg "home" [] </> seg "resultsbytarga" ["targa" <=> string "a"]
initInterface = Pose $ seg "login" []
        
--------------------- Elements ---------------------
interface = do
    log <- login
    hom <- home
    a <- alternative [ log <@> "login"
                     , hom <@> "home"
                     ]
    return $ a

login = do
    u <- input  "Userid:"   (string "")
    p <- input  "Password:" (string "")
    b <- button (string "Accedi")
    pl <- label $ string "Login"
    bl <- blank
    hl <- hpanel "primary" "fa fa-user fa-5x" pl (u <-> p <-> b ) bl
    transition b click EmptyAction (seg "home" [] </> seg "welcome" ["userid" <=> value u])
    return $ bl <|> hl <|> bl

nav = do
    hb <- link "Home"    (seg "home" [] </> seg "welcome" ["userid" <=> string "unknown"])
    tb <- link "Ricerca Soggetti coinvolti" (seg "home" [] </> seg "searchbynome"  [])
    sb <- link "Ricerca Identificativi veicolo" (seg "home" [] </> seg "searchbytarga"  [])
    nb <- navbar [hb,tb,sb]
    return nb
    
home = do
    n <- nav
    w <- welcome
    d <- dettagli
    st <- searchbytarga
    rt <- resultsbytarga
    sn <- searchbynome
    rn <- resultsbynome
    a <- alternative [ w <@> "welcome"
                     , d <@> "dettagli"
                     , st <@> "searchbytarga"
                     , rt <@> "resultsbytarga"
                     , sn <@> "searchbynome"
                     , rn <@> "resultsbynome"
                     ]
    return $ n 
         <-> a


welcome = do
    id <- parameter "userid"
    sbn <- link "Vai alla pagina" (seg "home" [] </> seg "searchbynome"  [])
    lbn <- label (string "Ricerca per nominativo")
    sbt <- link "Vai alla pagina" (seg "home" [] </> seg "searchbytarga"  [])
    lbt <- label (string "Ricerca identificativi veicolo")
    uh <- label (string "Utente")
    bl <- blank
    ub <- list (utenteById $ param id) showUtente
    utente <- hpanel "primary" "fa fa-user-o fa-5x" uh ub bl
    searchbytarga <- hpanel "primary" "fa fa-car fa-5x" lbt sbt bl
    searchbynome <- hpanel "danger" "fa fa-users fa-5x" lbn sbn bl
    return $ id <\> utente 
                <-> searchbytarga <|> searchbynome
showUtente q = do
    luserid <- doubleLabel "Userid" q "userid"
    lnome <- doubleLabel "Nome" q "nome"
    lcognome <- doubleLabel "Cognome" q "cognome" 
    lult <- doubleLabel "Ultimo accesso" q "ultacc" 
    lprof <- doubleLabel "Profilo" q "prof" 
    return $ luserid <-> lnome <-> lcognome <-> lult <-> lprof
    
searchbytarga = do
    t <- input  "Targa:"       (string "")
    d <- input  "Data inizio:" (string "")
    l <- input  "Data fine:"   (string "")
    b <- button (string "Invio")
    transition b click EmptyAction (seg "home" [] </> seg "resultsbytarga" ["targa" <=> value t])
    bl <- blank
    return $ (t 
         <-> d 
         <-> l
         <-> b) <|> bl 
  
searchbynome = do
    n <- input  "Codice sinistro:"  (string "")
    d <- input  "Data inizio:"      (string "")
    l <- input  "Data fine:"        (string "")
    c <- input "Codice fiscale:"    (string "")
    b <- button (string "Invio")
    transition b click EmptyAction (seg "home" [] </> seg "resultsbynome" ["nome" <=> value c])
    bl <- blank
    return $ (n 
         <-> d 
         <-> l 
         <-> c
         <-> b) <|> bl 

resultsbytarga = do
    t <- parameter "targa"
    ptable <- htable (parametriByTarga (param t)) showParametriTable
    ph <- label $ string "Parametri di significativita"
    bl <- blank
    hp <- hpanel "primary" "fa fa-pie-chart fa-5x" ph ptable bl
    let plotVs v = (v ! "x",v ! "y") 
    c <- chart allVs plotVs
    ps <- label $ string "Dati del sinistro"
    stable <- htable (sinistriByTarga (param t)) showSinistroTable
    hs <- hpanel "danger" "fa fa-car fa-5x" ps stable bl
    return $ t <\> hp
               <-> c
               <-> hs


resultsbynome = do
    n <- parameter "nome"
    stable <- htable (sinistriByNome (param n)) showSinistroTable
    return $ n <\> stable

showParametriTable v = do
    lric <- label (v ! "Ricorrenze")
    lv1 <- label (v ! "V1")
    lv2 <- label (v ! "V2")
    lv3 <- label (v ! "V3")
    lv4 <- label (v ! "V4")
    lv5 <- label (v ! "V5")
    lv6 <- label (v ! "V6")
    return $ lric .*. lv1 .*. lv2 .*. lv3 .*. lv4 .*. lv5 .*. lv6 .*. HNil
    
showSinistroTable r = do
    lid <- label        (r ! "Codice")
    ldataacc <- label   (r ! "Accadimento")
    ldataden <- label   (r ! "Denuncia")
    ldatadef <- label   (r ! "Definizione")
    lstato <- label     (r ! "Stato")
    lluogo <- label     (r ! "Luogo")
    lautor <- label     (r ! "Autorita")
    ldanni <- label     (r ! "Danni")
    llesioni <- label   (r ! "Decessi")
    ldecessi <- label   (r ! "Lesioni")
    ptable <- htable (personeBySinistro (value lid)) showPersonaTable
    vtable <- htable (veicoliBySinistro (value lid)) showVeicoloTable
    dett <- button (string "Dettagli")
    transition dett click EmptyAction (seg "home" [] </> seg "dettagli" ["sinistro" <=> value lid])
    return $ lid .*. ldataacc .*. ldataden .*. ldatadef .*. lstato .*. lluogo .*. lautor .*. ldanni .*. llesioni .*. ldecessi .*. ptable .*. vtable .*. dett .*. HNil

dettagliPersone = do
    s <- parameter "sinistro"
    h <- label (string "Persone coinvolte nel sinistro")
    -- lid <- label (r ! "idSinistro")
    ptable <- htable (personeBySinistro (param s)) showPersonaTable
    bl <- blank
    hp <- hpanel "danger" "fa fa-users fa-5x" 
            h  -- <|> lid <|> bl
            ptable
            bl
    return hp
    
dettagliVeicoli = do
    s <- parameter "sinistro"
    h <- label (string "Veicoli coinvolti nel sinistro")
    -- lid <- label (r ! "idSinistro")
    vtable <- htable (dettagliVeicoliBySinistro (param s)) showDettagliVeicoloTable
    bl <- blank
    hp <- hpanel "danger" "fa fa-car fa-5x"
            h
            vtable
            bl
    return hp
    
dettagli = do
    s <- parameter "sinistro"
    l <- list (sinistroById $ param s) showDettagli
    let plotVs v = (v ! "x",v ! "y") 
    c <- chart allVs plotVs
    p <- dettagliPersone
    v <- dettagliVeicoli
    a <- alternative [ p <@> "persone"
                     , v <@> "veicoli"
                     ]
    return $ s <\> l <|> c
               <-> a
               
showPersonaTable p = do
    lnome <- label (p ! "Nome") 
    lcognome <- label (p ! "Cognome")
    return $ lnome .*. lcognome .*. HNil
    
showVeicoloTable v = do
    ltarga <- label (v ! "Targa") 
    ltelaio <- label (v ! "Telaio")
    return $ ltarga .*. ltelaio .*. HNil
            
showDettagliVeicoloTable v = do
    lruolo <- label (v ! "Ruolo") 
    ltarga <- label (v ! "Targa") 
    ltelaio <- label (v ! "Telaio")
    lcond <- label ((v ! "Nome") .+. string " " .+. (v ! "Cognome"))
    return $ lruolo .*. ltarga .*. ltelaio .*. lcond .*. HNil

showDettagli s = do
    h <- label (string "Dettagli sinistro")
    lid <- label (s ! "idSinistro")
    let countVeicoli = do
        r <- veicoliBySinistro $ value lid
        count
    let countPersone = do
        r <- personeBySinistro $ value lid
        count
    lnumv <- aggregate countVeicoli
    lnumvlab <- label (string "Numero di veicoli coinvolti")
    lnump <- aggregate countPersone
    lnumplab <- label (string "Numero di persone coinvolte")
    ldataacc <- doubleLabel "Data accadimento" s "dataacc"
    ldataden <- doubleLabel "Data denuncia" s "dataden"
    ldatadef <- doubleLabel "Data definizione" s "datadef"
    lstato <- doubleLabel "Stato del sinistro" s "stato"
    lluogo <- doubleLabel "Luogo di accadimento" s "luogo"
    lautor <- doubleLabel "Intervento dell'autorita" s "autor"
    ldanni <- doubleLabel "Danni a cose" s "danni"
    llesioni <- doubleLabel "Lesioni fisiche" s "lesioni"
    ldecessi <- doubleLabel "Numero di decessi" s "decessi"
    bp <- button (string "Dettagli")
    transition bp click EmptyAction (seg "home" [] </> (seg "dettagli" ["sinistro" <=> value lid]  </> seg "persone" ["sinistro" <=> value lid]))
    bv <- button (string "Dettagli")
    transition bv click EmptyAction (seg "home" [] </> (seg "dettagli" ["sinistro" <=> value lid] </> seg "veicoli" ["sinistro" <=> value lid]))
    bl <- blank
    hp <- hpanel "primary" "fa fa-car fa-5x" (h <|> lid)
                (lstato
             <-> lluogo
             <-> lautor
             <-> ldataacc
             <-> ldataden
             <-> ldatadef
             <-> ldanni
             <-> llesioni
             <-> ldecessi
             <-> lnumvlab <|> lnumv <|> bv
             <-> lnumplab <|> lnump <|> bp)
                bl
    return $ hp

doubleLabel l1 c l2 = do
    l1' <- label (string l1)
    l2' <- label (c ! l2)
    bl <- blank
    return $ l1' <|> l2' <|> bl
    
--------------------- Data ---------------------
persone = Table "persone" ["idPersona","cognome", "nome", "ragsoc", "luogonasc", "datanasc", "codfisc", "partiva"]
ruoliveic = Table "ruoliveic" ["idveic","idsin","ruolo","idcond"]
ruolipers = Table "ruolipers" ["idpers","idsin","ruolo"]
sinistri = Table "sinistri" ["idSinistro","dataacc","dataden","datadef","stato","luogo","autor","danni","lesioni","decessi"]
veicoli = Table "veicoli" ["targa","telaio","dataimm","idprop","idcontr","ricorr","v1","v2","v3","v4","v5","v6"]
utenti = Table "utenti" ["userid","nome","cognome","ultacc","prof"]

------ Query -----------
sinistroById i = do
    s <- table sinistri
    restrict (s ! "idSinistro" *==* i)
    project [ s ! "idSinistro" *@* "idSinistro"
            , s ! "dataacc" *@* "dataacc"
            , s ! "dataden" *@* "dataden"
            , s ! "datadef" *@* "datadef"
            , s ! "stato" *@* "stato"
            , s ! "luogo" *@* "luogo"
            , s ! "autor" *@* "autor"
            , s ! "danni" *@* "danni"
            , s ! "decessi" *@* "decessi"
            , s ! "lesioni" *@* "lesioni"
            ]

parametriByTarga t = do
    v <- table veicoli
    restrict (v ! "targa" *==* t)
    project [ v ! "ricorr" *@* "Ricorrenze"
            , v ! "v1" *@* "V1"
            , v ! "v2" *@* "V2"
            , v ! "v3" *@* "V3"
            , v ! "v4" *@* "V4"
            , v ! "v5" *@* "V5"
            , v ! "v6" *@* "V6"
            ]
attrSinistro s = [ s ! "idSinistro"     *@* "Codice"
                 , s ! "dataacc"        *@* "Accadimento"
                 , s ! "dataden"        *@* "Denuncia"
                 , s ! "datadef"        *@* "Definizione"
                 , s ! "stato"          *@* "Stato"
                 , s ! "luogo"          *@* "Luogo"
                 , s ! "autor"          *@* "Autorita"
                 , s ! "danni"          *@* "Danni"
                 , s ! "decessi"        *@* "Decessi"
                 , s ! "lesioni"        *@* "Lesioni"
                 ]
sinistriByTarga t = do
    s <- table sinistri
    r <- table ruoliveic
    v <- table veicoli
    restrict ((v ! "targa" *==* t) *&&* ((r ! "idveic" *==* v ! "targa") *&&* (s ! "idSinistro" *==* r ! "idsin")))
    project $ (attrSinistro s) ++ [ s ! "lesioni" *@* "Persone coinvolte"
                                  , s ! "lesioni" *@* "Veicoli coinvolti"]
sinistriByNome n = do
    s <- table sinistri
    r <- table ruolipers
    p <- table persone
    restrict ((p ! "cognome" *==* n) *&&* ((r ! "idpers" *==* p ! "idPersona") *&&* (s ! "idSinistro" *==* r ! "idsin")))
    project $ attrSinistro s
personeBySinistro s = do
    r <- table ruolipers
    p <- table persone
    restrict ((r ! "idsin" *==* s) *&&* (p ! "idPersona" *==* r ! "idpers"))
    project [ p ! "nome" *@* "Nome"
            , p ! "cognome" *@* "Cognome"
            ]

veicoliBySinistro s = do
    r <- table ruoliveic
    v <- table veicoli
    restrict ((r ! "idsin" *==* s) *&&* (v ! "targa" *==* r ! "idveic"))
    project [ v ! "targa" *@* "Targa"
            , v ! "telaio" *@* "Telaio"
            ]

dettagliVeicoliBySinistro s = do
    r <- table ruoliveic
    v <- table veicoli
    p <- table persone
    restrict ((r ! "idsin" *==* s) *&&* ((v ! "targa" *==* r ! "idveic") *&&* (p ! "idPersona" *==* r ! "idcond")))
    project [ r ! "ruolo" *@* "Ruolo"
            , v ! "targa" *@* "Targa"
            , v ! "telaio" *@* "Telaio"
            , p ! "nome" *@* "Nome"
            , p ! "cognome" *@* "Cognome"
            ]

utenteById id = do
    u <- table utenti
    restrict (u ! "userid" *==* id)
    projectAttr u ["userid","nome","cognome","ultacc","prof"]

allVs = do
    v <- table vdata
    projectAttr v ["x","y"]
    
    
---- initialization ----
vdata = Table "vdata" ["x","y"]
initVdata = do
    into vdata
    insert $ "x" *=* (string "v1") : "y" *=* (string "0") : []
    insert $ "x" *=* (string "v2") : "y" *=* (string "1") : []
    insert $ "x" *=* (string "v3") : "y" *=* (string "2") : []
    insert $ "x" *=* (string "v4") : "y" *=* (string "3") : []
    insert $ "x" *=* (string "v5") : "y" *=* (string "4") : []
    insert $ "x" *=* (string "v6") : "y" *=* (string "5") : []

model = [initVdata,initSinistri,initRuolipers,initPersone,initVeicoli,initRuoliveic,initUtenti]
initSinistri = do
    into sinistri
    insert $ "idSinistro" *=* string "0" : "dataacc" *=* string "01/12/2015" : "dataden" *=* string "01/12/2015" : "datadef" *=* string "01/12/2015" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    insert $ "idSinistro" *=* string "1" : "dataacc" *=* string "01/12/2016" : "dataden" *=* string "01/12/2016" : "datadef" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "2" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "3" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "4" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "5" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "6" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "7" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "8" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "9" : "dataacc" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
        
    
initRuolipers = do
    into ruolipers
    insert $ "idpers" *=* string "0" : "idsin" *=* string "1" : "ruolo" *=* string "coinvolto" : []        
    insert $ "idpers" *=* string "1" : "idsin" *=* string "1" : "ruolo" *=* string "danneggiato" : []        
    
initPersone = do
    into persone
    insert $ "idPersona" *=* string "0" : "cognome" *=* string "Rossi" : "nome" *=* string "Mario" : "ragsoc" *=* string "***" : "luogonasc" *=* string "Roma" : "datanasc" *=* string "17/02/1980" : "codfisc" *=* string "RSSMRI80D45H789N" : "partiva" *=* string "***" : []
    insert $ "idPersona" *=* string "1" : "cognome" *=* string "Bianchi" : "nome" *=* string "Sergio" : "ragsoc" *=* string "***" : "luogonasc" *=* string "Roma" : "datanasc" *=* string "17/02/1960" : "codfisc" *=* string "BNCSRG60D45H789N" : "partiva" *=* string "***" : []
   
initRuoliveic = do
    into ruoliveic
    insert $ "idveic" *=* string "a" : "idsin" *=* string "1" : "ruolo" *=* string "coinvolto" : "idcond" *=* string "0" : []        
    insert $ "idveic" *=* string "b" : "idsin" *=* string "1" : "ruolo" *=* string "danneggiato" : "idcond" *=* string "1" : []        
    
initVeicoli = do
    into veicoli
    insert $ "targa" *=* string "a" : "telaio" *=* string "qwerty" : "dataimm" *=* string "20/05/1988" : "idprop" *=* string "0" : "idcontr" *=* string "0" : "ricorr" *=* string "0" : "v1" *=* string "0" : "v2" *=* string "0" : "v3" *=* string "0" : "v4" *=* string "0" : "v5" *=* string "0" : "v6" *=* string "0" : []
    insert $ "targa" *=* string "b" : "telaio" *=* string "asdfgh" : "dataimm" *=* string "20/05/1989" : "idprop" *=* string "1" : "idcontr" *=* string "1" : "ricorr" *=* string "0" : "v1" *=* string "1" : "v2" *=* string "1" : "v3" *=* string "2" : "v4" *=* string "1" : "v5" *=* string "3" : "v6" *=* string "0" : []

initUtenti = do
    into utenti
    insert $ "userid" *=* string "agenzia01" : "nome" *=* string "Giuseppe" : "cognome" *=* string "Verdi" : "prof" *=* string "compagnia" : []
    insert $ "userid" *=* string "agenzia02" : "nome" *=* string "Marco" : "cognome" *=* string "Neri" : "prof" *=* string "ministero" : []
    
    
    
    


-- class HList l => HBuild a l where
    -- hBuild :: [a] -> l
-- instance HBuild a HNil where
    -- hBuild _ = HNil
-- instance HBuild a l => HBuild a (HCons a l) where
    -- hBuild (x:xs) = HCons x (hBuild xs)
-- testtt = (hBuild ["1","2"]) -- :: (String :*: String :*: HNil)    
-- labels :: HList l => Rel -> [String] -> l
-- labels r ls = do 
    -- ls' <- sequence $ map (\l -> label (r ! l)) ls
    -- return $ hBuild ls'
-- hBuild :: HList l => [a] -> l
-- hBuild [] = HNil
-- hBuild (x:xs) = HCons x (hBuild xs)