module Language.Aia where
import Language
--------------------- Elements ---------------------
interface = do
    log <- login
    hom <- home
    a <- alternative [ log <@> "login"
                     , hom <@> "home"
                     ]
    return $ a

login = do
    nm  <- formInput  "Utente:"             "name"           Required (string "")
    pwd <- formInput  "Password:"           "password"       Required (string "")
    acc <- button (string "Accedi")
    accTrans <- transition acc Click EmptyAction (seg "home" [] </> seg "welcome" [])
    f <- plainForm (    nm
                    <-> pwd) acc
    return f

nav = do
    hb <- link "Home"    (seg "home" [] </> seg "welcome" [])
    ttb <- link "Test"    (seg "home" [] </> seg "test" [])
    tb <- link "Ricerca Soggetti coinvolti" (seg "home" [] </> seg "searchbynome"  [])
    sb <- link "Ricerca Identificativi veicolo" (seg "home" [] </> seg "searchbytarga"  [])
    nb <- navbar [hb,tb,sb,ttb]
    return nb
    
home = do
    n <- nav
    w <- welcome
    t <- test
    st <- searchbytarga
    rt <- resultsbytarga
    sn <- searchbynome
    rn <- resultsbynome
    a <- alternative [ w <@> "welcome"
                     , t <@> "test"
                     , st <@> "searchbytarga"
                     , rt <@> "resultsbytarga"
                     , sn <@> "searchbynome"
                     , rn <@> "resultsbynome"
                     ]
    return $ n 
         <-> a


welcome = do
    sbn <- link "Vai alla pagina" (seg "home" [] </> seg "searchbynome"  [])
    sbt <- link "Vai alla pagina" (seg "home" [] </> seg "searchbytarga"  [])
    searchbytarga <- panel "primary" sbt (string "Ricerca identificativi veicolo")
    searchbynome <- panel "danger" sbn (string "Ricerca per nominativo")
    return $ searchbytarga <|> searchbynome

searchbytarga = do
    t <- input  "Targa:"       (string "")
    d <- input  "Data inizio:" (string "")
    l <- input  "Data fine:"   (string "")
    b <- button (string "Invio")
    transition b click EmptyAction (seg "home" [] </> seg "resultsbytarga" ["targa" <=> value t])
    return $ t 
         <-> d 
         <-> l
         <-> b 
         
    
test = do
    l <- label $ string "test"
    return l
    
    
searchbynome = do
    n <- input  "Codice sinistro:"  (string "")
    d <- input  "Data inizio:"      (string "")
    l <- input  "Data fine:"        (string "")
    c <- input "Codice fiscale:"    (string "")
    b <- button (string "Invio")
    transition b click EmptyAction (seg "home" [] </> seg "resultsbynome" ["nome" <=> value c])
    return $ n 
         <-> d 
         <-> l 
         <-> c
         <-> b 

resultsbytarga = do
    t <- parameter "targa"
    l <- label (param t)
    lid <- label (string "id")
    ldata <- label (string "data")
    lstato <- label (string "stato")
    lluogo <- label (string "luogo")
    lautor <- label (string "autor")
    ldanni <- label (string "danni")
    llesioni <- label (string "lesioni")
    ldecessi <- label (string "decessi")
    lpersone <- label (string "persone")
    stable <- htable allSinistri showSinistroTable
    return $ t <\> l
               <-> stable


resultsbynome = do
    n <- parameter "nome"
    l <- label (param n)
    stable <- htable allSinistri showSinistroTable
    return $ n <\> l
               <-> stable

showSinistroTable r = do
    lid <- label (r ! "idSinistro")
    ldata <- label (r ! "data")
    lstato <- label (r ! "stato")
    lluogo <- label (r ! "luogo")
    lautor <- label (r ! "autor")
    ldanni <- label (r ! "danni")
    llesioni <- label (r ! "lesioni")
    ldecessi <- label (r ! "decessi")
    ptable <- htable (personeBySinistro (value lid)) showPersonaTable
    return $ lid .*. ldata .*. lstato .*. lluogo .*. lautor .*. ldanni .*. llesioni .*. ldecessi .*. ptable .*. HNil

showPersona p = do
    lab <- label ((p ! "nome") .+. (p ! "cognome"))
    return $ lab
    

showPersonaTable p = do
    lnome <- label (p ! "nome") 
    lcognome <- label (p ! "cognome")
    return $ lnome .*. lcognome .*. HNil
    
    
    
    
--------------------- Data ---------------------
persone = Table "persone" ["idPersona","cognome", "nome", "ragsoc", "luogonasc", "datanasc", "codfisc", "partiva"]
ruoliveic = Table "ruoliveic" ["idveic","idsin","ruolo"]
ruolipers = Table "ruolipers" ["idpers","idsin","ruolo"]
sinistri = Table "sinistri" ["idSinistro","data","stato","luogo","autor","danni","lesioni","decessi"]
veicoli = Table "veicoli" ["targa","telaio","dataimm"]

------ Query -----------
allSinistri = do
    s <- table sinistri
    projectAttr s ["idSinistro","data","stato","luogo","autor","danni","lesioni","decessi"]

personeBySinistro s = do
    r <- table ruolipers
    p <- table persone
    restrict ((r ! "idsin" *==* s) *&&* (p ! "idPersona" *==* r ! "idpers"))
    project [ p ! "nome" *@* "nome"
            , p ! "cognome" *@* "cognome"
            ]

            
---- initialization ----
model = [initSinistri,initRuolipers,initPersone]
initSinistri = do
    into sinistri
    insert $ "idSinistro" *=* string "0" : "data" *=* string "01/12/2015" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    insert $ "idSinistro" *=* string "1" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "2" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "3" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "4" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "5" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "6" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "7" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "8" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
    -- insert $ "idSinistro" *=* string "9" : "data" *=* string "01/12/2016" : "stato" *=* string "liquidato" : "luogo" *=* string "Roma" : "autor" *=* string "si" : "danni" *=* string "no" : "lesioni" *=* string "no" : "decessi" *=* string "no" : []    
        
    
initRuolipers = do
    into ruolipers
    insert $ "idpers" *=* string "0" : "idsin" *=* string "1" : "ruolo" *=* string "coinvolto" : []        
    insert $ "idpers" *=* string "1" : "idsin" *=* string "1" : "ruolo" *=* string "danneggiato" : []        
    
initPersone = do
    into persone
    insert $ "idPersona" *=* string "0" : "cognome" *=* string "Rossi" : "nome" *=* string "Mario" : "ragsoc" *=* string "***" : "luogonasc" *=* string "Roma" : "datanasc" *=* string "17/02/1980" : "codfisc" *=* string "RSSMRI80D45H789N" : "partiva" *=* string "***" : []
    insert $ "idPersona" *=* string "1" : "cognome" *=* string "Bianchi" : "nome" *=* string "Sergio" : "ragsoc" *=* string "***" : "luogonasc" *=* string "Roma" : "datanasc" *=* string "17/02/1960" : "codfisc" *=* string "BNCSRG60D45H789N" : "partiva" *=* string "***" : []
   