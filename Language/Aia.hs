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
    accTrans <- transition acc Click EmptyAction (seg "home" [] </> seg "welcome" ["name" <=> value nm])
    f <- plainForm (    nm
                    <-> pwd) acc
    return f

nav = do
    hb <- link "Home"    (seg "home" [] </> seg "welcome" ["name" <=> string ""])
    sb <- link "Ricerca" (seg "home" [] </> seg "search"  [])
    nb <- navbar [hb,sb]
    return nb
    
home = do
    n <- nav
    w <- welcome
    s <- search
    r <- results
    a <- alternative [ w <@> "welcome"
                     , s <@> "search"
                     , r <@> "results"
                     ]
    return $ n 
         <-> a

welcome = do
    name <- parameter "name"
    hello <- label (param name)
    return $ name <\> hello

search = do
    inp <- input "Targa:" (string "")
    but <- button (string "Cerca")
    transition but click none (seg "home" [] </> seg "results" ["targa" <=> (value inp)])
    return $ inp 
         <-> but

search' = do
    n <- formInput  "Codice sinistro:"    "codice"   Required (string "")
    t <- formInput  "Targa:"              "targa"    Required (string "")
    d <- formInput  "Data inizio:"        "inizio"   Required (string "")
    l <- formInput  "Data fine:"          "fine"     Required (string "")
    cf <- formInput "Codice fiscale:"     "fcode"    (Required <%> MinLen 16 <%> MaxLen 16) (string "")
    b <- button (string "Invio")
    f <- plainForm (    n <|> t 
                    <-> d <|> l 
                    <-> cf) b 
    transition b click EmptyAction (seg "home" [] </> seg "results" ["targa" <=> value t])
    return f

results = do
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
    slist <- list allSinistri showSinistro
    return $ t <\> l
               <-> (lid  <|> ldata <|> lstato <|> lluogo <|> lautor <|> ldanni <|> llesioni <|> ldecessi <|> lpersone)
               <-> slist


-- showRequest r = do
    -- lid <- label (r ! "id")
    -- lsid <- label (r ! "sid")
    -- laid <- label (r ! "aid")
    -- return $ lid <|> lsid <|> laid
    
    
showSinistro r = do
    lid <- label (r ! "idSinistro")
    ldata <- label (r ! "data")
    lstato <- label (r ! "stato")
    lluogo <- label (r ! "luogo")
    lautor <- label (r ! "autor")
    ldanni <- label (r ! "danni")
    llesioni <- label (r ! "lesioni")
    ldecessi <- label (r ! "decessi")
    plist <- list (personeBySinistro (r ! "idSinistro")) showPersona
    return $ (lid  <|> ldata <|> lstato <|> lluogo <|> lautor <|> ldanni <|> llesioni <|> ldecessi <|> plist)

showPersona p = do
    lnome <- label (p ! "nome")
    lcognome <- label (p ! "cognome")
    return $ lnome <|> lcognome
    
    
    
    
--------------------- Data ---------------------
persone = Table "persone" ["idPersona","cognome", "nome", "ragsoc", "luogonasc", "datanasc", "codfisc", "partiva"]
ruoliveic = Table "ruoliveic" ["idveic","idsin","ruolo"]
ruolipers = Table "ruolipers" ["idpers","idsin","ruolo"]
sinistri = Table "sinistri" ["idSinistro","data","stato","luogo","autor","danni","lesioni","decessi"]
veicoli = Table "veicoli" ["targa","telaio","dataimm"]


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
