module Language.Aia where
import Language
--------------------- Data ---------------------
persone = Table "persone" ["id","cognome", "nome", "ragsoc", "luogonasc", "datanasc", "codfisc", "partiva"]
ruoliveic = Table "ruoliveic" ["idveic","idsin","ruolo"]
ruolipers = Table "ruolipers" ["idpers","idsin","ruolo"]
sinistri = Table "sinistri" ["id","data","stato","luogo","autor","danni","lesioni","decessi"]
veicoli = Table "veicoli" ["targa","telaio","dataimm"]

allSinistri = do
    s <- table sinistri
    projectAttr s ["data","stato","luogo","autor","danni","lesioni","decessi"]

-- allPersons = do
    -- p <- table persons
    -- projectAttr p ["id" ,"nome","cognome","datanasc","luogonasc","codfisc"]

-- allChecks = do
    -- c <- table checks 
    -- projectAttr c ["id","abi","cab","data","luogo","divisa"]
    
-- reqbyStatus st = do
    -- r <- table requests 
    -- restrict (r ! "sid" *==* st)
    -- projectAttr r ["id","sid","aid"]
           
-- reqByCheck c = do
    -- r <- table requests 
    -- restrict (r ! "aid" *==* c)
    -- projectAttr r ["id","sid","aid"]
            
-- getChecks id = do
    -- c <- table checks 
    -- restrict (c ! "id" *==* id)
    -- projectAttr c ["id","cab","abi","data","luogo","divisa"]

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
    a <- alternative [ w <@> "welcome"
                     , s <@> "search"
                     ]
    return $ n 
         <-> a

welcome = do
    name <- parameter "name"
    hello <- label (param name)
    return $ name <\> hello

search = do
    n <- formInput  "Codice sinistro:"    "codice"   Required (string "")
    t <- formInput  "Targa:"              "targa"    Required (string "")
    d <- formInput  "Data inizio:"        "inizio"   Required (string "")
    l <- formInput  "Data fine:"          "fine"     Required (string "")
    cf <- formInput "Codice fiscale:"     "fcode"    (Required <%> MinLen 16 <%> MaxLen 16) (string "")
    b <- button (string "Invio")
    f <- plainForm (    n <|> c 
                    <-> d <|> l 
                    <-> cf) b 
    transition b click EmptyAction (seg "results" ["targa" <=> value t])
    return f

results = do
    slist <- list allSinistri showSinistro
    return $ slist


showRequest r = do
    lid <- label (r ! "id")
    lsid <- label (r ! "sid")
    laid <- label (r ! "aid")
    return $ lid <|> lsid <|> laid
    
    
showSinistro s = do
    lid <- label (r ! "id")
    ldata <- label (r ! "data")
    lstato <- label (r ! "stato")
    lluogo <- label (r ! "luogo")
    lautor <- label (r ! "autor")
    ldanni <- label (r ! "danni")
    llesioni <- label (r ! "lesioni")
    ldecessi <- label (r ! "decessi")
    return $ (lid  <|> ldata <|> lstato <|> lluogo <|> lautor <|> ldanni <|> llesioni <|> ldecessi)

initSinistri = do
    insert $ "id" *=* (string "01") : "aid" *=* (string "41") : "sid" *=* (string "attesa")      : []
    
    
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
"id" *=* string "" : "data" *=* string "" : "stato" *=* string "" : "luogo" *=* string "" : "autor" *=* string "" : "danni" *=* string "" : "lesioni" *=* string "" : "decessi" *=* string "" :     
    
    
    
    
    
    
    
    
    
    
    
    
    
-- initStates = do
    -- into states
    -- insert $ "id" *=* (string "attesa") : []
    -- insert $ "id" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "conclusa") : []
-- initRequests = do
    -- into requests
    -- insert $ "id" *=* (string "01") : "aid" *=* (string "41") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "02") : "aid" *=* (string "05") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "03") : "aid" *=* (string "04") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "04") : "aid" *=* (string "07") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "05") : "aid" *=* (string "10") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "06") : "aid" *=* (string "06") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "07") : "aid" *=* (string "05") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "08") : "aid" *=* (string "08") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "09") : "aid" *=* (string "10") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "10") : "aid" *=* (string "10") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "11") : "aid" *=* (string "10") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "12") : "aid" *=* (string "05") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "13") : "aid" *=* (string "09") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "14") : "aid" *=* (string "06") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "15") : "aid" *=* (string "07") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "16") : "aid" *=* (string "05") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "17") : "aid" *=* (string "06") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "18") : "aid" *=* (string "03") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "19") : "aid" *=* (string "09") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "20") : "aid" *=* (string "05") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "21") : "aid" *=* (string "10") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "22") : "aid" *=* (string "01") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "23") : "aid" *=* (string "02") : "sid" *=* (string "attesa")      : []
    -- insert $ "id" *=* (string "24") : "aid" *=* (string "06") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "25") : "aid" *=* (string "01") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "26") : "aid" *=* (string "03") : "sid" *=* (string "lavorazione") : []
    -- insert $ "id" *=* (string "27") : "aid" *=* (string "05") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "28") : "aid" *=* (string "04") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "29") : "aid" *=* (string "04") : "sid" *=* (string "conclusa")    : []
    -- insert $ "id" *=* (string "30") : "aid" *=* (string "02") : "sid" *=* (string "conclusa")    : []
-- initChecks = do                                          
    -- into checks                
    -- insert $ "id" *=* (string "01") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "02") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "03") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "04") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "05") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "06") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "07") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "08") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "09") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "10") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "11") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "12") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "13") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "14") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "15") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "16") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "17") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "18") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "19") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "20") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "21") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "22") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "23") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "24") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "25") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "26") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "27") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "28") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "29") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "30") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "31") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "32") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "33") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "34") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "35") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "36") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "37") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "38") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "39") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "40") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "41") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "42") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "43") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "44") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "45") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "46") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "47") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
    -- insert $ "id" *=* (string "48") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "3") : []
    -- insert $ "id" *=* (string "49") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "1") : []
    -- insert $ "id" *=* (string "50") : "luogo" *=* (string "00100") :  "data" *=* (string "00120") : "data" *=* (string "01/01/2000") : "luogo" *=* (string "Roma") : "divisa" *=* (string "EUR") : "pid" *=* (string "2") : []
-- initPersons = do                                          
    -- into persons
    -- insert $ "id" *=* (string "1") : "nome" *=* (string "nome1") : "cognome" *=* (string "cognome1") : "datanasc" *=* (string "01/01/2000") : "luogonasc" *=* (string "Roma") : "codfisc" *=* (string "DJFEIKA78L34Y4784C") : []
    -- insert $ "id" *=* (string "2") : "nome" *=* (string "nome2") : "cognome" *=* (string "cognome2") : "datanasc" *=* (string "01/01/2000") : "luogonasc" *=* (string "Roma") : "codfisc" *=* (string "DJFEIKA38L34Y4784C") : []
    -- insert $ "id" *=* (string "3") : "nome" *=* (string "nome3") : "cognome" *=* (string "cognome3") : "datanasc" *=* (string "01/01/2000") : "luogonasc" *=* (string "Roma") : "codfisc" *=* (string "DJFEIKA78L34Y4744C") : []
model = []