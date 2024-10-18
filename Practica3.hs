module Practica3 where
    ----------------------------------------------------------------
    ------------------------ Practica 3 ----------------------------
    ----------------------------------------------------------------
    ---------------------------------------------------------------
    ----------------------------------------------------------------

    data Bit = Cero | Uno deriving Eq

    instance Show Bit where
        show Cero = "0"
        show Uno = "1"
  {-| Ejericio 1: simula una compuerta lógica "and"
  Ejemplos: 
  compuertaAND Cero Cero 
  compuertaAnd Cero Uno
  compuertaAnd Uno Cero
  compuertaAnd Uno Uno
  -}      
    compuertaAND :: Bit -> Bit -> Bit
    compuertaAND Cero  Cero = Cero
    compuertaAND Uno   Cero = Cero
    compuertaAND Cero  Uno  = Cero
    compuertaAND Uno   Uno  = Uno

    {-| Ejericio 2: simula una compuerta lógica "or"
    Ejemplos:
    compuertaOR Cero Cero
    compuertaOR Cero Uno
    compuertaOR Uno Cero
    compuertaOR Uno Uno
    -}
    compuertaOR :: Bit -> Bit -> Bit
    compuertaOR Cero   Cero = Cero
    compuertaOR Uno    Cero = Uno
    compuertaOR Cero   Uno  = Uno
    compuertaOR Uno    Uno  = Uno

    {-| Ejericio 3: simula una compuerta lógica "not"
    Ejemplos:
    compuertaNOT Cero 
    compuertaNOT Uno
    -}
    compuertaNOT :: Bit -> Bit
    compuertaNOT Cero = Uno
    compuertaNOT Uno  = Cero

    {-| Ejericio 4: simula una compuerta lógica "xor"
    Ejemplos:
    compuertaXOR Cero Cero
    compuertaXOR Cero Uno
    compuertaXOR Uno Cero
    compuertaXOR Uno Uno
    -}
    compuertaXOR :: Bit -> Bit -> Bit
    compuertaXOR a b = if a == b then Cero else Uno

    {-| Ejericio 5: sim ula una compuerta lógica "nand"
    Ejemplos:
    compuertaNAND Cero Cero
    compuertaNAND Cero Uno
    compuertaNAND Uno Cero
    compuertaNAND Uno Uno
    -}
    compuertaNAND :: Bit -> Bit -> Bit
    compuertaNAND Cero Cero = Uno
    compuertaNAND Cero Uno  = Uno
    compuertaNAND Uno  Cero = Uno
    compuertaNAND Uno  Uno  = Cero

    {-| Ejericio 6: simula una compuerta lógica "nor"
    Ejemplos:
    compuertaNOR Cero Cero
    compuertaNOR Cero Uno
    compuertaNOR Uno Cero
    compuertaNOR Uno Uno
    -}
    compuertaNOR :: Bit -> Bit -> Bit
    compuertaNOR Cero Cero = Uno
    compuertaNOR Cero Uno  = Cero
    compuertaNOR Uno  Cero = Cero
    compuertaNOR Uno  Uno  = Cero

    {-| Ejericio 7: simula una compuerta lógica "xnor"
    -}
    compuertaXNOR :: Bit -> Bit -> Bit
    compuertaXNOR a b = if a == b then Uno else Cero

    {-| Ejericio 8: Función para medio sumador (Half Ader )
    Ejemplos:
    halfAdder Cero Cero
    halfAdder Cero Uno  
    halfAdder Uno  Cero
    halfAdder Uno  Uno
    -}
    halfAdder :: Bit -> Bit -> (Bit, Bit)
    halfAdder a b = (compuertaXOR a b, compuertaAND a b)

    {-| Ejericio 9: Función para sumador completo (Full Adder)  
    Ejemplos:
    fullAdder Cero Cero Cero
    fullAdder Cero Cero Uno
    fullAdder Cero Uno  Cero
    fullAdder Cero Uno  Uno
    fullAdder Uno  Cero Cero
    fullAdder Uno  Cero Uno
    fullAdder Uno  Uno  Cero
    fullAdder Uno  Uno  Uno
    -}
    fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
    fullAdder a b c = (compuertaXOR (compuertaXOR a b) c, compuertaOR (compuertaAND a b) (compuertaOR (compuertaAND a c) (compuertaAND b c)))


