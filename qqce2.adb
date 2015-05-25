
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|source: qqce2.adb :: [ADA] *flat *ascii *file
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|
--| #####     ##    #####     ##     ####    ####
--| #    #   #  #   #    #   #  #   #       #
--| #####   #    #  #    #  #    #   ####    ####
--| #    #  ######  #    #  ######       #       #
--| #    #  #    #  #    #  #    #  #    #  #    #
--| #####   #    #  #####   #    #   ####    ####
--|
--|  ####    ####   #       #    #   ####   ######
--| #       #    #  #       #    #  #    #  #
--|  ####   #    #  #       #    #  #       #####
--|      #  #    #  #       #    #  #       #
--| #    #  #    #  #       #    #  #    #  #
--|  ####    ####   ######   ####    ####   ######
--|

--|   ------    ----------------    ------     <---- HARDCODED SOLUCE (file)
--|   |    |    |    | -  |    |    | 66 |
--|   ------    ----------------    ------
--|   | +  |    | *  |    | -  |    | =  |
--|   ------    ------    ------    ------
--|   | 13 |    | 12 |    | 11 |    | 10 |
--|   ------    ------    ------    ------
--|   | *  |    | +  |    | +  |    | -  |
--|   ------    ------    ------    ------
--|   |    |    |    |    |    |    |    |
--|   ----------------    ----------------
--|   | :  |    | +  |    | *  |    | :  |
--|   ----------------    ----------------
--|

--|#############################################################################
--| Attention bordelle grave BADASS a eliminer, refactoring au derche...
--| CE PROGRAM N'EST EXEMPLAIRE EN RIEN !!!
--| THIS IS'NT AN EDUCATIONAL OBJECT, AT ALL, NEED TO BE SEVERALY REFACTORED...
--|#############################################################################
--|
--|            _________________________________________________________________
--| Title:     LDQ :: Lowest Divisor of a Natural
--|            Non probabilist ...
--|            |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|
--| plateform: Linux 2.6.38 #1 SMP EDT 2011 i686 GNU/Linux Backtrack 5 10.04 LTS
--| $H4rd:     ASUS eeePC, ASUS eeeBox --no more details @FOYK --
--| $7ypical:  gnatmake -Wall -gnat05 -gnataif qqce2.adb
--| $she11:    /usr/bin/tcsh
--| Braind6ad: @void1nside [ccQ]
--|_____________________________________________________________________________
--|
--|     ---------------------------------------------------------------------
--| L2  HEAP memory :: [le tas, allocate/deallocate].fr
--|     ---------------------------------------------------------------------
--| L1  STACK memory :: [allocation automatique et contextuelle: Pile(s)].fr
--|     ---------------------------------------------------------------------
--| L0  STATIC memory :: [accessibilite globale not= scope! process_level].fr
--|     ---------------------------------------------------------------------
--|_____________________________________________________________________________
--|
pragma license(Unrestricted);
pragma ada_2005;
--|
with ada.text_IO;          --| console/terminal textual outputs (e.g. tty)
with ada.integer_text_IO;  --| format integer's image (as string)
with ada.command_line;     --| having shell/cmdline arguments,counts,rc,...
with interfaces;           --| working with bitwise...
--|                                                                  @void1nside
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| // --------------------------------------------------------------
--| // -- the Lowest Divisor function (ldQ) 
--| // -- MY ONE from refs... BaaAd Springer(s), sample #4 incomplete
--| // -- an exercise ? NOT@ALL ...
--| // -- Having done a first "try" in c/C++
--| // --------------------------------------------------------------
--| //
--| // refs:
--| //   pages: p(166)
--| //   Title: <<Programming for Mathematicians>> - 
--| //   Author: Raymond Seroul
--| //   Springer :: serie -UNIVERSITEXT-
--| //
--| // This one is my own one ass(-Q)
--| // (only checked on MacOSX - PowerPC G4 - half 64bits...)
--| //
--| static inline uint64 ldq(const uint64 number_,
--|                          const uint64 qBegin_ = 3ull) {
--|   //---------------------------------------------------
--|   if ( number_ < 2ull)          return number_;
--|   if ((number_ & 0x01) == 0x00) return 2ull;
--|   register uint64 q = qBegin_; // q-impaire(s) a tester par sauts
--|   if (q < 3ull)   q = 3ull;
--|   if (euclidian_mod(q,2ull)==0ull) q++; // pour ze robustess.[BigImplement]
--|   register uint64 square = q*q; // 'q' est un [carre]...
--|   while (square < number_) {
--|     if (euclidian_mod(number_,q)==0ull) return q; // TRASH LOOP EXIT (JUMP)
--|     square += (q<<0x02) + 4ull; // soit: square += 4*(q+1)
--|     q += 2ull;
--|   }; // end while
--|   if (square == number_) return q;       // c'est un [nombre carre]
--|   if (square >  number_) return number_; // 'q' passe la frontiere du carre
--|   return number_;
--| }; // end ldq
--|
--|                                                        @void1nside.duploland
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
procedure qqce2 is
  use ada.integer_text_IO;
  use ada.text_IO;
  use ada.command_line;
  use interfaces;

--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| footwarePack :: nawakWare :: local pack for huhuhu... match potatoes utils
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  package footwarePack is
    type wordulo64 is mod 2 ** 64;
    for wordulo64'size use 64; -- bitwise/64

    type dirtyString(lenmax : natural) is record
      length : natural := 0;
      value  : string(1..lenmax);
    end record;

    function wformaT(i : in wordulo64; width : in natural := 0) return string;
    procedure wput(i: wordulo64; width: in natural := 0);
    procedure swapp(a,b: in out wordulo64);

  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  --| increase++
  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    generic
      type sameType is (<>);
    procedure increaseGeneriq(i : in out sameType; exbit : in out boolean);

  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  --| decrease--
  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    generic
      type sameType is (<>);
    procedure decreaseGeneriq(i : in out sameType; exbit : in out boolean);
    pragma unreferenced(decreaseGeneriq);

    procedure set_empty(s : in out dirtyString);
    procedure set(s : in out dirtyString; to : in string);
    function get(s : in dirtyString) return string;
    function get(s : in dirtyString; dft : in wordulo64 := 0) return wordulo64;
    function get_digit(s : in dirtyString; ixpos : in natural) return integer;
    function len(s : in dirtyString) return natural;
    function nonVide(s : in dirtyString) return boolean;
    function even(n: in wordulo64 := 0) return boolean;
  --function odd(n: in wordulo64 := 0)  return boolean;
    function zshift(bw: in wordulo64; nbits: in integer) return wordulo64;
    procedure bitwiseShift(bwise : in out wordulo64;
                           nbits : in     integer;
                           exbit : in out boolean);
  --function getbit(qbit : in wordulo64; n : in integer) return boolean;
  --procedure setbit(qbit : in out wordulo64;
  --                 n :    in integer;
  --                 b :    in boolean := TRUE);
  end footwarePack;
--|-----------------------------------------------------------------------------
  package body footwarePack is
  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  --| wformaT:: w64u -> string :: pour de basse raisons couillonnes..........
  --| Je l'avais dit que j'aime pas faire confiance aux libs..................
  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function wformaT(i: in wordulo64; width: in natural := 0) return string is
      bcd        : constant string := "0123456789"; --| const base :: 10
      oox        : string(1..32)   := (others => ' ');
      value,d,ix : wordulo64 := i;
    --|
      begin --|wformaT|--
        ix := 32;
        loop
          d                := value mod 10;
          value            := value / 10;
          oox(integer(ix)) := bcd(integer(d+1));
          ix               := ix - 1;
          exit when value = 0;
        end loop;
        ix := 1 + wordulo64'MIN(32 - wordulo64(width), ix);
        return oox(integer(ix)..32);
      end wformaT;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure wput(i: wordulo64; width: in natural := 0) is
      pragma inline(wput);
      begin
        put(wformaT(i,width));
      end wput;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure swapp(a,b: in out wordulo64) is
      temp : constant wordulo64 := a; --| or tune with XOR IFF You're nerdy
      pragma inline(swapp);
      begin
        a := b;
        b := temp;
      end swapp;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure increaseGeneriq(i: in out sameType; exbit: in out boolean) is
      begin -- increase modulaire
        if i = sameType'last then
          i     := sameType'FIRST;
          exbit := TRUE;
        else
          i     := sameType'SUCC(i);
          exbit := FALSE;
        end if;
      end increaseGeneriq;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure decreaseGeneriq(i: in out sameType; exbit: in out boolean) is
      begin -- decrease modularythmetix
        if i = sameType'first then
          i     := sameType'LAST;
          exbit := TRUE;
        else
          i     := sameType'PRED(i);
          exbit := FALSE;
        end if;
      end decreaseGeneriq;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function even(n: in wordulo64 := 0) return boolean is
      pragma inline(even);
      begin
        return (n and 2#01#) = 0;
      end even;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function odd(n: in wordulo64 := 0) return boolean is
      pragma inline(odd);
      begin
        return (n and 2#01#) = 1;
      end odd;
      pragma unreferenced(odd);
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function zshift(bw: in wordulo64; nbits: in integer) return wordulo64 is
      n : aliased wordulo64 := bw;
      e : aliased boolean := FALSE; --| no data out of range usage /!\
    --|
      pragma inline(zshift);
      begin -- zshift/skip exbit/modularity "exception"/rotation /!\
        bitwiseShift(n,nbits,e);
        return n;
      end zshift;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  --| bitwise shiting relativ based on an integer [-63..63]
  --|   :: negative|positive <-> (right|left)_bitwise/shift
  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure bitwiseShift(bwise : in out wordulo64;
                           nbits : in     integer;
                           exbit : in out boolean) is
      hbit : constant wordulo64 := wordulo64(2**(wordulo64'size-1));
    --|
      pragma inline(bitwiseShift);
      begin
        if abs(nbits) < wordulo64'size then
          if nbits < 0 then
            exbit := ((bwise and 2#01#) = 2#01#);
            bwise := wordulo64(shift_right(unsigned_64(bwise),natural(-nbits)));
          elsif nbits > 0 then
            exbit := ((bwise and hbit) = hbit);
            bwise := wordulo64(shift_left(unsigned_64(bwise),natural(nbits)));
          end if;
        else
          exbit := TRUE;
        end if;
      end bitwiseShift;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function getbit(qbit : in wordulo64; n : in integer) return boolean is
      K       : aliased wordulo64;
      pow2arr : constant array (0..63) of wordulo64 :=
        ( 0 => 16#0000000000000001#,  1 => 16#0000000000000002#,
          2 => 16#0000000000000004#,  3 => 16#0000000000000008#,
          4 => 16#0000000000000010#,  5 => 16#0000000000000020#,
          6 => 16#0000000000000040#,  7 => 16#0000000000000080#,
          8 => 16#0000000000000100#,  9 => 16#0000000000000200#,
         10 => 16#0000000000000400#, 11 => 16#0000000000000800#,
         12 => 16#0000000000001000#, 13 => 16#0000000000002000#,
         14 => 16#0000000000004000#, 15 => 16#0000000000008000#,
         16 => 16#0000000000010000#, 17 => 16#0000000000020000#,
         18 => 16#0000000000040000#, 19 => 16#0000000000080000#,
         20 => 16#0000000000100000#, 21 => 16#0000000000200000#,
         22 => 16#0000000000400000#, 23 => 16#0000000000800000#,
         24 => 16#0000000001000000#, 25 => 16#0000000002000000#,
         26 => 16#0000000004000000#, 27 => 16#0000000008000000#,
         28 => 16#0000000010000000#, 29 => 16#0000000020000000#,
         30 => 16#0000000040000000#, 31 => 16#0000000080000000#,
         32 => 16#0000000100000000#, 33 => 16#0000000200000000#,
         34 => 16#0000000400000000#, 35 => 16#0000000800000000#,
         36 => 16#0000001000000000#, 37 => 16#0000002000000000#,
         38 => 16#0000004000000000#, 39 => 16#0000008000000000#,
         40 => 16#0000010000000000#, 41 => 16#0000020000000000#,
         42 => 16#0000040000000000#, 43 => 16#0000080000000000#,
         44 => 16#0000100000000000#, 45 => 16#0000200000000000#,
         46 => 16#0000400000000000#, 47 => 16#0000800000000000#,
         48 => 16#0001000000000000#, 49 => 16#0002000000000000#,
         50 => 16#0004000000000000#, 51 => 16#0008000000000000#,
         52 => 16#0010000000000000#, 53 => 16#0020000000000000#,
         54 => 16#0040000000000000#, 55 => 16#0080000000000000#,
         56 => 16#0100000000000000#, 57 => 16#0200000000000000#,
         58 => 16#0400000000000000#, 59 => 16#0800000000000000#,
         60 => 16#1000000000000000#, 61 => 16#2000000000000000#,
         62 => 16#4000000000000000#, 63 => 16#8000000000000000#);
       --| monotony suggest genericity and more... (::TODO)

      pragma inline(getbit);
      begin --|getbit|--
        if    n <  0 then return FALSE;
        elsif n > 63 then return FALSE;
        else
          K := pow2arr(n);
          return (qbit and K) = K;
        end if;
      end getbit;
      pragma unreferenced(getbit);
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure setbit(qbit : in out wordulo64;
                     n    : in integer;
                     b    : in boolean := TRUE) is
    --|
      pow2arr : constant array (0..63) of wordulo64 :=
        ( 0 => 16#0000000000000001#,  1 => 16#0000000000000002#,
          2 => 16#0000000000000004#,  3 => 16#0000000000000008#,
          4 => 16#0000000000000010#,  5 => 16#0000000000000020#,
          6 => 16#0000000000000040#,  7 => 16#0000000000000080#,
          8 => 16#0000000000000100#,  9 => 16#0000000000000200#,
         10 => 16#0000000000000400#, 11 => 16#0000000000000800#,
         12 => 16#0000000000001000#, 13 => 16#0000000000002000#,
         14 => 16#0000000000004000#, 15 => 16#0000000000008000#,
         16 => 16#0000000000010000#, 17 => 16#0000000000020000#,
         18 => 16#0000000000040000#, 19 => 16#0000000000080000#,
         20 => 16#0000000000100000#, 21 => 16#0000000000200000#,
         22 => 16#0000000000400000#, 23 => 16#0000000000800000#,
         24 => 16#0000000001000000#, 25 => 16#0000000002000000#,
         26 => 16#0000000004000000#, 27 => 16#0000000008000000#,
         28 => 16#0000000010000000#, 29 => 16#0000000020000000#,
         30 => 16#0000000040000000#, 31 => 16#0000000080000000#,
         32 => 16#0000000100000000#, 33 => 16#0000000200000000#,
         34 => 16#0000000400000000#, 35 => 16#0000000800000000#,
         36 => 16#0000001000000000#, 37 => 16#0000002000000000#,
         38 => 16#0000004000000000#, 39 => 16#0000008000000000#,
         40 => 16#0000010000000000#, 41 => 16#0000020000000000#,
         42 => 16#0000040000000000#, 43 => 16#0000080000000000#,
         44 => 16#0000100000000000#, 45 => 16#0000200000000000#,
         46 => 16#0000400000000000#, 47 => 16#0000800000000000#,
         48 => 16#0001000000000000#, 49 => 16#0002000000000000#,
         50 => 16#0004000000000000#, 51 => 16#0008000000000000#,
         52 => 16#0010000000000000#, 53 => 16#0020000000000000#,
         54 => 16#0040000000000000#, 55 => 16#0080000000000000#,
         56 => 16#0100000000000000#, 57 => 16#0200000000000000#,
         58 => 16#0400000000000000#, 59 => 16#0800000000000000#,
         60 => 16#1000000000000000#, 61 => 16#2000000000000000#,
         62 => 16#4000000000000000#, 63 => 16#8000000000000000#);
          --| monotony generated by an ada-program...
          --| (double)monotony, need to use genericity and more !!!       ::TODO

      pragma inline(setbit);
      begin --|setbit|--
        if    n <  0 then raise constraint_error with "wordulo64::setbit(...)";
        elsif n > 63 then raise constraint_error with "wordulo64::setbit(...)";
        else
          if b then
            qbit := qbit or pow2arr(n);
          else
            qbit := not qbit;
            qbit := qbit or pow2arr(n);
            qbit := not qbit;
          end if;
        end if;
      end setbit;
      pragma unreferenced(setbit);
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure set_empty(s : in out dirtyString) is
      begin
        s.length := 0;
      end set_empty;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    procedure set(s : in out dirtyString; to : in string) is
      pragma inline(set);
      begin
        if to'length > 0 then
          s.length := to'length;
          s.value(to'first..(s.length)) := to(to'first..(s.length));
        else
          s.length := 0;
        end if;
      end set;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function get(s : in dirtyString) return string is
      pragma inline(get);
      begin
        if s.length > 0 then
          return s.value(1..(s.length));
        else
          return "";
        end if;
      end get;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function get(s   : in dirtyString;
                 dft : in wordulo64 := 0) return wordulo64 is
    --| return type is part of function's signature, c/C++ ?   ...not@all < 2004
      rp,n : wordulo64 := 0;
    --|
      pragma inline(get);
      begin
        if s.length > 0 then
          rp := 0;
          for i in 1..(s.length) loop
            n := wordulo64(character'pos(s.value(i)) - character'pos('0'));
            rp := 10*rp + n;
          end loop;
          return rp;
        else
          return dft;
        end if;
      end get;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function get_digit(s : in dirtyString; ixpos : in natural) return integer is
      pragma inline(get_digit);
      begin
        if ixpos > 0 and then ixpos <= s.length then
          return integer(character'POS(s.value(ixpos)) - character'POS('0'));
        else
          return 0;
        end if;
      end get_digit;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function len(s : in dirtyString) return natural is
      pragma inline(len);
      begin
        return s.length;
      end len;
    --|_________________________________________________________________________


  --||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    function nonVide(s : in dirtyString) return boolean is
      pragma inline(nonVide);
      begin
        return (s.length > 0);
      end nonVide;
    --|_________________________________________________________________________

  end footwarePack;
  use footwarePack; --| let get the 'footwarePack' in scope till this line...
--|__________________________________________________________________________end


  procedure increase is new increaseGeneriq(wordulo64);
  pragma inline(increase);

--procedure decrease is new decreaseGeneriq(wordulo64);
--procedure decrease is new decreaseGeneriq(integer);

  procedure increase is new increaseGeneriq(integer);
  pragma inline(increase);


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| -----------------------------------------------------------
--| -- The Lowest Divisor function (LD)
--| -- Revised by @void1nside, maths'n00bygirl +o)
--| -----------------------------------------------------------
--|
--| REFS
--|   pages:        p(166)
--|   Title:        <<Programming for Mathematicians>> - 
--|   Author:       Raymond Seroul
--|   Springer[EN]: serie -UNIVERSITEXT-
--| 
--| Recherche du PLUS PETIT DIVISEUR PREMIER,
--| ----------------------------------------
--| excluant bien sur 0 :: n'est pas un diviseur, sauf pour JCVD, peut-etre?
--| excluant bien sur 1 :: N'est pas un diviseur,
--|                        puisqu'il ne DIVISE PAS operationnellement parlant
--|
--| court-cicuit sur 2  :: Le "plus impaire" des nombres premiers...
--|                     :: Two is the oddest prime number of primes... (quote)
--| 
--| Ensuite on part de 3 en sautant tout les nombres pair, car 2|pair ...
--| Ne le saviez vous point? 
--| ...c'est balot... #DrHouse
--| 
--| Une autre regle est appliquer:
--| 
--|   On sait que N, s'il a plus d'un [facteur premier],
--|   il y en a forcement un plus petit que le carre le plus petit
--|   et strictement superieur a N. Je vous laisse y reflechir.
--|
--|   Et on parle toujours STRICTEMENT d'entier.
--|   Dans cet algorithme, on ne traite aussi strictement QUE
--|   des entiers positifs. ...Z'aviez remarquez non?
--|
--| Dans MA terminologie, un "diviseur" est OPERATIONNEL. (opere un changement!)
--| En ce sens, [1='un'='one'] n'en est donc pas un|one.
--| Soit, un "diviseur" n'est operationnel QUE s'il DIVISE l'entier N (input),
--| Ceci, si et seulement si (ssi) il le "divise" sans reste Euclidien.     #MOD
--| 
--| a/ Donc, dans cet algorithme on va d'abord eliminer les "trivialites"
--| mentionnees, puis partir de 3 (ou d'un nombre impair >=3 donnez,
--| dans le cadre d'une enumeration des diviseurs...)
--| 
--| b/ Puis passer d'un nombre "temoin" (q) qui saute d'impaire en impaire,
--| de la forme [DLF::2k+1] a [DLF::2k+3]. Ceci tout en calculant le carre #SQRT
--| "sentinel" suivant, sq(2k+1) --next-> sq(2k+3) [voir dessin].
--| Soit sq(q) [ou q*q] --> Terminer si q*q >= N, et q EST le diviseur si q*q=N
--| Sinon. si q divise N (reste ZERO, mod) return(q) < q*q ...
--|
--| Representation "visuelle" du [nombre carre]
--| Ainsi que de son "suivant", puis son "isoparity".sucesseur...
--| -------------------------------------------------------------
--| Hum: "isoparity" pour "conservateur de 'parity'"
--| Rappel de lecture: [pas d'accent francophonique avec ASCII 8bits (US)]
--|
--|                 sq(n::m+1)=n**2=n^2
--|
--|    _________    sq(n::0+1)=1
--| 4 |_______  |   sq(n::1+1)=4
--| 3 |_____  | |   sq(n::2+1)=9
--| 2 |___  | | |   ...
--| 1 |_  | | | |   sq(n'succ) = (n+1)**2 = n^2 + [2n+1]
--| 0 |_|_|_|_|_|   sq(n+2)    = (n+2)**2 = n^2 + 4n+4 = n^2 + 4*[n+1]     #CORR
--|    0 1 2 3 4 .... m -->
--|
--|    Donc: sq(n+2) = sq(n)+4*[n+1]
--|    Soit, square -> square + [n<<2] + 4      :: '<<' left-shift
--|    '<<'(n,m) :: n*2^m ::- (m=2 :: 4*n)
--|
--| ----------------------------------
--| LOWEST DIVISOR [for small numbers]
--| ----------------------------------
--|_____________________________________________________________________________
  function ldq(N           : in wordulo64; --| rappel: strictement pas negatif
               continue_at : in wordulo64 := 3) return wordulo64 is
           --| continue_at is usefull to enumerate all divisor of ONE number !
    begin
      if   N < 2 then return 1; end if; --| divible           :: 1|N ==: 1
      if even(N) then return 2; end if; --| divisible par 2   :: 2|N ==: 2
      declare
        q      : aliased wordulo64 := wordulo64'MAX(3,continue_at); --|factorize
        square : aliased wordulo64 :=     0;
        exbit  : aliased boolean   := FALSE;
      begin
        if even(q) then increase(q,exbit); end if; --| si PAIR... (+1)q
        square := q * q; --| Ben oui...Paske sinon bon... Enfin voyez kwa?
        while square < n loop
          if (n mod q) = 0 then return q; end if; --| q divise n => return(q)
        --| C'est ici que se situe, la correction, rapport au Springer[sic],
        --| puisque l'on passe d'un nombre impaire a l'impaire suivant...
        --| il est juste que le carre suivant soit "deux" carres plus loin...
          square := square + zshift(q,2) + 4; --| square+=4*(q+1) ...      #deux
          q      := q + 2;
        end loop;
        if square = n then return q; end if; --| le diviseur est le carre
        if square > n then return n; end if; --| barriere du "sqrt" passee...
      end; --| yellow-submarine
      return 0;
    end ldq; -- LowestDivisor[tune_by_mon_Q]
  --|___________________________________________________________________________


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| Context d'EXECUTION / CommandeLine
--|_____________________________________________________________________________
  type mainQontext_type is record
    plist    : dirtyString(32); --| j'dit 32 comme j'dirais... moins...
    pfact    : dirtyString(32);
    pfactlen : dirtyString(32);
    pfrom    : dirtyString(32); --| range [from..to]
    pto      : dirtyString(32);
    plen     : dirtyString(32);
    qqcomb   : dirtyString(32);
    done     : boolean := FALSE;
    njump    : integer := 1;
  end record;

  procedure reset(ctx : in out mainQontext_type) is
    begin
      set_empty(ctx.plist);
      set_empty(ctx.pfact);
      set_empty(ctx.pfactlen);
      set_empty(ctx.pfrom);
      set_empty(ctx.pto);
      set_empty(ctx.plen);
      set_empty(ctx.qqcomb); --| specific to 'ce2 exercise, viet-snake'
      ctx.done  := FALSE;
      ctx.njump := 1;
    end reset;
--|_____________________________________________________________________________


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  function divise(a,b : integer) return boolean is --| boolean(a|b)
    begin
      if a = 0 then return FALSE; end if;
      return (b mod a) = 0;
    end divise;
  --|___________________________________________________________________________


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| Procedure listant des nombres premiers, partant de 2..
--|_____________________________________________________________________________
  procedure commande_listeDeNombresPremiers(ctx : in out mainQontext_type) is
    pmax  : constant wordulo64 := get(ctx.plist);
    q     : wordulo64 :=     0;
    page  : wordulo64 :=     0;
    line  : integer   :=     0;
    exbit : boolean   := FALSE;
    ftime : boolean   :=  TRUE;
    begin
      put_line("--|");
      put_line("--| Liste de nombre premiers dans [2.." & get(ctx.plist) & "]");
      put_line("--|");
      for i in 2..integer(pmax) loop
        q := wordulo64(i);
        if ldq(q) = q then
          if (page mod 10) = 0 then
            if not ftime then new_line; end if;
            ftime := FALSE;
            put("--|   #");
            put(line,width=>7);
            put(" :: ");
            increase(line,exbit); --| on s'en fou d'exbit...
            ctx.done := TRUE;
          end if;
          increase(page,exbit); --| ici aussi (Felicie)...
          put(" ");
          put(i,width=>5);
        end if;
      end loop;
      new_line;
      put_line("--|");
    end commande_listeDeNombresPremiers;


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| Procedure listant des nombres premiers dans in INTERVAL donnay...
--|_____________________________________________________________________________
  procedure commande_rangeDeNombresPremiers(ctx : in out mainQontext_type) is
    pfrom : wordulo64          := get(ctx.pfrom);
    pto   : wordulo64          := get(ctx.pto);
    plen  : constant wordulo64 := get(ctx.plen);
    q,nbp,page : wordulo64     := 0;
    line  : integer   :=     0;
    exbit : boolean   := FALSE;
    ftime : boolean   :=  TRUE;
    begin
      if pfrom > pto then swapp(pfrom,pto); end if;
      put_line("--|");
      put("--| Liste de nombre premiers dans [");
        wput(pfrom); put(".."); wput(pto);
        put("]");
        if plen > 0 then put(", limite: "); wput(plen); end if;
        new_line;
      put_line("--|");
      for i in pfrom..pto loop
        q := i;
        if ldq(q) = q then
          if (page mod 10) = 0 then
            if not ftime then new_line; end if;
            ftime := FALSE;
            put("--|   #");
            put(line,width=>7); --| Ben oui c'est de l'integer [low card]
            put(" :: ");
            increase(line,exbit); --| on s'en fou d'exbit...
            ctx.done := TRUE;
          end if;
          increase(page,exbit); --| ici aussi (Felicie)...
          put(" ");
          wput(i,width=>5);
          nbp := nbp + 1;
          exit when plen > 0 and then nbp >= plen;
        end if;
      end loop;
      new_line;
      put_line("--|");
    end commande_rangeDeNombresPremiers;


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| Procedure donnant des factorizations de nombres quelquonques...
--|_____________________________________________________________________________
  procedure commande_factorizationDeNombres(ctx : in out mainQontext_type) is
    arrivee          : constant wordulo64 := get(ctx.pfact,10000);
    pfactlen         : constant wordulo64 := get(ctx.pfactlen,200);
    n,q,qLast,depart :          wordulo64 := 2;
    produit,i        :          wordulo64 := 1;
    exbit            :          boolean   := FALSE;
    begin
      if arrivee > pfactlen then depart := arrivee - pfactlen + 1; end if;
      put_line("--|");
      put("--| Factorization des nombres dans [");
      wput(depart,width=>0);
      put_line(".." & get(ctx.pfact) & "]");
      put_line("--|");
      q := 0;
      i := depart;
      while i <= arrivee loop
--for i in integer(depart)..integer(arrivee) loop :: kunt: card(integer)  #SUCKS
        n := i;
        put("--| ");
        wput(i,width=>10);
        q := ldq(n);
        if n = q then
          put_line(" is PRIME                                       <-- PRIME");
          ctx.done := TRUE;
        else
          put(" = ");
          produit := 1;
          while n > 1 loop
            wput(q,width=>0);
            n   := n / q;
            produit := produit * q;
            if n > 1 then put("*"); end if;
            qLast := q;
            q     := ldq(n,qLast);
          end loop;
          ctx.done := TRUE;
          pragma assert(
            produit=i,
            "le produit n'est pas correcte pour" & wordulo64'image(i) &
            "...");
          new_line;
        end if;
        increase(i,exbit); exit when exbit; --| Ben sinon .... heu... MegaBitoys
      end loop;
      put_line("--|");
    end commande_factorizationDeNombres;
  --|___________________________________________________________________________


  function qqce2_check(a,b,c,d,e,f,g,h,q : in natural) return boolean is
    uu,vv : natural := 0;
  --|
    pragma inline(qqce2_check);
    begin
      if not divise(q,g)   then return FALSE; end if; --| tune q|13g == q|g ctx
      if not divise(a,b*c) then return FALSE; end if;
      uu := 87 + ( d - f - h );
      vv := 12*e + c*b/a + 13*g/q;
      return uu = vv;
    end qqce2_check;


  procedure ppforce is
    a,b,c,d,e,f,g,h,q : natural := 0;
    w66               : integer := 0;
    xperm : dirtyString(10);
  --|
    procedure x_permutation(ixA,ixB,ixC,ixQ,ixG : in natural) is
      begin
        for ixD in 1..9 loop
          if ixD /= ixA and then
             ixD /= ixB and then
             ixD /= ixC and then
             ixD /= ixQ and then
             ixD /= ixG then
            d := get_digit(xperm,ixD);
            for ixE in 1..9 loop
              if ixE /= ixA and then
                 ixE /= ixB and then
                 ixE /= ixC and then
                 ixE /= ixD and then
                 ixE /= ixQ and then
                 ixE /= ixG then
                e := get_digit(xperm,ixE);
                for ixF in 1..9 loop
                  if ixF /= ixA and then
                     ixF /= ixB and then
                     ixF /= ixC and then
                     ixF /= ixD and then
                     ixF /= ixE and then
                     ixF /= ixG and then
                     ixF /= ixQ then
                    f := get_digit(xperm,ixF);
                    for ixH in 1..9 loop
                      if ixH /= ixA and then
                         ixH /= ixB and then
                         ixH /= ixC and then
                         ixH /= ixD and then
                         ixH /= ixE and then
                         ixH /= ixF and then
                         ixH /= ixG and then
                         ixH /= ixQ then
                        h := get_digit(xperm,ixH);
                        if qqce2_check(a,b,c,d,e,f,g,h,q) then
                          w66 := h + 13*g/q + f + 12*e - d - 11 + c*b/a - 10;
                          new_line;
                          put_line("--| solution:"
                            & natural'image(h)   & " + 13*"
                            & natural'image(g)   & ":"
                            & natural'image(q)   & " +"
                            & natural'image(f)   & " + 12*"
                            & natural'image(e)   & " -"
                            & natural'image(d)   & " - 11 +"
                            & natural'image(c)   & "*"
                            & natural'image(b)   & ":"
                            & natural'image(a)   & " - 10 ="
                            & integer'image(w66) & " [eos]");
                          --|
                        end if;
                      end if;
                    end loop;
                  end if;
                end loop;
              end if;
            end loop;
          end if;
        end loop;
      end x_permutation;
  --|
    procedure qg_permutation(ixQ,ixG : in natural) is
      begin
        for ixA in 1..9 loop
          if ixA /= ixQ and then ixA /= ixG then
            a := get_digit(xperm,ixA);
            for ixB in 1..9 loop
              if ixB /= ixA and then ixB /= ixQ and then ixB /= ixG then
                b := get_digit(xperm,ixB);
                if divise(a,b) then
                  for ixC in 1..9 loop
                    if ixC /= ixA and then
                       ixC /= ixB and then
                       ixC /= ixQ and then
                       ixC /= ixG then
                      c := get_digit(xperm,ixC);
                      x_permutation(ixA,ixB,ixC,ixQ,ixG);
                    end if;
                  end loop;
                else
                  for ixC in 1..9 loop
                    if ixC /= ixA and then
                       ixC /= ixB and then
                       ixC /= ixQ and then
                       ixC /= ixG then
                      if divise(a,c) then
                        c := get_digit(xperm,ixC);
                        x_permutation(ixA,ixB,ixC,ixQ,ixG);
                      end if;
                    end if;
                  end loop;
                end if;
              end if;
            end loop;
          end if;
        end loop;
      end qg_permutation;
  --|
    begin
      set(xperm,"123456789");
      for ixG in 1..9 loop
        g := get_digit(xperm,ixG);
        for ixQ in 1..9 loop
          if ixQ /= ixG then
            q := get_digit(xperm,ixQ);
            if divise(q,g) then --| == divise(q|13*g) in ctx::[1..9]
              put("--| qg" & natural'image(q) & "|" & natural'image(g));
              qg_permutation(ixQ,ixG); --| q and g are compatible
              new_line;
              new_line;
            end if;
          end if;
        end loop;
      end loop;
      put_line("[FB-END]");
    end ppforce;


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  procedure commande_qqce2Validator(ctx : in out mainQontext_type) is
    a,b,c,d,e,f,g,h,q : integer := 0;
    uu,vv             : integer := 0;
  --|
    begin
      put_line("--|qqce2|--TP('" & get(ctx.qqcomb) & "')");
      if len(ctx.qqcomb) = 9 then
        d := get_digit(ctx.qqcomb,1);
        f := get_digit(ctx.qqcomb,2);
        h := get_digit(ctx.qqcomb,3);
        e := get_digit(ctx.qqcomb,4);
        c := get_digit(ctx.qqcomb,5);
        b := get_digit(ctx.qqcomb,6);
        g := get_digit(ctx.qqcomb,7);
        a := get_digit(ctx.qqcomb,8); --| keep divs on tail
        q := get_digit(ctx.qqcomb,9); --| .................
        put_line("--| +(dfh) e cbg : aq |= cb/a and 13g/q");
        uu := 66 + 10 + 11 + ( d - f - h ); --| 87 + ...
        put_line("--| uu = " & integer'image(uu));
        if not divise(a,c*b)  then put_line("--| FALSE <- a|cb");  end if;
        if not divise(q,13*g) then put_line("--| FALSE <- q|13g"); end if;
        vv := 12*e + c*b/a + 13*g/q;
        put_line("--| vv = " & integer'image(vv));
        if uu = vv then
          put_line("--| semi(TRUE), check divisibility...");
        end if;
      else
        put_line("--*|-- brut force...");
        ppforce;
      end if;
      ctx.done := TRUE;
    end commande_qqce2Validator;
  --|___________________________________________________________________________



--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|| (q)MAIN
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  begin
    declare
      ctx : aliased mainQontext_type;
    begin
      reset(ctx);
      for n in 1..argument_count loop
        if ctx.njump > 1 then
          ctx.njump := ctx.njump - 1;
        elsif argument(n) = "--plist" then
          if n < argument_count then
            set(ctx.plist,argument(n+1)); --| parm "--plist 1729" egzampeulle...
          end if;
        elsif argument(n) = "--tp00" and then argument_count > n then
          set(ctx.qqcomb,argument(n+1));
          ctx.njump := 2;
        elsif argument(n) = "--pfact" then
          if n < argument_count then
            set(ctx.pfact,argument(n+1));
          end if;
        elsif argument(n) = "--pfactlen" then
          if n < argument_count then
            set(ctx.pfactlen,argument(n+1));
          end if;
        elsif argument(n) = "--pmax" then
          if n < argument_count then
            set(ctx.plen,argument(n+1));
          end if;
        elsif argument(n) = "--prange" then
          if n < argument_count then
            set(ctx.pfrom,argument(n+1));
          end if;
          if n < argument_count+1 then
            set(ctx.pto,argument(n+2));
          end if;
        end if;
      end loop;
    --|_________________________________________________________________________
      if nonVide(ctx.qqcomb) then commande_qqce2Validator(ctx); end if;
      if nonVide(ctx.pfact)  then commande_factorizationDeNombres(ctx); end if;
      if nonVide(ctx.plist)  then commande_listeDeNombresPremiers(ctx); end if;
      if nonVide(ctx.pto)    then commande_rangeDeNombresPremiers(ctx); end if;
      if not ctx.done then
        put_line("--|");
        put_line("--| Need parms...");
        put_line("--|");
        put_line("--|   --plist N :: Liste des nb premiers de 2..N");
        put_line("--|   --prange N M :: Liste des nb premiers in [N..M]");
        put_line("--|   --pmax U :: Fix un max de nb premiers a afficher");
        put_line("--|   --pfact N :: Factorization de 2..N");
        put_line("--|   --pfactlen L :: Factorize de N-L+1..N, si --pfact");
        put_line("--|");
      end if;
    end; --| declare
  end qqce2;
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| @void1nside
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| annotated/ada++
--| signa11: @void1nside .ada-arythmeticall-selfstudy |||||||||||| #GNAT/ADA2005
--| F00T-SOURCE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||END
