
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|source: lowestdivisors.adb :: [ADA] *flat *ascii *file
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|__________
--|_____/\  /| o o1o o oo1-1o ooooo oo o o o o o o oo oo oo ooo ooooo ooooooo oq
--|____/__\/=|   ooo o ______ o:oo:oo o o o o_o o o__o  o  o   o____o o ooo o dp
--|___/   /\_|
--|      /   |
--|          |__|||_||_\/||_|p___________________________________annotated/ada++
--|
--|            _________________________________________________________________
--| Title:     LDQ :: Lowest Divisor of a Natural
--|            Non probabilist ...
--|            |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|
--| plateform: Linux 2.6.38 #1 SMP EDT 2011 i686 GNU/Linux Backtrack 5 10.04 LTS
--| $H4rd:     ASUS eeePC, ASUS eeeBox --no more details @FOYK --
--| $7ypical:  gnatmake -Wall -gnat05 -gnataif lowestdivisors.adb
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
procedure lowestDivisors is
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

    procedure set(s : in out dirtyString; to : in string);
    function get(s : in dirtyString) return string;
    function get(s : in dirtyString; dft : in wordulo64 := 0) return wordulo64;
  --function len(s : in dirtyString) return natural;
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
    function len(s : in dirtyString) return natural is
      pragma inline(len);
      begin
        return s.length;
      end len;
      pragma unreferenced(len); --| Sooooo... all that unreferenced are ludixxx?
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
  type main_exeQontextType is record
    plist    : dirtyString(32); --| j'dit 32 comme j'dirais... moins...
    pfact    : dirtyString(32);
    pfactlen : dirtyString(32);
    pfrom    : dirtyString(32); --| range [from..to]
    pto      : dirtyString(32);
    plen     : dirtyString(32);
    done     : boolean := FALSE;
  end record;
--|_____________________________________________________________________________


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| Procedure listant des nombres premiers, partant de 2..
--|_____________________________________________________________________________
  procedure commande_listeDeNombresPremiers(ctx : in out main_exeQontextType) is
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
  procedure commande_rangeDeNombresPremiers(ctx : in out main_exeQontextType) is
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
  procedure commande_factorizationDeNombres(ctx : in out main_exeQontextType) is
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



--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|| (q)MAIN
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  begin
    declare
      ctx : aliased main_exeQontextType;
    begin
      for n in 1..argument_count loop
        if argument(n) = "--plist" then
          if n < argument_count then
            set(ctx.plist,argument(n+1)); --| parm "--plist 1729" egzampeulle...
          end if;
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
      if nonVide(ctx.pfact) then commande_factorizationDeNombres(ctx); end if;
      if nonVide(ctx.plist) then commande_listeDeNombresPremiers(ctx); end if;
      if nonVide(ctx.pto)   then commande_rangeDeNombresPremiers(ctx); end if;
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
  end lowestDivisors;
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| @void1nside
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||| annotated/ada++
--| signa11: @void1nside .ada-arythmeticall-selfstudy |||||||||||| #GNAT/ADA2005
--| F00T-SOURCE ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||END





--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||some usages
--| EXECUTION version intermediaire...
--|
--| $ gnatmake -Wall -gnat05 -gnataif lowestdivisors.adb
--|   gcc -c -Wall -gnat05 -gnataif lowestdivisors.adb
--|   lowestdivisors.adb:42:09: warning: unit "command_line" is not referenced
--|   gnatbind -x lowestdivisors.ali
--|   gnatlink lowestdivisors.ali

--| extrait(s) ...

--| .begin
--|   #      0 ::      2     3     5     7    11    13    17    19    23    29
--|   #      1 ::     31    37    41    43    47    53    59    61    67    71
--|   #      2 ::     73    79    83    89    97   101   103   107   109   113
--|   #      3 ::    127   131   137   139   149   151   157   163   167   173
--|   #      4 ::    179   181   191   193   197   199   211   223   227   229
--|   #      5 ::    233   239   241   251   257   263   269   271   277   281
--|   #      6 ::    283   293   307   311   313   317   331   337   347   349
--|   #      7 ::    353   359   367   373   379   383   389   397   401   409
--|   #      8 ::    419   421   431   433   439   443   449   457   461   463
--|   #      9 ::    467   479   487   491   499   503   509   521   523   541
--|   #     10 ::    547   557   563   569   571   577   587   593   599   601
--|   #     11 ::    607   613   617   619   631   641   643   647   653   659
--|   #     12 ::    661   673   677   683   691   701   709   719   727   733
--|   #     13 ::    739   743   751   757   761   769   773   787   797   809
--|   #     14 ::    811   821   823   827   829   839   853   857   859   863
--|   #     15 ::    877   881   883   887   907   911   919   929   937   941
--|   #     16 ::    947   953   967   971   977   983   991   997  1009  1013
--|   #     17 ::   1019  1021  1031  1033  1039  1049  1051  1061  1063  1069
--|   #     18 ::   1087  1091  1093  1097  1103  1109  1117  1123  1129  1151
--|   #     19 ::   1153  1163  1171  1181  1187  1193  1201  1213  1217  1223
--|   #     20 ::   1229  1231  1237  1249  1259  1277  1279  1283  1289  1291
--|   #     21 ::   1297  1301  1303  1307  1319  1321  1327  1361  1367  1373
--|   #     22 ::   1381  1399  1409  1423  1427  1429  1433  1439  1447  1451
--|   #     23 ::   1453  1459  1471  1481  1483  1487  1489  1493  1499  1511
--|   #     24 ::   1523  1531  1543  1549  1553  1559  1567  1571  1579  1583
--|   #     25 ::   1597  1601  1607  1609  1613  1619  1621  1627  1637  1657
--|   #     26 ::   1663  1667  1669  1693  1697  1699  1709  1721  1723  1733
--|   #     27 ::   1741  1747  1753  1759  1777  1783  1787  1789  1801  1811
--|   #     28 ::   1823  1831  1847  1861  1867  1871  1873  1877  1879  1889
--|   #     29 ::   1901  1907  1913  1931  1933  1949  1951  1973  1979  1987
--|   #     30 ::   1993  1997  1999  2003  2011  2017  2027  2029  2039  2053
--|   ...etc...


--|   # 7844 ::  999091 999101 999133 999149 999169 999181 999199 999217
--|_             999221 999233
--|   # 7845 ::  999239 999269 999287 999307 999329 999331 999359 999371
--|_             999377 999389
--|   # 7846 ::  999431 999433 999437 999451 999491 999499 999521 999529
--|_             999541 999553
--|   # 7847 ::  999563 999599 999611 999613 999623 999631 999653 999667
--|_             999671 999683
--|   # 7848 ::  999721 999727 999749 999763 999769 999773 999809 999853
--|_             999863 999883
--|   # 7849 ::  999907 999917 999931 999953 999959 999961 999979 999983
--| .end
--|_____________________________________________________________________________

--| EXECUTION A VIDE (help) ...

--|
--| Need parms...
--|
--|   --plist N :: Liste des nb premiers de 2..N
--|   --prange N M :: Liste des nb premiers in [N..M]
--|   --pmax U :: Fix un max de nb premiers a afficher
--|   --pfact N :: Factorization de 2..N
--|   --pfactlen L :: Factorize de N-L+1..N, si --pfact
--|_____________________________________________________________________________


--| $ gnatmake -Wall -gnat05 -gnataif lowestdivisors.adb
--|   gcc -c -Wall -gnat05 -gnataif lowestdivisors.adb
--|   gnatbind -x lowestdivisors.ali
--|   gnatlink lowestdivisors.ali
--|
--| $ ./lowestdivisors --pfact 5979999991 --pfactlen 30 \
--|        --prange 7979999991 8979999991 --pmax 50


--|
--| Factorization des nombres dans [5979999962..5979999991]
--|
--| 5979999962 = 2*2989999981
--| 5979999963 = 3*7*284761903
--| 5979999964 = 2*2*53*28207547
--| 5979999965 = 5*109*10972477
--| 5979999966 = 2*3*491*2029871
--| 5979999967 = 521*11477927
--| 5979999968 = 2*2*2*2*2*17*10992647
--| 5979999969 = 3*3*664444441
--| 5979999970 = 2*5*7*85428571
--| 5979999971 = 11*1931*281531
--| 5979999972 = 2*2*3*149*3344519
--| 5979999973 = 229*26113537
--| 5979999974 = 2*13*47*4893617
--| 5979999975 = 3*5*5*31*2572043
--| 5979999976 = 2*2*2*12799*58403
--| 5979999977 = 7*23*37*67*14983
--| 5979999978 = 2*3*3*41*127*63803
--| 5979999979 = 19*89*3536369
--| 5979999980 = 2*2*5*298999999
--| 5979999981 = 3*43*593*78173
--| 5979999982 = 2*11*271818181
--| 5979999983 is PRIME                                       <-- PRIME
--| 5979999984 = 2*2*2*2*3*7*7*29*73*1201
--| 5979999985 = 5*17*2797*25153
--| 5979999986 = 2*283*10565371
--| 5979999987 = 3*3*3*13*17037037
--| 5979999988 = 2*2*59*233*108751
--| 5979999989 = 12413*481753
--| 5979999990 = 2*3*5*199333333
--| 5979999991 = 7*383*2230511
--|
--|
--| Liste de nombre premiers dans [7979999991..8979999991], limite: 50
--|
--|   # 0 ::  7979999999 7980000031 7980000043 7980000103 7980000181
--|_          7980000241 7980000251 7980000257 7980000269 7980000299
--|   # 1 ::  7980000313 7980000317 7980000319 7980000331 7980000349
--|_          7980000367 7980000403 7980000461 7980000463 7980000467
--|   # 2 ::  7980000487 7980000527 7980000547 7980000571 7980000619
--|           7980000631 7980000643 7980000671 7980000689 7980000701
--|   # 3 ::  7980000739 7980000751 7980000781 7980000803 7980000827
--|_          7980000839 7980000841 7980000869 7980000871 7980000883
--|   # 4 ::  7980000953 7980000983 7980000989 7980001027 7980001033
--|_          7980001063 7980001091 7980001097 7980001111 7980001129
--|
