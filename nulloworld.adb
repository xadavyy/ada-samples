
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| NULL WORLD program brainstormed in ADA/GNAT from adacore.com
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| H0000 |-unlesson nr0
--|_____________________________________________________________________________
--| nick: adaPGML0VR
--| mapp: {@ada|gnat}-x0x0v3rym
--|
pragma ada_2005; --| means The Compiler have to understand rules <= ADA2005...
pragma license(Unrestricted); --| means licence WITHOUT Restriction (even GPL)
--|
procedure nullOworld is --| procedure-name does match filename (nulloworld.adb)
--|
  begin
    null; --| ... 'null' means "no operation here..."
  end nullOworld;
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||END

--|
--| names map(X): icase(procpack_Name) -> lowcase(filename)
--|   so the map(X) is surjective // epimorph
--|   don't care about this...
--|
--| Hum... {{Art_of_unsaying things}}
--|
--| NB: Any line begin with '--' are just COMMENTS (not compile-pertinent)
--|
