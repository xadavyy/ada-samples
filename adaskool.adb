
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| ADA c'est c00L quand on connay (adaskool.adb)
--| [en] ADA is cool when we does'nt waste time to avoid knowing it...
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|
pragma ada_2005; --| means The Compiler have to understand rules <= ADA2005...
pragma license(Unrestricted); --| means licence WITHOUT Restriction (even GPL)
--|
with ada.text_IO; --| declare we wanna use this package 'ada.text_IO'
--|                 | Don't worry about case sensitivity 'ADA'=='ada'
procedure adasKool is
  use ada.text_IO; --| USING a NON PURE ada.package (side effect)
                   --| side effect <-- referential transparency

  type my_type is mod 10; --| integer in [0..9] using mod-processor acceleration
  for  my_type'SIZE use 4; --|bits|--

  function foo(i : in my_type) return string is
    pragma inline(foo); --| Callme back about c/C++ inlining...
    begin
      return natural'image(1000 * natural(i) + 42);
    end foo;
    pragma pure_function(foo); --| Normally, compiling warn about side_effexx...

--|
  begin
    put_line("DEBUT");
    for i in my_type'RANGE loop
      put_line("  i:" & my_type'image(i) & " -> " & foo(i));
    end loop;
    put_line("FIN");
  end adasKool; --> Guess what's The "output" @exe ./adaskool
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||END
