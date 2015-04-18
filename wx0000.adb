
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| voidProgramming
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--|
pragma ADA_2005;
pragma license(Unrestricted);
--|
with ada.text_IO;
with ada.integer_text_IO;
--|
procedure wx0000 is
  use ada.text_IO;
  package intFMT renames ada.integer_text_IO;
--|
  type bitwiseWord_type is mod 2**64;
  for  bitwiseWord_type'SIZE use  64;
--| :: size -> range[0..size-1]
  type bitwiseWord_bounds is new natural range 0..(bitwiseWord_type'SIZE-1);


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| MAIN |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  const_eqLine : constant string(1..75) := (others => '=');
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  begin
    put_line(const_eqLine);
    for ix in bitwiseWord_bounds'range loop
      if (ix mod 16) = 0 then put("  --> "); end if;
    --put(":" & bitwiseWord_bounds'image(ix));
      put(": "); intFMT.put(integer(ix),width=>2);
      if (ix mod 16) = 15 then put_line(" *EOL"); end if;
    end loop;
    put_line(const_eqLine);
  end wx0000;
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||END
