
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
procedure wx0001 is
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||declaratives
  use ada.text_IO;
  package intFMT renames ada.integer_text_IO;
--|
  type word_bounds is  mod 64; --| 2**6 --> keep 8 as standard word size
  for  word_bounds'SIZE use 8;
  word_Cardinal : constant natural := 1+natural(word_bounds'LAST); --| 64
--|
  type bitwiseWord_type is mod 2**word_Cardinal;
  for  bitwiseWord_type'SIZE use  word_Cardinal;
--| :: size -> range[0..size-1]
  type bitwiseWord_bounds is new natural range 0..(bitwiseWord_type'SIZE-1);
--|_____________________________________________________________________________

  function sizeof(m : in bitwiseWord_type) return natural is
    begin
      return m'SIZE / 8; --| ((/ 8) SIZE::[bit]) -> sizeof::[(FR)Byte:==octet]
    end sizeof;

  procedure pput(msg : in string; i : in natural; unity : string := "1") is
    begin
      put_line("  " & msg & " ->" & natural'image(i) & "[" & unity & "]");
    end pput;

--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||ioContext
  type ioContext_type is record --| not private Yet...
    initialised  : boolean := FALSE; --| IFF bivalued, boolean is sufficient...
    ix_item      : natural := 0;
    const_eqLine : string(1..75) := (others => '='); --| cant be constant here
  end record;

--| local in this structure "mean"...
  procedure push_RESET(ctx : in out ioContext_type) is
    begin
      if not ctx.initialised then
        ctx.const_eqLine := (others => '=');
        ctx.initialised  := TRUE; --| Ghost In The Fred...
        ctx.ix_item      := 0;    --| obviously maybe, like a SONG...
      end if;
      put_line(ctx.const_eqLine);  --| Hardcoded, gess next...
    end push_RESET;

  procedure push_FLUSH(ctx : in out ioContext_type) is
    begin
      if ctx.initialised then
        ctx.initialised := FALSE;   --| minimalist mind of spirit ::StLouis
        put_line(ctx.const_eqLine); --| ...guess wtf...
      end if;
    end push_FLUSH;

  procedure push(ctx : in out ioContext_type; ix : in natural := 0) is
  --| actualy HARDLY "hardcoded", gess next...
    begin --| (non external parms) 16 -> 0 -> 15
      if not ctx.initialised then push_reset(ctx); end if;
      if (ctx.ix_item mod 16) = 0 then put("  --> "); end if;
      put(":"); intFMT.put(integer(ix),width=>3);      --| little change gess...
      if (ctx.ix_item mod 16) = 15 then put_line(" *EOL"); end if;
      ctx.ix_item := natural'SUCC(ctx.ix_item);        --| increase 1
    end push;
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||ioContext::END


--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--| MAIN |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  w   : constant bitwiseWord_type := 0;
  ctx : ioContext_type; --| takes initial values of native typed components...
--||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
  begin
    pput("(.first)word_bounds",natural(word_bounds'FIRST));
    pput("(.cardinal)word_bounds",natural(word_bounds'LAST));
    pput("(.value)word_bounds",word_Cardinal);
    pput("(.sizeof)bitwiseWord_type",sizeof(w),"Byte");
    for ix in bitwiseWord_bounds'range loop push(ctx,natural(ix)); end loop;
    push_FLUSH(ctx);
  end wx0001;
--|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||END
