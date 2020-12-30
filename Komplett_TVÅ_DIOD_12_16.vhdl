-- Charlie Emil Shabir Daniel 16 december 14:26

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;

Entity IR_sender is 
	Port ( CLK100MHZ : in STD_LOGIC;
	       CPU_RESETN: in STD_LOGIC; -- Playerselect
	       BTNC : in STD_LOGIC; -- Sender, ShotSound
	       BTNU : in STD_LOGIC; --Playerselect
               BTND : in STD_LOGIC; --Playerselect
               BTNL : in STD_LOGIC; -- send kill-data on UART
	       Seg : out STD_LOGIC_VECTOR (7 downto 0); --Playerselect
	       AN : out STD_LOGIC_VECTOR (7 downto 0); --Playerselect
	       Port1 : out STD_LOGIC; -- Sender
	       Reciever1 : in STD_LOGIC; -- Reciever1
		Reciever2 : in STD_LOGIC; -- Reciever2
	       UART_port : out STD_LOGIC;
	       UART_RXD_OUT : out STD_LOGIC;
	       Speakerport : out STD_LOGIC;-- Speaker
	       Kill_Light : out STD_LOGIC;
	       UART_LED : out STD_LOGIC;
	       JC : out STD_LOGIC_VECTOR(7 downto 0)); -- Reciever (debug)
	       
end IR_sender;

Architecture behavioural of IR_sender is
constant IR_carrier_delay      : INTEGER := 100000000/38000/2;--38kHz 
constant IR_startBit_delay 	: INTEGER := 3*100000000/1000;--Längden på startbiten
constant IR_high_delay 		: INTEGER := 2*100000000/1000;--Längden på en etta
constant IR_low_delay 		: INTEGER := 1*100000000/1000;--Längden på en nolla
constant IR_paus_delay 		: INTEGER := 1*100000000/1000;--Längden på pausen mellan bitarna
--constant IR_packet : UNSIGNED(7 downto 0) := "00101010"; --Spelar id
signal IR_packet : UNSIGNED(7 downto 0) := "00000000"; --Spelar id

signal Death : STD_LOGIC := '0';--Är 1 vid träff och återställs efter Träffdelay
signal SpeakerOutshot : STD_LOGIC; -- Speaker
signal SpeakerOutdmg : STD_LOGIC; --Speaker
signal KillList : STD_LOGIC_VECTOR(63 downto 0);--Lagring för träffar

signal UART_clock : STD_LOGIC := '0'; --debug;

signal HIT : STD_LOGIC := '0';

signal dig1 : STD_LOGIC_VECTOR (3 downto 0) := "0000";--Display
signal dig2 : STD_LOGIC_VECTOR (2 downto 0) := "111";--Display

begin

Sender:process(CLK100MHZ)

variable IR_carrier_counter : INTEGER := 0;
variable IR_virtual_port : UNSIGNED(0 downto 0) := "0";
variable IR_send : UNSIGNED(0 downto 0) := "0";
variable IR_state : INTEGER := -1; -- 0->startBit, 1->sendbit, 2->
variable IR_send_counter : INTEGER := 0;
variable IR_send_delay : INTEGER := IR_startBit_delay;

begin

if rising_edge(CLK100MHZ) then

	if BTNC = '1' AND Death = '0' then --Avtryckarn trycks ner och skott skjuts
	
		if IR_state = -1 then -- start bit
			IR_send_delay := IR_startBit_delay;
			IR_send := "1";
		elsif IR_state mod 2 = 0 then -- data bits
			IR_send_delay := IR_paus_delay;
			IR_send := "0";
		else
			if IR_packet(IR_state/2) = '1' then
				IR_send_delay := IR_high_delay;
			else
				IR_send_delay := IR_low_delay;
			end if;
			IR_send := "1";
		end if;

		if IR_send_counter > IR_send_delay then
			IR_state := IR_state +1;
			IR_send_counter := 0;
			if IR_state > 16 then -- if all bits are sent, trap it in paus loop;
				IR_state := 16;
			end if;
		else
			IR_send_counter := IR_send_counter +1;
		end if;
	else
		IR_send_counter := 0;
		IR_state := -1;
		IR_send := "0";
	end if;

	if IR_send = "1" then --IR FREQ. GENERATOR f = 38kHz
		if IR_carrier_counter >IR_carrier_delay then
			IR_carrier_counter := 0;
			IR_virtual_port := NOT IR_virtual_port;
	
		else 
			IR_carrier_counter := IR_carrier_counter +1;
			Port1 <= std_logic(IR_virtual_port(0));
		end if;
	else 
		Port1 <= '0';
		IR_virtual_port := "0";
	end if;
end if;
end process;


Reciever:process(CLK100MHZ, killList)

variable reciever_startbitHigh_count : INTEGER := (IR_startBit_delay + IR_high_delay)/2;
variable reciever_highLow_count : INTEGER := (IR_high_delay + IR_low_delay)/2;
variable reciever_LowNull_count : INTEGER := (IR_low_delay)/2;
variable reciever_packet : UNSIGNED(7 downto 0) := "00000000";
variable reciever_temp_packet : UNSIGNED(7 downto 0) := "00000000";
variable reciever_counter : INTEGER := 0;
variable reciever_state : INTEGER := -1;
variable Index : INTEGER := 0;

variable deathcounter : INTEGER := 0;
variable deathThreshold : INTEGER := 1000000000; -- Death=1 i 30*100 000 000 klockcykler, alltså 30sekunder.
variable dekad: Integer := 0;
variable dekadThreshold : Integer := 0;

begin

if rising_edge(CLK100MHZ) then

	if HIT = '0' then
		dekadThreshold := 3;
	elsif HIT = '1' then
		dekadThreshold := 6;
	end if;


	if Death > '0' then
		if dekad = dekadThreshold then -- När dekad =3 har 30s gått.
			Death <= '0';
		end if;	
		if deathcounter > deathThreshold then
			deathcounter := 0;
			dekad := dekad + 1; -- Dekad inkrementeras var 10e sekund. deathThreshold kunde inte vara hur stor som helst. 
		else
			deathcounter := deathcounter +1;
		end if;
	end if;
	JC <= STD_LOGIC_VECTOR(reciever_packet);
	if Reciever1 = '0' OR Reciever2 = '0'  then 
		reciever_counter := reciever_counter +1;

	elsif reciever_counter > reciever_LowNull_count then -- förhindra noll count loop

		if reciever_counter > reciever_startbitHigh_count then -- startbit funnen!
			reciever_state := 0;
			reciever_counter := 0;
		elsif reciever_state > -1 then -- kollar korrekt state för data

			if reciever_counter > reciever_highLow_count then -- 1:a funnen
				reciever_temp_packet(reciever_state) := '1';
				reciever_counter := 0;
			else -- 0:a funnen
				reciever_temp_packet(reciever_state) := '0';
				reciever_counter := 0;
			end if;

			reciever_state := reciever_state +1;
			if reciever_state > 7 then -- om all data är funnen, gå tillbaka
				reciever_packet := reciever_temp_packet;
				reciever_state := -1; 
				if reciever_packet(3)/=IR_packet(3) AND Death='0' AND reciever_packet < 16 then
				    Death <= '1'; -- du dör :'-(
				    dekad := 0;
				    -- Killist (64 bitar) lagrar info om träffar (8 bitar per motståndare och 8 motståndare). 
                    Index := 8*to_integer(reciever_packet(2 downto 0)); 
                    if HIT='0' then
                        if killList(Index +3 downto Index) < "1001" then
                            killList(Index +3 downto Index) <= killList(Index +3 downto Index) +1;
                        else
                            killList(Index +7 downto Index +4) <= killList(Index +7 downto Index +4) +1;
                            killList(Index +3 downto Index) <= "0000";
                        end if;
                    elsif HIT='1' then
                        if killList(Index +3 downto Index) < "1001" then
                            killList(Index +3 downto Index) <= killList(Index +3 downto Index) +2;
                        else
                            killList(Index +7 downto Index +4) <= killList(Index +7 downto Index +4) +2;
                            killList(Index +3 downto Index) <= "0000";
                        end if;
                    end if;
                                        --killList(Index+7 downto Index) <= killList(Index+7 downto Index) + '1';
				   
			    end if;
			end if;
		end if;
	--else 
	   	 -- om icke-definerad puls funnen, reset och leta efter ny starbit;
	end if;
end if;
end process;

HitChoice:process(CLK100MHZ)

begin
if rising_edge(CLK100MHZ) then
    if Reciever1 = '0' AND Reciever2 = '1' then
	   HIT <= '0';
    elsif Reciever2 = '0' then
	   HIT <= '1';
--else HIT <= '0';
    end if;

end if;

end process;



ShotSound:process(CLK100MHZ)
variable counter : UNSIGNED(17 downto 0) := "000000000000000000"; -- En 18-bitars counter
variable clkdividersvep : INTEGER := 100000000/4000/2; --2kHz - signal
variable clkdivider	  : INTEGER := 100000000/4000/2; --2kHz - signal
variable SP : STD_LOGIC := '0';--Behövs då vi inte kan använda outputen direkt, som ett mellansteg

begin
if rising_edge(CLK100MHZ) then
    if BTNC='0' AND (clkdivider > 49999 OR clkdivider = clkdividersvep) then-- Som en reset, tryck på BTNC så låter högtalaren. Då clkdivider når 50000 är signalen klar.
	   counter := "000000000000000000"; -- Countern resetas och räknar ej.
	   clkdivider := clkdividersvep;
	   SP := '0';
    else
	   if to_integer(counter) = clkdivider then -- När räknaren når clkdivider så switchas SP och countern nollställs.
		  SP := NOT SP;-- SP byter tecken vilket i sin tur kommer byta tecken på utsignalen till Port1.
		  
		  counter := "000000000000000000";
		  clkdivider := clkdivider + 100;
	   elsif clkdivider < 50000 then -- countern räknar så länge clkdivider är mindre än 50000
	       counter := counter + 1;
	   else
	       counter := "000000000000000000";
	   end if; 
    end if;
    SpeakerOutshot <= SP;
end if;
end process;

DeathSound:process(CLK100MHZ)
variable percountD : INTEGER := 0;
variable counterD : UNSIGNED(17 downto 0) := "000000000000000000"; -- En 18-bitars counter
variable SPD : STD_LOGIC := '0';
variable clkdividerdmg : INTEGER := 100000000/200/2;

begin
if rising_edge(CLK100MHZ) then
	if Death='0' AND (percountD > 150 OR percountD = 0) then--När death ändras körs ljudet
		counterD := "000000000000000000"; -- Countern resetas och räknar ej.
		SPD := '0';
		percountD := 0;

	else
		if to_integer(counterD) = clkdividerdmg then -- När räknaren når clkdividerdmg så switchas SPD och countern nollställs.
			if percountD < 50 OR (percountD > 100 AND percountD < 150) then -- Skickar ut två ljudpulser (ljud 0-50, paus 50-100, ljud 100-150)
			SPD := NOT SPD;-- SPD byter tecken vilket i sin tur kommer byta tecken på utsignalen till Port1.
			
			end if;
			counterD := "000000000000000000";
			percountD := percountD + 1;
		else
			counterD := counterD + 1;
		end if; 
	end if;
	SpeakerOutdmg <= SPD; -- SP/SPD matas in på utsignalen
end if;

end process;

Mergesounds:process (Speakeroutshot, Speakeroutdmg)

begin

if Death = '0' then
	Speakerport <= Speakeroutshot XOR Speakeroutdmg;
else 
    Speakerport <= Speakeroutdmg;
end if;

end process;

Display:process (CLK100MHZ,CPU_RESETN)
--Playerselect
variable position: INTEGER;
variable pcount : UNSIGNED (13 downto 0);
variable player : UNSIGNED (3 downto 0);
variable clickU : STD_LOGIC := '1';
variable clickD : STD_LOGIC := '1';
--Death
variable freq: UNSIGNED (26 downto 0) := "101111101011110000100000000";
variable count : UNSIGNED (26 downto 0) := "000000000000000000000000000";
variable count2 : UNSIGNED (13 downto 0) := "00000000000000";
variable k : INTEGER;

begin

if rising_edge (CLK100MHZ) then

	if Death = '0' then
--		if HIT='0' then
--        		dig2 <= "011";
--		elsif HIT='1' then
--			dig2="110";
--		end if;
        dig1 <= "0000";
        count := "000000000000000000000000000";
	k := 1;

        	IR_packet(3 downto 0) <= player; 

		pcount := pcount + 1; -- count räknar upp

		if pcount = "11111111111111" then -- resetar när count når 16383
		pcount := "00000000000000";
		elsif pcount < "01010101010101" then -- position 0 för count: 0-5461 (första siffran/bokstaven lyser)
		position := 0;
		elsif (pcount > "10101010101010" OR pcount = "10101010101010") AND pcount < "11111111111111" then -- position 1 för count: 10922-16383 (andra siffran/bokstaven lyser)
		position := 1;
		else -- position 2 för count: 5462-10921 (tredje siffran/bokstaven lyser)
		position := 2;
		end if; -- count

		if BTNU = '1' then -- när BTNU trycks ned så ökar player med ett.
			if clickU = '1' then
				player := player + 1;
				clickU := '0';
			else NULL;
			end if; --click
		else
			clickU := '1';
		end if; --BTNU
		if BTND = '1' then -- när BTND trycks ned så minskar player med ett.
			if clickD = '1' then
				player := player - 1;
				clickD := '0';
			else NULL;
			end if; --click
		else
			clickD := '1';
		end if; --BTNC


		case position is 
			when 2 => AN <= "01111111";
				seg <= "10001100"; -- P (player)
			when 0 => AN <= "10111111"; 
				case player is
					when "0000" => seg <= "11111001"; -- player 1
					when "0001" => seg <= "10100100"; -- player 2
					when "0010" => seg <= "10110000"; -- player 3
					when "0011" => seg <= "10011001"; -- player 4
					when "0100" => seg <= "10010010"; -- player 5
					when "0101" => seg <= "10000010"; -- player 6
					when "0110" => seg <= "11111000"; -- player 7
					when "0111" => seg <= "10000000"; -- player 8
					when "1000" => seg <= "10010000"; -- player 9
					when "1001" => seg <= "11111001"; -- player 10
					when "1010" => seg <= "11111001"; -- player 11
					when "1011" => seg <= "11111001"; -- player 12
					when "1100" => seg <= "11111001"; -- player 13
					when "1101" => seg <= "11111001"; -- player 14
					when "1110" => seg <= "11111001"; -- player 15
					when "1111" => seg <= "11111001"; -- player 16
					when others => seg <= "11111111"; -- tom
				end case; -- player
			when 1 => AN  <= "11011111";
				case player is
					when "0000" => seg <= "11111111"; -- player 1
					when "0001" => seg <= "11111111"; -- player 2
					when "0010" => seg <= "11111111"; -- player 3
					when "0011" => seg <= "11111111"; -- player 4
					when "0100" => seg <= "11111111"; -- player 5
					when "0101" => seg <= "11111111"; -- player 6
					when "0110" => seg <= "11111111"; -- player 7
					when "0111" => seg <= "11111111"; -- player 8
					when "1000" => seg <= "11111111"; -- player 9
					when "1001" => seg <= "11000000"; -- player 10
					when "1010" => seg <= "11111001"; -- player 11
					when "1011" => seg <= "10100100"; -- player 12
					when "1100" => seg <= "10110000"; -- player 13
					when "1101" => seg <= "10011001"; -- player 14	
					when "1110" => seg <= "10010010"; -- player 15
					when "1111" => seg <= "10000010"; -- player 16
					when others => seg <= "11111111"; -- tom
				end case; -- player
			when others => AN <= "11111111";
				seg <= "11111111";
		end case; -- position
	elsif Death = '1' then
		if HIT = '0' AND k = 1 then
			Dig2 <= "011";
			k := 0;
		elsif HIT = '1' AND k = 1 then
			Dig2 <= "110";
			k:= 0;
		else
			count := count + 1;
			count2 := count2 + 1;

			if count = freq then
				count := "000000000000000000000000000";
				if dig1 = "0000" AND dig2 = "000" then
--				Death <= '0';s
				elsif dig1 = "0000" AND (dig2 = "001" OR dig2 = "010" OR dig2 = "011" OR dig2 = "100" OR dig2 = "101" OR dig2 = "110") then
					dig1 <= "1001";
					dig2 <= dig2 - 1;
				else
					dig1 <= dig1 - 1;
				end if;
			end if;
	
			if count2 = "11111111111111" then -- resetar när count2 når 16383
				count2 := "00000000000000";
			elsif count2 < "00101010101010" OR count2 = "00101010101010" then -- position 0 för count2, första siffran lyser
				position := 0;
			elsif (count2 < "01010101010100" AND count2 > "00101010101010") OR  count2 = "00101010101010" then -- position 1 för count2, andra siffran lyser
				position := 1;
			elsif (count2 < "01111111111110" AND count2 > "01010101010100") OR count2 = "01010101010100" then -- position 2 för count2, tredje siffran lyser
				position := 2;
			elsif (count2 < "10101010101000" AND count2 > "01111111111110") OR count2 = "01111111111110" then -- position 3 för count2, fjärde siffran lyser
				position := 3;
			elsif (count2 < "11010101010010" AND count2 > "10101010101000") OR count2 = "10101010101000" then -- position 4 för count2, femte siffran lyser
				position := 4;
			else    											  -- position 5 för count2, sjätte siffran lyser
            			position := 5;
			end if;

			case position is 
				when 0 => AN <= "11111110"; -- Least significant digit
				case dig1 is
					when "0000" => seg <= "11000000"; -- 0
					when "0001" => seg <= "11111001"; -- 1
					when "0010" => seg <= "10100100"; -- 2
					when "0011" => seg <= "10110000"; -- 3
					when "0100" => seg <= "10011001"; -- 4
					when "0101" => seg <= "10010010"; -- 5
					when "0110" => seg <= "10000010"; -- 6
					when "0111" => seg <= "11111000"; -- 7
					when "1000" => seg <= "10000000"; -- 8	
					when "1001" => seg <= "10010000"; -- 9
					when others => seg <= "11111111"; -- tom
				end case;
				when 1 => AN <= "11111101"; -- Most significant digit
				case dig2 is
					when "000" => seg <= "11000000"; -- 0
					when "001" => seg <= "11111001"; -- 1
					when "010" => seg <= "10100100"; -- 2
					when "011" => seg <= "10110000"; -- 3	
					when "100" => seg <= "10011001"; -- 4
					when "101" => seg <= "10010010"; -- 5
					when "110" => seg <= "10000010"; -- 6
					when others => seg <= "11111111"; -- tom
				end case;
				when 2 => AN <= "01111111";
					seg <= "11000000"; --D
				when 3 => AN <= "10111111"; 
					seg <= "10000110"; --E
				when 4 => AN <= "11011111"; 
					seg <= "10001000"; --A
				when 5 => AN <= "11101111"; 
					seg <= "11000000"; --D
				when others => AN <= "11111111";
					seg <= "11111111";
			end case;
		end if; -- HIT
	end if; --Death
end if; -- klocka

end process;

UART:process(CLK100MHZ)--Överföring av killist till Dator
constant UART_delay : INTEGER := 100000000/9600; --9600Hz baud
variable UART_counter : INTEGER := 0;
variable Send_UART : STD_LOGIC := '0';--Flagga för att styra så vi bara skickar informationen en gång 
variable UART_state : INTEGER := 0; --state machine;
variable UART_bit : STD_LOGIC := '1';--
variable UART_ListIndex : INTEGER := 0;

begin
	UART_port <= UART_bit;
	UART_RXD_OUT <= UART_bit;
	UART_LED<=Send_UART;
	if CPU_RESETN = '0' then
	   UART_counter := 0;
	   UART_state := 0;
       UART_bit := '1';
       UART_ListIndex := 0;
       Send_UART := '0';
	
	
	elsif(rising_edge(CLK100MHZ)) then
	if BTNL = '1' then
		Send_UART := '1';	   
	end if;
	
	if Send_UART = '1' then
	    if BTNL ='0' AND UART_state > 43 then -- if finished with message and BTNL released
	        UART_state := 0;
            UART_bit := '1';
            UART_ListIndex := 0;
            Send_UART := '0';
		elsif UART_counter > UART_delay then 
			UART_counter := 0; --reset counter
			UART_state := UART_state +1;
			UART_clock <= '1'; --debug
		else
		    UART_counter := UART_counter+1;
		    UART_clock <= '0'; --debug
		end if;

		case UART_state is
			-- first number/character
			when 0 => UART_bit := '0'; --start bit
			when 1 => UART_bit := KillList(8*UART_ListIndex +4);
			when 2 => UART_bit := KillList(8*UART_ListIndex +5);
			when 3 => UART_bit := KillList(8*UART_ListIndex +6);
			when 4 => UART_bit := KillList(8*UART_ListIndex +7);
			when 5 => UART_bit := '1'; --ascii siffer-index
			when 6 => UART_bit := '1'; --ascii siffer-index
			when 7 => UART_bit := '0'; --ascii siffer-index
			when 8 => UART_bit := '0';
			when 9 => UART_bit := '1'; --stopbit
			when 10 => UART_bit := '1'; --stopbit
			-- second number/character
			when 11 => UART_bit := '0'; --start bit
			when 12 => UART_bit := KillList(8*UART_ListIndex +0);
			when 13 => UART_bit := KillList(8*UART_ListIndex +1);
			when 14 => UART_bit := KillList(8*UART_ListIndex +2);
			when 15 => UART_bit := KillList(8*UART_ListIndex +3);
			when 16 => UART_bit := '1'; --ascii siffer-index
			when 17 => UART_bit := '1'; --ascii siffer-index
			when 18 => UART_bit := '0'; --ascii siffer-index
			when 19 => UART_bit := '0';
			when 20 => UART_bit := '1'; --stopbit
            when 21 => UART_bit := '1'; --stopbit
			-- new line 
			when 22 => UART_bit := '0'; --start bit
			when 23 => UART_bit := '0'; --0
			when 24 => UART_bit := '1'; --2
			when 25 => UART_bit := '0'; --0
			when 26 => UART_bit := '1'; --8
			when 27 => UART_bit := '0'; --0
			when 28 => UART_bit := '0'; --0
			when 29 => UART_bit := '0'; --0
			when 30 => UART_bit := '0'; --0
			when 31 => UART_bit := '1'; --stopbit
            
            when 32 => UART_bit := '1'; --stopbit
            --Börja om längst till vänster på ny rad i putty
            when 33 => UART_bit := '0'; --start bit
            when 34 => UART_bit := '1'; --1
            when 35 => UART_bit := '0'; --0
            when 36 => UART_bit := '1'; --4
            when 37 => UART_bit := '1'; --8
            when 38 => UART_bit := '0'; --0
            when 39 => UART_bit := '0'; --0
            when 40 => UART_bit := '0'; --0
            when 41 => UART_bit := '0'; --0
            when 42 => UART_bit := '1'; --stopbit
            when 43 => UART_bit := '1'; --stopbit
            
			when others => -- transmission complete, reset to known state;
					if UART_ListIndex < 7 then
						UART_ListIndex := UART_ListIndex +1;
						UART_state := 0;
					else 
					   UART_bit := '1'; --idle
					end if;
		end case;
		
	--if finished then send_UART = '0';
	else -- send_UART = 0
	   UART_bit := '1';
	end if;
  end if;
end process;

end behavioural;