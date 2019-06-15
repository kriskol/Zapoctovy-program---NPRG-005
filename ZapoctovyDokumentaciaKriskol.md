# Dokumentácia zápočtového programu z predmetu NPRG005 - Sudoku solver
 Odovzdaný zápočtový program pozostáva z tejto dokumentácie a súboru **ZapoctovySudokuSolverKriskol**.
 Kde druhý menovaný obsahuje kód odovzdaného programu a pár testovacích dát v podobe dotazov. 
## Fungovanie sudoku solver-a:

Spôsob vypĺňania zadaného sudoku je nasledovný:
1.Ak má sudoku vyplnené všetky polia, tak ho vráť a skonči. Inak pokračuj 2.
2.Skús vyplniť všetky také polia v sudoku, ktoré ešte nemajú vyplnenú hodnotu a existuje práve jedna hodnota, ktorú je možné do daného poľa vložiť bez porušenia pravidiel vypĺňania sudoku. Ak sa vyplní aspoň jedno pole sudoku, tak na konci 2 opakuj 2. Inak pokračuj 3.
3.Nájdi také pole, ktoré je ešte nevyplnené a existuje aspoň jedna hodnota, ktorá je doplniteľná do daného poľa bez porušenia pravidiel. A ešte pre každé iné nevyplnené pole platí, že takéto pole má rovnaký alebo väčší počet možností na jeho doplnenie, tak aby sa neporušili žiadne pravidlá, ktoré plynú zo stavu sudoku. Pokračuj na krok č.4.
4.Ak sa v 3. našlo nejaké pole, tak ho vyplň nejakou z možných hodnôt, 
	inak nič nevypĺňaj. Prejdi na krok č. 1.

Pokiaľ v 2. alebo 3. dôjde k nájdeniu takého poľa, ktoré nie je vyplnené a zároveň neexistuje hodnota, ktorou by ho bolo možné vyplniť bez porušenia pravidiel, tak dôjde k backtracking-u do naposledy
vykonaného kroku č.4 a v ňom sa vyplní pole, ktoré bolo vypĺňané v tomto kroku  inou hodnotou z tých hodnôt, ktorými môže byť vyplnené. Ak taká hodnota neexistuje, boli už vyskúšané všetky možnosti, tak opäť nastane backtracking, teraz do predposledného kroku č.4 a skúsi sa vykonať to isté ako pri prvom backtracking-u. Prípadne program skončí s false, pokiaľ už nebude mať kam backtrack-ovat.

## Poznámky k reprezentácii sudoku:
Sudoku je interne pri hľadaní jeho vyplnenia reprezentované ako zoznam, ktorého prvky sú zoznamy, ktoré reprezentujú riadky sudoku. Prvky zoznamu, ktoré reprezentujú riadok sú opäť zoznamy.
Tieto zoznamy reprezentujú jednotlivé časti riadku, ktoré prislúchajú k jednotlivým štvorčekom v rámci sudoku. Tieto zoznamy sú nazývané v programe ako **partial rows**.

V takejto podobe je možné sudoku i zadať. A v takejto podobe je vždy vyplnené sudoku i vrátené.

Sudoku je možné ešte zadať v podobe, kedy zoznam, ktorý reprezentuje riadok sa už nedelí na ďalšie zoznamy, ale rovno obsahuje čísla, ktorými je vyplnený daný riadok.

Pokiaľ je dané pole sudoku ešte "nevyplnené", tak je vyplnené 0.
## Poznámky k slovám a termom použitým v kóde a jeho komentároch:
`Order` alebo `cell order` je typický číslo, také že existuje práve toľko polí pred práve uvažovaným políčkom ako je hodnota `Order`. Kde pri počítaní políčok začíname vľavo hore a pokračujeme vpravo až po okraj sudoku, potom prejdeme na ďalší riadok. Za každé políčko pripočítame 1.  

Term `position(X,Y,Z)` jednoznačne udáva políčko v sudoku. Kde X
udáva riadok, kde riadky sú indexované "zhora nadol" od 0.
Nech `N`je také číslo, že `N*N` je počet riadkov sudoku.
Tak potom `Y*N + Z`, kde `Z < N` a obe čísla sú celé nezáporné, udáva stĺpec, kde stĺpce sú indexované "zľava  doprava" od 0.

